#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include <cctype>
#include <cstdio>
#include <map>
#include <string>
#include <vector>

using namespace llvm;


enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,

    // control
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
};

static std::string IdentifierStr;
static double NumVal;

static int gettok() {
    static int LastChar = ' ';

    while (isspace(LastChar))
        LastChar = getchar();

    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;

        if (IdentifierStr == "def") return tok_def;
        if (IdentifierStr == "extern") return tok_extern;
        if (IdentifierStr == "if") return tok_if;
        if (IdentifierStr == "then") return tok_then;
        if (IdentifierStr == "else") return tok_else;

        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == '.') {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    if (LastChar == '#') {
        do LastChar = getchar();
        while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF)
            return gettok();
    }

    if (LastChar == EOF)
        return tok_eof;

    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}


namespace {


class ExprAST {
public:
    virtual ~ExprAST() {}
    virtual Value *CodeGen() = 0;
};


class NumberExprAST : public ExprAST {
    double Val;
public:
    NumberExprAST(double val) : Val(val) {}
    virtual Value *CodeGen();
};


class VariableExprAST : public ExprAST {
    std::string Name;
public:
    VariableExprAST(const std::string &name) : Name(name) {}
    virtual Value *CodeGen();
};


class BinaryExprAST : public ExprAST {
    char Op;
    ExprAST *LHS, *RHS;
public:
    BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs)
        : Op(op), LHS(lhs), RHS(rhs) {}
    virtual Value *CodeGen();
};


class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<ExprAST*> Args;
public:
    CallExprAST(const std::string &callee, std::vector<ExprAST*> &args)
        : Callee(callee), Args(args) {}
    virtual Value *CodeGen();
};


class IfExprAST : public ExprAST {
    ExprAST *Cond, *Then, *Else;
public:
    IfExprAST(ExprAST *cond, ExprAST *then, ExprAST *_else)
        : Cond(cond), Then(then), Else(_else) {}
    virtual Value *CodeGen();
};



class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
public:
    PrototypeAST(const std::string &name, const std::vector<std::string> &args)
        : Name(name), Args(args) {}
    Function *CodeGen();
};


class FunctionAST {
    PrototypeAST *Proto;
    ExprAST *Body;
public:
    FunctionAST(PrototypeAST *proto, ExprAST *body)
        : Proto(proto), Body(body) {}
    Function *CodeGen();
};

}


static int CurTok;
static int getNextToken() {
    return CurTok = gettok();
}


ExprAST *Error(const char *Str) { fprintf(stderr, "Error: %s\n", Str); return 0; }
PrototypeAST *ErrorP(const char *Str) { Error(Str); return 0; }
FunctionAST *ErrorF(const char *Str) { Error(Str); return 0; }
Value *ErrorV(const char *Str) { Error(Str); return 0; }


static Module *TheModule;
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, Value*> NamedValues;
static FunctionPassManager *TheFPM;


static ExprAST *ParseExpression();


static ExprAST *ParseNumberExpr() {
    ExprAST *Result = new NumberExprAST(NumVal);
    getNextToken();
    return Result;
}


static ExprAST *ParseParenExpr() {
    getNextToken();
    ExprAST *V = ParseExpression();
    if (!V) return 0;

    if (CurTok != ')')
        return Error("expected ')'");

    getNextToken();
    return V;
}


static ExprAST *ParseIfExpr() {
    getNextToken();
    ExprAST *Cond = ParseExpression();
    if (!Cond) return 0;

    if (CurTok != tok_then)
        return Error("expected then");

    getNextToken();
    
    ExprAST *Then = ParseExpression();
    if (Then == 0) return 0;

    if (CurTok != tok_else)
        return Error("expected else");
    getNextToken();

    ExprAST *Else = ParseExpression();
    if (!Else) return 0;

    return new IfExprAST(Cond, Then, Else);
}




static ExprAST *ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;

    getNextToken();

    if (CurTok != '(')
        return new VariableExprAST(IdName);

    getNextToken();
    std::vector<ExprAST*> Args;
    if (CurTok != ')') {
        while (1) {
            ExprAST *Arg = ParseExpression();
            if (!Arg) return 0;
            Args.push_back(Arg);

            if (CurTok == ')') break;

            if (CurTok !=  ',')
                return Error("Expected ')' or '.' in argument list");

            getNextToken();
        }
    }
    getNextToken();

    return new CallExprAST(IdName, Args);
}


static ExprAST *ParsePrimary() {
    switch (CurTok) {
        default: return Error("Unknown token when expecting an expression");
        case tok_identifier: return ParseIdentifierExpr();
        case tok_number: return ParseNumberExpr();
        case '(': return ParseParenExpr();
        case tok_if: return ParseIfExpr();
    }
}


static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence() {
    if (!isascii(CurTok))
        return -1;

    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0) return -1;
    return TokPrec;
}


static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
    while (1) {
        int TokPrec = GetTokPrecedence();

        if (TokPrec < ExprPrec) {
            return LHS;
        }

        int BinOp = CurTok;
        getNextToken();

        ExprAST *RHS = ParsePrimary();
        if (!RHS) return 0;

        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, RHS);
            if (RHS == 0) return 0;
        }

        LHS = new BinaryExprAST(BinOp, LHS, RHS);
    }
}


static ExprAST *ParseExpression() {
    ExprAST *LHS = ParsePrimary();
    if (!LHS) return 0;

    return ParseBinOpRHS(0, LHS);
}


static PrototypeAST *ParsePrototype() {
    if (CurTok != tok_identifier)
        return ErrorP("Expected function name in prototype");

    std::string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(')
        return ErrorP("Expected '(' in prototype");

    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier)
        ArgNames.push_back(IdentifierStr);

    if (CurTok != ')')
        return ErrorP("Expected ')' in prototype");

    getNextToken();

    return new PrototypeAST(FnName, ArgNames);
}


static FunctionAST *ParseDefinition() {
    getNextToken();

    PrototypeAST *Proto = ParsePrototype();
    if (Proto == 0) return 0;

    if (ExprAST *E = ParseExpression())
        return new FunctionAST(Proto, E);

    return 0;
}

static FunctionAST *ParseTopLevelExpr() {
    if (ExprAST *E = ParseExpression()) {
        PrototypeAST *Proto = new PrototypeAST("", std::vector<std::string>());
        return new FunctionAST(Proto, E);
    }
    return 0;
}

static PrototypeAST *ParseExtern() {
  getNextToken();
  return ParsePrototype();
}

Value *NumberExprAST::CodeGen() {
    return ConstantFP::get(getGlobalContext(), APFloat(Val));
}


Value *VariableExprAST::CodeGen() {
    Value *V = NamedValues[Name];
    return V ? V : ErrorV("Unknown variable name");
}


Value *BinaryExprAST::CodeGen() {
    Value *L = LHS->CodeGen();
    Value *R = RHS->CodeGen();
    if (L == 0 || R == 0) return 0;
    switch (Op) {
        case '+': return Builder.CreateFAdd(L, R, "addtmp");
        case '-': return Builder.CreateFSub(L, R, "subtmp");
        case '*': return Builder.CreateFMul(L, R, "multmp");
        case '<':
            L = Builder.CreateFCmpULT(L, R, "cmptmp");
            return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()), "booltmp");
        default: return ErrorV("Unknown binary operator");
    }
}


Value *CallExprAST::CodeGen() {
    Function *CalleeF = TheModule->getFunction(Callee);
    if (CalleeF == 0) {
        return ErrorV("Unknown function referenced");
    }

    if (CalleeF->arg_size() != Args.size())
        return ErrorV("Incorrect number of arguments");

    std::vector<Value*> ArgsV;
    for (unsigned i=0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->CodeGen());
        if (ArgsV.back() == 0) return 0;
    }

    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}


Value *IfExprAST::CodeGen() {
    Value *CondV = Cond->CodeGen();
    if (CondV == 0) return 0;

    CondV = Builder.CreateFCmpONE(CondV, ConstantFP::get(getGlobalContext(), APFloat(0.0)), "ifcond");

    Function *TheFunction = Builder.GetInsertBlock()->getParent();

    BasicBlock *ThenBB = BasicBlock::Create(getGlobalContext(), "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(getGlobalContext(), "else");
    BasicBlock *MergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");

    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    Builder.SetInsertPoint(ThenBB);
    Value *ThenV = Then->CodeGen();
    if (ThenV == 0) return 0;

    Builder.CreateBr(MergeBB);
    ThenBB = Builder.GetInsertBlock();

    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);
    Value *ElseV = Else->CodeGen();
    if (ElseV == 0) return 0;

    Builder.CreateBr(MergeBB);
    ElseBB = Builder.GetInsertBlock();

    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    PHINode *PN = Builder.CreatePHI(Type::getDoubleTy(getGlobalContext()), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;
}


Function *PrototypeAST::CodeGen() {
    std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(getGlobalContext()));
    FunctionType *FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()), Doubles, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule);
    if (F->getName() != Name) {
        // If this function was previously defined, it will have been renamed as soon as it
        // as created.  So we get rid of the one we just created, and replace it with the old one.
        // This allows multiple prototype definitions.
        F->eraseFromParent();
        F = TheModule->getFunction(Name);

        if (!F->empty()) {
            // ...unless it was actually defined with a body previously.
            ErrorF("redefinition of function");
            return 0;
        }

        if (F->arg_size() != Args.size()) {
            // Or it had a different number of args.
            ErrorF("redefinition of function with a different number of args");
            return 0;
        }
    }
    unsigned Idx = 0;
    for (Function::arg_iterator AI = F->arg_begin(); Idx != Args.size(); ++AI, ++Idx)
    {
        AI->setName(Args[Idx]);
        NamedValues[Args[Idx]] = AI;
    }

    return F;
}


Function *FunctionAST::CodeGen() {
    NamedValues.clear();

    Function *TheFunction = Proto->CodeGen();
    if (TheFunction == 0)
        return 0;

    BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    if (Value *RetVal = Body->CodeGen()) {
        Builder.CreateRet(RetVal);
        verifyFunction(*TheFunction);
        TheFPM->run(*TheFunction);
        return TheFunction;
    }

    TheFunction->eraseFromParent();
    return 0;
}


static ExecutionEngine *TheExecutionEngine;

static void HandleDefinition() {
    if (FunctionAST *F = ParseDefinition()) {
        if (Function *LF = F->CodeGen()) {
            fprintf(stderr, "Parsed a function definition.\n");
            LF->dump();
        }
    } else {
      // Skip token for error recovery.
      getNextToken();
    }
}

static void HandleExtern() {
    if (PrototypeAST *P = ParseExtern()) {
        if (Function *F = P->CodeGen()) {
            fprintf(stderr, "Parsed an extern\n");
            F->dump();
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (FunctionAST *F = ParseTopLevelExpr()) {
    if (Function *LF = F->CodeGen()) {
        LF->dump();

        void *FPtr = TheExecutionEngine->getPointerToFunction(LF);

        double (*FP)() = (double (*)())(intptr_t)FPtr;
        fprintf(stderr, "Evaluated to %f\n", FP());
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}



static void MainLoop() {
    while (1) {
        fprintf(stderr, "ready> ");
        switch (CurTok) {
            case tok_eof:   return;
            case ';':   getNextToken(); break;
            case tok_def:   HandleDefinition(); break;
            case tok_extern: HandleExtern(); break;
            default: HandleTopLevelExpression(); break;
        }
    }
}



int main() {
    InitializeNativeTarget();
    LLVMContext &Context = getGlobalContext();

    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;

    fprintf(stderr, "ready> ");
    getNextToken();

    TheModule = new Module("my cool jit", Context);

    std::string ErrStr;
    TheExecutionEngine = EngineBuilder(TheModule).setErrorStr(&ErrStr).create();
    if (!TheExecutionEngine) {
        fprintf(stderr, "Could not create ExcutionEngine: %s\n", ErrStr.c_str());
        exit(1);
    }

    FunctionPassManager OurFPM(TheModule);
    OurFPM.add(new DataLayout(*TheExecutionEngine->getDataLayout()));
    OurFPM.add(createBasicAliasAnalysisPass());
    OurFPM.add(createInstructionCombiningPass());
    OurFPM.add(createReassociatePass());
    OurFPM.add(createGVNPass());
    OurFPM.add(createCFGSimplificationPass());
    OurFPM.doInitialization();

    TheFPM = &OurFPM;

    MainLoop();

    TheFPM = 0;

    TheModule->dump();

    return 0;
}


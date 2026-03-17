#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<IRBuilder<>> Builder;
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;
enum Token{
  tok_eof = -1,
  tok_def= -2,
  tok_extern = -3,
  tok_identifier = -4,
  tok_number = -5,
};

static std::string identifierStr;
static double NumVal;

static int gettok(){
  static int LastChar = ' ';

  while(isspace(LastChar))
    LastChar = getchar();

  if(isalpha(LastChar)){
    identifierStr = LastChar;
    while(isalnum((LastChar = getchar())))
      identifierStr += LastChar;

    if(identifierStr == "def")
      return tok_def;
    if(identifierStr == "extern")
      return tok_extern;
    return tok_identifier;
  }
  if(isdigit(LastChar) || LastChar == '.'){
    std::string NumbrStr;

  do{
    NumbrStr += LastChar;
    LastChar = getchar();
  }while(isdigit(LastChar) || LastChar == '.');
  NumVal = strtod(NumbrStr.c_str(),0);
  return tok_number;
  }
  
  if(LastChar == '#'){
    do{
      LastChar = getchar();
    }
while(LastChar != EOF && LastChar != '\n' && LastChar != '\r' );
// when the user writes a comment,if its not the EOF, it will not return any token, instead it will call gettok again.
    if(LastChar != EOF)
      return gettok();
    if(LastChar == EOF)
      return tok_eof;
  }
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

class ExprAST{
  public:
    virtual ~ExprAST(){}
    virtual Value *codegen() = 0;
};

class NumberExprAST : public ExprAST{
  double Val;
  public:
    NumberExprAST(double Val) : Val(Val){}
    Value *codegen() override;
};

class VariableExprAST : public ExprAST{
  std::string Name;
  public:
    VariableExprAST(const std::string &Name): Name(Name){}
    Value *codegen() override;
};
class BinaryExprAST : public ExprAST{
  char Op;
  std::unique_ptr<ExprAST> LHS,RHS;
  public:
    BinaryExprAST(char Op,std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)){}
    Value *codegen() override;
};

class CalleeExprAST : public ExprAST{
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
  public:
    CalleeExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args): Callee(Callee), Args(std::move(Args)){}
    Value *codegen() override;
};

class PrototypeExprAST : public ExprAST{
  std::string Name;
  std::vector<std::string> Args;
  public:
    PrototypeExprAST(const std::string &Name, std::vector<std::string> Args): Name(Name), Args(std::move(Args)){}
    Function *codegen() override;
    const std::string &getName() const { return Name; }
};

class FunctionExprAST: public ExprAST{
  std::unique_ptr<PrototypeExprAST> Proto;
  std::unique_ptr<ExprAST> Body;
  public:
    FunctionExprAST(std::unique_ptr<PrototypeExprAST> Proto,std::unique_ptr<ExprAST> Body): Proto(std::move(Proto)), Body(std::move(Body)){}
    Function *codegen() override;
};

static int CurTok;
static int getNextToken(){
  return CurTok = gettok();
}
static std::unique_ptr<ExprAST> ParseExpression();
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ExprAST> LogError(const char *Str){
  fprintf(stderr,"LogError: %s/n", Str);
  return nullptr;
}
std::unique_ptr<PrototypeExprAST> LogErrorP(const char *Str){
  LogError(Str);
  return nullptr;
}
Value *LogErrorV(const char *Str){
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseNumberExpr(){
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken();
  return std::move(Result);
}
static std::unique_ptr<ExprAST> ParseParenExpr(){
  getNextToken();
  auto V = ParseExpression();
  if(!V)
    return nullptr;

  if(CurTok != ')')
    return LogError("Expected ')'");
  getNextToken();
      return V;
}
static std::unique_ptr<ExprAST> ParseIdentifierExpr(){
  std::string idName = identifierStr;

  getNextToken();
  if(CurTok != '(')
    return std::make_unique<VariableExprAST>(idName);
  getNextToken();
  std::vector<std::unique_ptr<ExprAST>> Args;
  if(CurTok != ')'){
    while(true){
      if(auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else 
        return nullptr;

      if(CurTok == ')')
        break;
      if(CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }
  getNextToken();
  return std::make_unique<CalleeExprAST>(idName, std::move(Args));
}

static std::unique_ptr<ExprAST> ParsePrimary(){
  switch(CurTok){
    default:
      return LogError("Unknown token when expecting an expression");
    case tok_identifier:
      return ParseIdentifierExpr();
    case tok_number:
      return ParseNumberExpr();
    case '(':
      return ParseParenExpr();
  }
}

static std::map<char,int> BinOpPrecedence;
static int getTokPrecedence(){
  if(!isascii(CurTok))
    return -1;
  int TokPrec = BinOpPrecedence[CurTok];
  if(TokPrec <= 0)
    return -1;
  return TokPrec;
}

static std::unique_ptr<ExprAST> ParseExpression(){
  auto LHS = ParsePrimary();
  if(!LHS)
    return nullptr;
  return ParseBinOpRHS(0,std::move(LHS));
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS){
  while(true){
    int TokPrec = getTokPrecedence();
    if(TokPrec < ExprPrec)
      return LHS;

    int BinOp = CurTok;
    getNextToken();
    auto RHS = ParsePrimary();
    if(!RHS)
      return nullptr;
    int NextPrec = getTokPrecedence();
    if(TokPrec < NextPrec){
      RHS = ParseBinOpRHS(TokPrec + 1,std::move(RHS));
      if(!RHS)
        return nullptr;
    }
    LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS),std::move(RHS));
  } 
}


static std::unique_ptr<PrototypeExprAST> ParsePrototype(){
  if(CurTok != tok_identifier)
    return LogErrorP("Expected function name");

  std::string FnName = identifierStr;
  getNextToken();
  
  if(CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while(getNextToken() == tok_identifier)
    ArgNames.push_back(identifierStr);
  if(CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  getNextToken();
  return std::make_unique<PrototypeExprAST>(FnName,std::move(ArgNames));
}
static std::unique_ptr<FunctionExprAST> ParseDefinition(){
  getNextToken();
  auto proto = ParsePrototype();
  if(!proto)
    return nullptr;

  if(auto E = ParseExpression())
    return std::make_unique<FunctionExprAST>(std::move(proto),std::move(E));
  return nullptr;
}

static std::unique_ptr<PrototypeExprAST> ParseExtern(){
  getNextToken();
  return ParsePrototype();
}

static std::unique_ptr<FunctionExprAST> ParseTopLevelExpr(){
  if(auto E = ParseExpression()){
    auto proto = std::make_unique<PrototypeExprAST>("anon_expr",std::vector<std::string>());
    return std::make_unique<FunctionExprAST>(std::move(proto),std::move(E));
  }
  return nullptr;
}


Value *NumberExprAST::codegen(){
  return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen(){
  Value *V = NamedValues[Name]; // look this variable up in the function
  if(!V)
    LogErrorV("Unknown variable name");
  return V;
}
Value *BinaryExprAST::codegen(){
  Value *L = LHS -> codegen();
  Value *R = RHS -> codegen();
  if (!L || !R)
    return nullptr;

  switch(Op){
    case '+':
      return Builder -> CreateFAdd(L,R,"addtmp");
    case '-':
      return Builder -> CreateFSub(L,R,"subtmp");
    case '*':
      return Builder -> CreateFMul(L,R,"multmp");
    case '<':
      L = Builder -> CreateFCmpULT(L,R,"cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return Builder -> CreateUIToFP(L,Type::getDoubleTy(*TheContext),"booltmp");
    default:
      return LogErrorV("Invalid binary operator");
  }
}

Value *CalleeExprAST::codegen(){
  Function *CalleeF = TheModule -> getFunction(Callee);
  if(!CalleeF)
    return LogErrorV("Unknown function referenced");
  if(CalleeF -> arg_size() != Args.size())
    return LogErrorV("Expected arguments");

  std::vector<Value *> ArgsV;
  for(unsigned i = 0;unsigned e = Args.size(); i != e, ++i){
    ArgsV.push_back(Args[i] -> codegen());
    if(!ArgsV.back())
      return nullptr;
  }
  return Builder -> CreateCall(CalleeF,ArgsV,"calltmp");
}
Function *PrototypeExprAST::codegen(){
  std::vector<Type*> Doubles(Args.size(),Type::getDoubleTy(*TheContext));
  FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext),Doubles,false);
  Function *F = Function::Create(FT,Function::ExternalLinkage,Name,TheModule.get());

  unsigned Idx = 0;
  for(auto &Arg : F -> args())
    Arg.setName(Args[Idx++]);

  return F;
}

Function *FunctionExprAST::codegen(){
  Function *TheFunction = TheModule -> getFunction(Proto -> getName());
  if(TheFunction)
    TheFunction = Proto -> codegen();
  if(!TheFunction)
    return nullptr;
  if(!TheFunction -> empty())
    return (Function *) LogErrorV("Function cannot be redefined");
  BasicBlock *BB = BasicBlock::Create(*TheContext,"entry",TheFunction);
  Builder -> SetInsertPoint(BB);

  NamedValues.clear();
  for(auto &Arg : TheFunction -> args())
    NamedValues[std::string(Arg.getName())] = &Arg;
  if(Value *RetVal = Body -> codegen()){
    //Finish off the function
    Builder -> CreateRet(RetVal);
    //Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);
    return TheFunction;
  }
  //Error reading body, remove function
  TheFunction -> eraseFromParent();
  return nullptr;
}

static void InitializeModule() {
  // Open a new context and module.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("my cool jit", *TheContext);

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
      FnIR->print(errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read top-level expression:");
      FnIR->print(errs());
      fprintf(stderr, "\n");

      // Remove the anonymous expression.
      FnIR->eraseFromParent();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  // Install standard binary operators.
  // 1 is lowest precedence.
  BinOpPrecedence['<'] = 10;
  BinOpPrecedence['+'] = 20;
  BinOpPrecedence['-'] = 20;
  BinOpPrecedence['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  // Make the module, which holds all the code.
  InitializeModule();

  // Run the main "interpreter loop" now.
  MainLoop();

  // Print out all of the generated code.
  TheModule->print(errs(), nullptr);

  return 0;
}






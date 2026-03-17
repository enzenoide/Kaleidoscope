static int CurTok;
static int getNextToken(){
  return CurTok = gettok();
}
static std::unique_ptr<ExprAST> LogError(std::string *str){
  printf(stderr,"ERROR\n");
  return nullptr;
}
static std::unique_ptr<ExprAST> LogErrorP(std::string *Str){
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseNumberExpr(){
  auto result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken();
  return std::move(result);   
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr(){
  std::string Idname = identifierStr;

  getNextToken();
  if(CurTok != '(')
    return std::make_unique<VariableExprAST>(idName);
  getNextToken();
  std::vector<std::unique_ptr<ExprAST>> Args;
  if(CurTok != ')'){
  while(true){
    Arg = ParseExpression();
    if(Arg)
      Args.push_back(Arg);
    else
      return nullptr;

    if(CurTok == ')')
      break;
    if(CurTok != ',' )
      return LogError("Expected a ,");
    getNextToken();
    }
  }
  getNextToken();
  return std::make_unique<Callexpr>(Idname, Args);
}

static std::unique_ptr<ExprAST> ParseBinOPRHs(int ExpPrec, std::unique_ptr<ExprAST> LHS){
  while(true){
    int TokPrec = gettokprecedence();

    if(TokPrec < ExpPrec)
      return LHS;

    int BinOp = CurTok;
    getNextToken();
    auto RHS = ParsePrimary();
    if(!RHS)
      return nullptr;
    int NextPrec = gettokprecedence();
    if(TokPrec < NextPrec){
      RHS = ParseBinOPRHs(TokPrec + 1,std::move(LHS));
      if(!RHS)
        return nullptr;
    }
    LHS = std::make_unique<BinaryExprast>(BinOp, std::move(LHS), std::move(RHS)); 
  }

}

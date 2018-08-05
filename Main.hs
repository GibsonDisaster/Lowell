module Main where
  import Prelude hiding (words)
  import qualified Data.Map as M
  import Text.Pretty.Simple (pPrint)
  import Text.Parsec (Parsec, ParsecT, modifyState)
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Control.Monad.Identity
  import Types
  import TypeChecker
  import CodeGen

  {-
    TODO
    [X] Custom Data Types
    [] Code Gen To Assembly
    [] Recursion?
    [] Finish Type Checker
    [] Add optional type constraints on functions ("<typeclass>" in between :: and arglist)
  -}

  functionsList :: M.Map String LType
  functionsList = M.fromList [
                              ("print", LUnit),
                              ("input", LString)
                             ]

  initParserState :: ParserState
  initParserState = ParserState {
      decVars = M.empty,
      usedVars = [],
      funcDefs = [],
      funcBodies = [],
      funcReturns = [],
      funcRetTypes = M.empty,
      funcArgTypes = [],
      funcArgVars = [],
      currentFunc = "",
      context = LError,
      currentSection = "",
      ruleNames = [],
      dataStructs = [],
      errors = [],
      shouldError = False
  }

  -- ParserState Functions

  addDecVar :: String -> LType -> ParserState -> ParserState
  addDecVar var t ps = ps { decVars = M.insert var t (decVars ps) }

  addUsedVar :: String -> ParserState -> ParserState
  addUsedVar var ps = ps { usedVars = var:(usedVars ps) } 

  addFuncDef :: LStruct -> ParserState -> ParserState
  addFuncDef fd ps = ps { funcDefs = fd:(funcDefs ps) }

  addFuncBody :: LStruct -> ParserState -> ParserState
  addFuncBody fb ps = ps { funcBodies = fb:(funcBodies ps) }

  addFuncRet :: LStruct -> ParserState -> ParserState
  addFuncRet x ps = ps { funcReturns = (currentFunc ps, x):(funcReturns ps) }

  addError :: String -> ParserState -> ParserState
  addError e ps = ps { errors = e:(errors ps) }

  toggleError :: Bool -> ParserState -> ParserState
  toggleError b ps = ps { shouldError = b }

  changeCurFunc :: String -> ParserState -> ParserState
  changeCurFunc cf ps = ps { currentFunc = cf }

  addFuncArgType :: String -> [LType] -> ParserState -> ParserState
  addFuncArgType func t ps = ps { funcArgTypes = [(func, t)] ++ (funcArgTypes ps) }

  addFuncArgVar :: String -> [LStruct] -> ParserState -> ParserState
  addFuncArgVar func vars ps = ps { funcArgVars = [(func, vars)] ++ (funcArgVars ps) }

  addFuncTypeMap :: M.Map String LType -> ParserState -> ParserState
  addFuncTypeMap m ps = ps { funcRetTypes = M.union m (funcRetTypes ps) }

  changeContext :: LType -> ParserState -> ParserState
  changeContext t ps = ps { context = t }

  changeCurrentSection :: String -> ParserState -> ParserState
  changeCurrentSection s ps = ps { currentSection = s }

  addRule :: String -> ParserState -> ParserState
  addRule r ps = ps { ruleNames = (ruleNames ps) ++ [r] }

  addDataStruct :: String -> ParserState -> ParserState
  addDataStruct s ps = ps { dataStructs = s:(dataStructs ps) }

  -- Parsing Functions

  word :: ParsecT String ParserState Identity String
  word = many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_', '-']))

  words :: ParsecT String ParserState Identity String
  words = many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_', '-', ' ']))

  spaces :: ParsecT String ParserState Identity ()
  spaces = skipMany (char ' ')

  newlines :: ParsecT String ParserState Identity ()
  newlines = skipMany (char '\n')

  parseGeneric :: ParsecT String ParserState Identity String
  parseGeneric = do
    g <- choice (map char ['a'..'z'])
    return [g]

  parseArg :: ParsecT String ParserState Identity LStruct
  parseArg = do
    arg <- word
    optional (string ", ")
    return $ LArg arg

  parseStructArg :: ParsecT String ParserState Identity String
  parseStructArg = do
    s <- string "Struct"
    spaces
    return s

  parseArgT :: ParsecT String ParserState Identity LType
  parseArgT = do
    t <- choice [try (string "Int"), try (string "Float"), try (string "String"), try (string "Char"), try (string "Bool"), try (string "()"), try (string "a"), try parseStructArg, word]
    optional (string ", ")
    case t of
      "Int" -> return LInt
      "Float" -> return LFloat
      "String" -> return LString
      "Bool" -> return LBool
      "Char" -> return LChar
      "()" -> return LUnit
      "Struct" -> return $ LDataStruct "test"
      g -> return $ LGeneric (head g)

  parseType :: ParsecT String ParserState Identity LType
  parseType = do
    t <- choice [string "Int", string "Float", string "String", string "Bool", string "Char", string "()", string "Struct"]
    case t of
      "Int" -> return LInt
      "Float" -> return LFloat
      "String" -> return LString
      "Char" -> return LChar
      "Bool" -> return LBool
      "()" -> return LUnit
      "Struct" -> return $ LDataStruct "test"

  parseListType :: ParsecT String ParserState Identity LType
  parseListType = do
    t <- between (char '[') (char ']') (parseArgT)
    return $ LList t

  parseLiteral :: ParsecT String ParserState Identity LStruct
  parseLiteral = do
    literal <- (try parseFloat) <|> (try parseInt) <|> (try parseString) <|> (try parseUnit) <|> (try parseBool) <|> (try parseChar)
    return $ literal

  parseNumber :: ParsecT String ParserState Identity Int
  parseNumber = do
    num <- many1 digit
    return $ ((read num) :: Int)

  parseInt :: ParsecT String ParserState Identity LStruct
  parseInt = do
    i <- parseNumber
    return $ LLitI i

  parseFloat :: ParsecT String ParserState Identity LStruct
  parseFloat = do
    f <- parseNumber
    char '.'
    l <- parseNumber
    return $ LLitF (read ((show f) ++ "." ++ (show l)) :: Float)

  parseString :: ParsecT String ParserState Identity LStruct
  parseString = do
    s <- between (char '\"') (char '\"') (words)
    return $ LLitS s

  parseChar :: ParsecT String ParserState Identity LStruct
  parseChar = do
    c <- between (char '\'') (char '\'') (anyChar)
    return $ LLitC c

  parseBool :: ParsecT String ParserState Identity LStruct
  parseBool = do
    b <- choice [string "True", string "False"]
    case b of
      "True" -> return $ LLitB True
      "False" -> return $ LLitB False

  parseUnit :: ParsecT String ParserState Identity LStruct
  parseUnit = do
    string "()"
    return $ LLitU ()

  parseList :: ParsecT String ParserState Identity LStruct
  parseList = do
    elements <- between (char '[') (char ']') (sepBy ((try parseVar) <|> (try parseLiteral)) (string ", "))
    return $ LLitL elements

  parseVar :: ParsecT String ParserState Identity LStruct
  parseVar = do
    w <- word
    modifyState (addUsedVar w)
    n <- getState
    let t = case M.lookup w (decVars n) of
              Nothing -> LUnit
              (Just ty) -> ty
    return $ LVarName w t

  parseVar' :: ParsecT String ParserState Identity [LStruct] 
  parseVar' = do
    w <- word
    n <- getState
    let t = case M.lookup w (decVars n) of
              Nothing -> LUnit
              (Just ty) -> ty
    return $ [LVarName w t]

  parseOperator :: ParsecT String ParserState Identity LStruct
  parseOperator = do
    o <- choice [string "+", string "-", string "*", string "/", string "%", string "<", string ">", string "=="]
    return $ LOperator o

  parseExpr :: ParsecT String ParserState Identity LStruct
  parseExpr = do
    spaces
    v1 <- (try parseVar) <|> (try parseLiteral)
    spaces
    op <- parseOperator
    spaces
    v2 <- (try parseExpr) <|> (try parseVar) <|> (try parseLiteral)
    return $ LExpr v1 op v2

  parseForLoop :: ParsecT String ParserState Identity LStruct
  parseForLoop = do
    spaces
    string "for"
    spaces
    string "("
    v <- word
    spaces
    string "in"
    spaces
    ls <- word
    string ")"
    spaces
    string "{\n"
    block <- many parseBlock
    spaces
    string "}"
    newlines
    return $ LFor v ls block

  parseLoop :: ParsecT String ParserState Identity LStruct
  parseLoop = do
    spaces
    string "loop"
    spaces
    char '('
    ex <- (try parseExpr) <|> (try parseLiteral) <|> (try parseVar)
    char ')'
    spaces
    string "{\n"
    block <- many parseBlock
    spaces
    string "}"
    newlines
    return $ LLoop ex block

  parseSingleIf :: ParsecT String ParserState Identity LStruct
  parseSingleIf = do
    spaces
    string "if"
    spaces
    expr <- between (char '(') (char ')') ((try parseExpr) <|> (try parseVar) <|> (try parseLiteral))
    spaces
    string "then"
    spaces
    v1 <- (try parseVar) <|> (try parseLiteral) <|> (try parseExpr)
    spaces
    string "else"
    spaces
    v2 <- (try parseVar) <|> (try parseLiteral) <|> (try parseExpr)
    return $ LSingleLineIf expr v1 v2

  parseMultiIf :: ParsecT String ParserState Identity LStruct
  parseMultiIf = do
    spaces
    string "if"
    spaces
    char '('
    ex <- (try parseExpr) <|> (try parseLiteral) <|> (try parseVar)
    char ')'
    spaces
    string "{\n"
    block <- many parseBlock
    spaces
    string "}"
    newlines
    return $ LIf ex block

  parseFuncCall :: ParsecT String ParserState Identity LStruct
  parseFuncCall = do
    spaces
    func <- word
    spaces
    state <- getState
    let cont = case M.lookup func functionsList of
                 (Just c) -> c
                 Nothing -> (context state)
    args <- between (char '(') (char ')')  (many parseArg)
    return $ LFuncCall func args

  parseLambda :: ParsecT String ParserState Identity LStruct
  parseLambda = do
    spaces
    char '('
    choice [string "\\", string "λ"]
    lambIn <- between (char '(') (char ')')  (many parseArg)
    spaces
    string "->"
    lExpr <- parseExpr
    char ')'
    return $ LLambda lambIn lExpr

  parseReturn :: ParsecT String ParserState Identity LStruct
  parseReturn = do
    spaces
    string "return"
    spaces
    r <- (try parseExpr) <|> (try parseLiteral) <|> (try parseVar)
    string ";\n"
    modifyState (addFuncRet r)
    return $ LReturn r

  parseAssign :: ParsecT String ParserState Identity LStruct
  parseAssign = do
    spaces
    string "let"
    spaces
    varName <- word
    char ':'
    spaces
    varType <- (try parseType) <|> (try parseListType)
    spaces
    string ":="
    spaces
    varVal <- (try parseFuncCall) <|> (try parseLambda) <|> (try parseSingleIf) <|> (try parseExpr) <|> (try parseLiteral) <|> (try parseVar) <|> (try parseList)
    string ";\n"
    modifyState (addDecVar varName varType)
    return $ LAssign varName varType varVal

  parseReassign :: ParsecT String ParserState Identity LStruct
  parseReassign = do
    spaces
    v <- word
    spaces
    string ":="
    spaces
    varVal <- (try parseLambda) <|> (try parseSingleIf) <|> (try parseExpr) <|> (try parseLiteral) <|> (try parseVar) <|> (try parseList)
    string ";\n"
    modifyState (addUsedVar v)
    return $ LReAssign v varVal

  parseComment :: ParsecT String ParserState Identity LStruct
  parseComment = do
    spaces
    comment <- between (char '#') (char '#') words
    newlines
    return $ LComment comment

  -- Should really be name "parseLine"
  parseBlock :: ParsecT String ParserState Identity LStruct
  parseBlock = do
    t <- (try parseForLoop) <|> (try parseLoop) <|> (try parseAssign) <|> (try parseExpr) <|> (try parseFuncCall) <|> (try parseLambda) <|> (try parseReturn) <|> (try parseFuncCall) <|> (try parseReassign) <|> (try parseMultiIf) <|> (try parseComment)
    return t

  -- Pt 1 of a function
  parseFuncDef :: ParsecT String ParserState Identity LStruct
  parseFuncDef = do
    spaces
    funcName <- word
    spaces
    string "::"
    spaces
    argTypes <- between (char '{') (char '}') (many ((try parseArgT) <|> (try parseListType)))
    spaces
    string "=>"
    spaces
    funcReturnT <- parseType
    n <- getState
    let cur = currentSection n
    if cur == "Teach" then modifyState (addRule funcName) else return ()
    if cur /= "Prog" then return () else modifyState (addFuncTypeMap (M.fromList [(funcName, funcReturnT)]))
    if cur /= "Prog" then return () else modifyState (addFuncDef (LFuncDec funcName argTypes funcReturnT))
    if cur /= "Prog" then return () else modifyState (addFuncArgType funcName argTypes)
    string ";\n"
    spaces
    return $ LFuncDec funcName argTypes funcReturnT

  -- Pt 2 of a function
  parseFuncBody :: ParsecT String ParserState Identity LStruct
  parseFuncBody = do
    spaces
    funcName <- word
    modifyState (changeCurFunc funcName)
    funcArgs <- between (char '(') (char ')') (many parseArg)
    spaces
    string "{\n"
    funcBlock <- many parseBlock
    spaces
    string "}"
    newlines
    n <- getState
    let cur = currentSection n
    if cur /= "Prog" then return () else modifyState (addFuncBody (LFuncBody funcName funcArgs funcBlock))
    if cur /= "Prog" then return () else modifyState (addFuncArgVar funcName funcArgs)
    return $ LFuncBody funcName funcArgs funcBlock

  parseFunc :: ParsecT String ParserState Identity LStruct
  parseFunc = do
    funcDefs <- parseFuncDef
    funcBody <- parseFuncBody
    newlines
    spaces
    return $ LFuncFull funcDefs funcBody

  -- Parses entire Prog{} section
  parseProgram :: ParsecT String ParserState Identity LStruct
  parseProgram = do
    spaces
    newlines
    string "Prog"
    spaces
    funcs <- between (string "{\n") (char '}') (many1 parseFunc)
    return $ LProgram (LComment []) funcs

  -- Returns a child of a data type (ie: Child in Person = Child | Adult)
  parseDataChildren :: ParsecT String ParserState Identity LStruct
  parseDataChildren = do
    name <- word
    spaces
    fields <- between (char '[') (char ']') (many parseArgT)
    spaces
    optional (string "or")
    spaces
    return $ LData (name, fields)

  -- Collects all children up into a parent (returns something like "LDataParent Foo (LDataChild (Bar, [Baz]))" )
  parseDataStructs :: ParsecT String ParserState Identity LStruct
  parseDataStructs = do
    spaces
    string "struct ::"
    spaces
    dataName <- word
    modifyState (addDataStruct dataName)
    spaces
    char '='
    spaces
    children <- many1 parseDataChildren
    newlines
    return $ LDataParent dataName children

  -- Parses entire Data{} section
  parseData :: ParsecT String ParserState Identity LStruct
  parseData = do
    spaces
    string "Data"
    spaces
    datas <- between (string "{\n") (char '}') (many parseDataStructs)
    newlines
    return $ LDatas datas

  parseRem :: ParsecT String ParserState Identity LStruct
  parseRem = do
    spaces
    newlines
    string "Remember " 
    remRules <- between (string "{\n") (char '}') (many1 parseFunc)
    return $ LRuleInstance remRules

  parseTeach :: ParsecT String ParserState Identity LStruct
  parseTeach = do
    spaces
    string "Teach"
    spaces
    teachRules <- between (string "{\n") (string "}") (many parseFuncDef)
    return $ LTeach teachRules

  parseRules :: ParsecT String ParserState Identity LStruct
  parseRules = do
    spaces
    string "Rules"
    spaces
    string "{\n"
    newlines
    spaces
    modifyState (changeCurrentSection "Teach")
    teach <- parseTeach
    newlines
    spaces
    modifyState (changeCurrentSection "Remember")
    rem <- parseRem
    newlines
    spaces
    string "}"
    return $ LRules teach rem

  mainParser :: Parsec String ParserState (LStruct, ParserState)
  mainParser = do
    modifyState (changeCurrentSection "Data")
    dat <- parseData
    rules <- parseRules
    modifyState (changeCurrentSection "Prog")
    prog <- parseProgram
    n <- getState
    return (LSections dat rules prog, n)

  getParser :: IO ParserState
  getParser = do
    input <- readFile "main.low"
    let state = case runParser mainParser initParserState "ERROR" input of
                  Left e -> EmptyState
                  Right (s, n) -> n
    return state

  main :: IO ()
  main = do
    input <- readFile "main.low"
    case runParser mainParser initParserState "ERROR" input of
      Left e -> print e
      Right s -> print s -- pPrint s
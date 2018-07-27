module Main where
  import Prelude hiding (words)
  import qualified Data.Map as M
  import Text.Parsec (Parsec, ParsecT, modifyState)
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Control.Monad.Identity
  import Types
  import TypeChecker

  {-
    TODO
    [] Custom Data Types
  -}

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

  addFuncVar :: String -> LStruct -> ParserState -> ParserState
  addFuncVar f v ps = ps { funcVars = insertTuple f v (funcVars ps) }

  addFuncTypeMap :: M.Map String LType -> ParserState -> ParserState
  addFuncTypeMap m ps = ps { funcRetTypes = M.union m (funcRetTypes ps) }

  -- Utility Functions

  -- Insert something into the value of a map at the specified key
  insertTuple :: String -> a -> [(String, [a])] -> [(String, [a])]
  insertTuple str l l'
    | null l' = [(str, [l])] -- maybe just return []?
    | (fst (head l')) == str = [(str, (snd (head l')) ++ [l])]
    | otherwise = [(head l')] ++ insertTuple str l (tail l')

  -- Parsing Functions

  word :: ParsecT String ParserState Identity String
  word = many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_', '-']))

  words :: ParsecT String ParserState Identity String
  words = many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_', '-', ' ']))

  spaces :: ParsecT String ParserState Identity ()
  spaces = skipMany (char ' ')

  newlines :: ParsecT String ParserState Identity ()
  newlines = skipMany (char '\n')

  parseArg :: ParsecT String ParserState Identity LStruct
  parseArg = do
    arg <- word
    optional (string ", ")
    return $ LArg arg

  parseArgT :: ParsecT String ParserState Identity LType
  parseArgT = do
    t <- choice [string "Int", string "Float", string "String", string "Char", string "Bool", string "()"]
    optional (string ", ")
    case t of
      "Int" -> return LInt
      "Float" -> return LFloat
      "String" -> return LString
      "Bool" -> return LBool
      "Char" -> return LChar
      "()" -> return LUnit

  parseType :: ParsecT String ParserState Identity LType
  parseType = do
    t <- choice [string "Int", string "Float", string "String", string "Bool", string "Char", string "()"]
    case t of
      "Int" -> return LInt
      "Float" -> return LFloat
      "String" -> return LString
      "Char" -> return LChar
      "Bool" -> return LBool
      "()" -> return LUnit

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
    args <- between (char '(') (char ')')  (many parseArg)
    --string ";\n"
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

  parseBlock :: ParsecT String ParserState Identity LStruct
  parseBlock = do
    t <- (try parseForLoop) <|> (try parseLoop) <|> (try parseAssign) <|> (try parseExpr) <|> (try parseFuncCall) <|> (try parseLambda) <|> (try parseReturn) <|> (try parseFuncCall) <|> (try parseReassign) <|> (try parseMultiIf)
    return t

  parseFuncDef :: ParsecT String ParserState Identity LStruct
  parseFuncDef = do
    spaces
    funcName <- word
    spaces
    string "::"
    spaces
    funcArgTypes <- between (char '{') (char '}') (many ((try parseArgT) <|> (try parseListType)))
    spaces
    string "=>"
    spaces
    funcReturnT <- parseType
    modifyState (addFuncTypeMap (M.fromList [(funcName, funcReturnT)]))
    string ";\n"
    modifyState (addFuncDef (LFuncDec funcName funcArgTypes funcReturnT))
    return $ LFuncDec funcName funcArgTypes funcReturnT

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
    modifyState (addFuncBody (LFuncBody funcName funcArgs funcBlock))
    return $ LFuncBody funcName funcArgs funcBlock

  parseFunc :: ParsecT String ParserState Identity LStruct
  parseFunc = do
    funcDefs <- parseFuncDef
    funcBody <- parseFuncBody
    return $ LFuncFull funcDefs funcBody

  parseProgram :: ParsecT String ParserState Identity LStruct
  parseProgram = do
    string "Prog "
    funcs <- between (string "{\n") (char '}') (many1 parseFunc)
    return $ LProgram (LComment []) funcs

  mainParser :: Parsec String ParserState (LStruct, ParserState)
  mainParser = do
    prog <- parseProgram
    n <- getState
    return (prog, n)

  main :: IO ()
  main = do
    input <- readFile "main.low"
    case runParser mainParser (ParserState M.empty [] [] [] [] M.empty M.empty [] "" [] False) "ERROR" input of
      Left e -> putStrLn (show e)
      Right s -> putStrLn (show s)
  {-  let r = case runParser mainParser (ParserState M.empty [] [] [] [] M.empty M.empty [] "" [] False) "ERROR" input of
              Left e -> Nothing
              Right (n, s) -> Just $ (n, s)
    putStrLn $ (show (fmap fst r)) ++ "\n-------------------\n" ++ (show (fmap snd r)) -}
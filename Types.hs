module Types where
  import qualified Data.Map as M

  data LType = LInt
             | LFloat
             | LBool
             | LString
             | LChar
             | LUnit
             | LList LType
             deriving (Show, Eq)

  data LStruct = LProgram LStruct [LStruct] -- Comments Main-Block
               | LComment String
               | LBlock [LStruct] -- Each line of the program
               | LFuncFull LStruct LStruct -- Func-Def Func-Body
               | LFuncDec String [LType] LType -- Func-Name Func-Args Func-Return-Type
               | LFuncBody String [LStruct] [LStruct] -- Func-Name Func-Args Func-Body
               | LFuncCall String [LStruct] -- FuncName FuncArgs
               | LVar LType String -- Var-Type Var-Name
               | LVarName String LType
               | LArg String
               | LSingleLineIf LStruct LStruct LStruct -- Expr If-Expr Else-Expr
               | LIf LStruct [LStruct] -- Expr If-Block Else
               | LElse [LStruct] -- Else-Block
               | LFor String String [LStruct] -- For-Var For-List For-Body
               | LLoop LStruct [LStruct] -- Loop-Expr Loop-Body
               | LExpr LStruct LStruct LStruct -- Var-Or-Literal LOperator Var-Or-Literal
               | LOperator String -- +-*/%
               | LLambda [LStruct] LStruct -- Input-Vars Expr
               | LAssign String LType LStruct -- Var-Name Var-Type Var-Value
               | LReAssign String LStruct -- Var-Name Var-Value
               | LReturn LStruct
               | LLitI Int
               | LLitF Float
               | LLitS String
               | LLitC Char
               | LLitB Bool
               | LLitL [LStruct]
               | LLitU () -- Deprecated
               deriving Show

  data ParserState = ParserState {
    decVars :: M.Map String LType, -- Var Var-Type
    usedVars :: [String], -- Var
    funcDefs :: [LStruct],
    funcBodies :: [LStruct],
    funcReturns :: [(String, LStruct)],
    funcRetTypes :: M.Map String LType,
    usedVarMap :: M.Map String LType,
    funcVars :: [(String, [LStruct])],
    currentFunc :: String,
    errors :: [String],
    shouldError :: Bool
  } deriving Show

module Types where
  import qualified Data.Map as M

  data LType = LInt
             | LFloat
             | LBool
             | LString
             | LChar
             | LUnit
             | LList LType
             | LGeneric
             | LError
             deriving (Show, Eq)

  data LStruct = LSections LStruct LStruct LStruct -- Data{} Rules{} Main {}
               | LProgram LStruct [LStruct] -- Comments Main-Block
               | LRules LStruct LStruct -- Teach{} Remember{}
               | LTeach [LStruct] -- everything in Teach{}
               | LGenericRule LStruct -- rule-name type-def
               | LRemember [LStruct] -- everything in Remember{}
               | LRuleInstance [LStruct] -- rule-name type-def func-body
               | LDatas [LStruct] -- All data of a file
               | LDataParent String [LStruct]
               | LData (String, [LType]) -- Data-type pairs of kinds with data associated with it ( ie: LData Person [(Child, []), (Adult, [])] )
               | LComment String -- comment
               | LBlock [LStruct] -- Each line of the program
               | LFuncFull LStruct LStruct -- Func-Def Func-Body
               | LFuncDec String [LType] LType -- Func-Name Func-Args Func-Return-Type
               | LFuncBody String [LStruct] [LStruct] -- Func-Name Func-Args Func-Body
               | LFuncCall String [LStruct] -- FuncName FuncArgs
               | LVar LType String -- Var-Type Var-Name
               | LVarName String LType -- Var-Name type
               | LArg String -- arg name
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
    decVars :: M.Map String LType, -- Vars that have been declared
    usedVars :: [String], -- Vars that have been used in the code
    funcDefs :: [LStruct], -- Function definitions
    funcBodies :: [LStruct], -- Function body definitions
    funcReturns :: [(String, LStruct)], -- Functions with what they actually return
    funcRetTypes :: M.Map String LType, -- Functions with their return type
    funcArgTypes :: [(String, [LType])], -- Functions with list of their arg types
    funcArgVars :: [(String, [LStruct])], -- Functions with list of their args
    currentFunc :: String, -- Current function being parsed
    context :: LType, -- Type that code is currently expecting to return (used for determining if var is being used right)
    currentSection :: String,
    ruleNames :: [String], -- List of typeclasses
    errors :: [String], -- List of errors as strings, accumulated throughout the parsing
    shouldError :: Bool -- True if 1 or more errors in the errors list
  } | EmptyState deriving Show
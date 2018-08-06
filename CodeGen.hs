module CodeGen where
  import Types
  import TypeChecker
  import Data.List

  writeCode :: LStruct -> IO ()
  writeCode lstr = writeFile "lowell-ouput.c" (generateCode lstr)

  generateCodeT :: LType -> String
  generateCodeT lt = case lt of
                       LInt -> "int "
                       LFloat -> "float "
                       LBool -> "int "
                       LString -> "char *"
                       LChar -> "char "
                       LUnit -> "void"
                       (LList t) -> generateCodeT t ++ " *"
                       (LGeneric c) -> ""
                       (LDataStruct s) -> ""
                       _ -> ""

  forEachMacro :: String
  forEachMacro = "#define for_each_item(item, list) \\\nfor(T * item = list->head; item != NULL; item = item->next)"

  getLetter :: Int -> Char
  getLetter n = (head . drop n) ['a'..'z']

  labelArgs :: [String] -> [LType] -> [String]
  labelArgs [] _ = []
  labelArgs _ [] = []
  labelArgs (ar:argStr) as@(a:args) = [ar ++ " " ++ [(getLetter (length as))] ] ++ labelArgs argStr args

  constructFuncArgs :: [LType] -> String
  constructFuncArgs args = concat $ (intersperse ", " argStr')
    where
      argStr = filter (/= ", ") (map generateCodeT args)
      argStr' = labelArgs argStr args

  generateCode :: LStruct -> String
  generateCode (LSections d r p) = generateCode d ++ generateCode r ++ generateCode p
  generateCode (LProgram _ bl) = concat $ map generateCode bl
  generateCode (LDatas ds) = concat $ map generateCode ds
  generateCode (LDataParent n children) = concat $ map generateCode children
  generateCode (LData (n, fields)) = "struct " ++ n ++ " {\n" ++ (concat $ map generateCodeT fields) ++ "}"
  generateCode (LRules t r) = generateCode t ++ generateCode r
  generateCode (LTeach outlines) = concat $ map generateCode outlines
  generateCode (LRemember decls) = concat $ map generateCode decls
  generateCode (LRuleInstance insts) = concat $ map generateCode insts
  generateCode (LComment c) = "// " ++ c
  generateCode (LBlock ls) = concat $ map generateCode ls
  generateCode (LFuncFull def body) = generateCode def ++ generateCode body
  generateCode (LFuncDec name args ret) = generateCodeT ret ++ " " ++ name ++ "(" ++ (constructFuncArgs args) ++ ") {\n"
  generateCode (LFuncBody _ _ blocks) = concat $ map generateCode blocks
  generateCode (LFuncCall name args) = name ++ "(" ++ (concat $ map generateCode args)
  generateCode (LVar _ name) = name
  generateCode (LVarName name _) = name
  generateCode (LArg name) = name
  generateCode (LSingleLineIf expr i e) = "if (" ++ generateCode expr ++ ") {\n" ++ generateCode i ++ "} else {\n" ++ generateCode e ++ "}"
  generateCode (LIf e i) = "if (" ++ generateCode e ++ ") {\n" ++ (concat $ map generateCode i)
  generateCode (LElse block) = concat $ map generateCode block
  generateCode (LFor forV forL body) = "for_each_time(" ++ forV ++ forL ++ ") {\n" ++ (concat $ map generateCode body)
  generateCode (LLoop e body) = "while (" ++ generateCode e ++ ") {\n" ++ (concat $ map generateCode body)
  generateCode (LExpr vl op vl2) = generateCode vl ++ " " ++ generateCode op ++ " " ++ generateCode vl2
  generateCode (LOperator o) = o
  generateCode (LLambda _ _) = ""
  generateCode (LAssign n t val) = generateCodeT t ++ " " ++ n ++ " = " ++ generateCode val ++ ";\n"
  generateCode (LReAssign n val) = n ++ " = " ++ generateCode val
  generateCode (LReturn s) = generateCode s
  generateCode (LLitI i) = show i
  generateCode (LLitF f) = show f
  generateCode (LLitS s) = s
  generateCode (LLitC c) = [c]
  generateCode (LLitB True) = "1"
  generateCode (LLitB False) = "0"
  generateCode (LLitL ls) = (show (getLType (head ls))) ++ " *"
  generateCode (LDataStructL n d) = "struct " ++ n
  generateCode _ = undefined
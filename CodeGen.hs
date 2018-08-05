module CodeGen (generateCode) where
  import Types

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


  generateCode :: LStruct -> String
  generateCode (LSections d r p) = generateCode d ++ generateCode r ++ generateCode p
  generateCode (LDatas ds) = concat $ map generateCode ds
  generateCode (LDataParent n children) = concat $ map generateCode children
  generateCode (LData (n, fields)) = "struct " ++ n ++ " {\n" ++ (concat $ map generateCodeT fields) ++ "}"
  generateCode _ = undefined
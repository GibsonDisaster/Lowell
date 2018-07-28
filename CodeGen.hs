module CodeGen where
  import Types

  -- creates asm code as string depending on struct it is given
  translate :: LStruct -> String
  translate s = ""

  -- Map over each struct and create asm representation of it
  generate :: LStruct -> Either String (IO ())
  generate (LProgram comments body) = Left $ concat (map translate ([comments] ++ body))
  generate _ = Right $ putStrLn "ERROR"
module CodeGen (generateCode) where
  import Types

  writeCode :: LStruct -> IO ()
  writeCode lstr = writeFile "lowell-ouput.c" (generateCode lstr)

  generateCode :: LStruct -> String
  generateCode = undefined
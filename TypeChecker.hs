module TypeChecker where
  import Types
  import qualified Data.Map as M

  {- TO IMPLEMENT -}
  {-
    [] All used vars types match up to dec type?
    [] List contains only one type
    [] No Orphans
      [] func defs
      [] func bodies-+-
  -}

  getReturnVal :: LStruct -> (String, LType)
  getReturnVal (LFuncDec n _ t) = (n, t)

  -- find current function using current func field, pass that functions list of types, and then pass its args 
  linkArgTypes :: (String, [LType]) -> (String, [LStruct]) -> Maybe (String, [(LStruct, LType)])
  linkArgTypes (n, types) (m, args)
    | n /= m || length types /= length args = Nothing
    | otherwise = Just (n, zip args types)

  getLType :: LStruct -> LType
  getLType (LLitI _) = LInt
  getLType (LLitF _) = LFloat
  getLType (LLitS _) = LString
  getLType (LLitC _) = LChar
  getLType (LLitB _) = LBool
  getLType (LLitL (x:xs)) = getLType x -- Possibly change to be a LList of getLType x
  getLType (LExpr v1 op v2) = LInt
  getLType (LVarName _ t) = t
  getLType (LVar t n) = t

  passAllChecks :: ParserState -> Bool
  passAllChecks ps = foldr (&&) True allFuncRets
    where
      allFuncRets = allFuncsMatchReturn (funcDefs ps) (funcReturns ps)

  -- Determine if all functions return their intended type
  allFuncsMatchReturn :: [LStruct] -> [(String, LStruct)] -> [Bool]
  allFuncsMatchReturn fDefs' fRets
    | null fDefs || null fRets || length fDefs /= length fRets = []
    | otherwise = [namesM && typesM] ++ allFuncsMatchReturn (tail fDefs') (tail fRets)
      where
        fDefs = map getReturnVal fDefs'
        namesM = (fst (head fDefs)) == (fst (head fRets))
        typesM = (snd (head fDefs)) == (getLType (snd (head fRets)))

  allVarsMatchDec :: M.Map String LType -> M.Map String LType -> [Bool]
  allVarsMatchDec varsDec varsUsed
    | null varsDec || null varsUsed || length varsDec /= length varsUsed = []
    | otherwise = []
      where
        varsUsed' = M.toList varsUsed
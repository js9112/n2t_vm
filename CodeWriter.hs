module CodeWriter where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class(lift)
import Data.Set hiding (map)
import Data.List(intercalate)
import qualified Parser as P
import Grammar


data CWState = CWState
  { allLabels :: Set String
  , allReferences :: Set String
  , currentFunction :: String
  , commandCounter :: Int
  , fileName :: String
  }

type CWError = String
type CodeWriter a = StateT CWState (Either CWError) a

failWriter :: CWError -> CodeWriter a
failWriter = lift . Left

modifyAllLabels :: (Set String -> Set String) -> CodeWriter ()
modifyAllLabels f = modify $ \as -> as { allLabels = f $ allLabels as }

modifyAllReferences :: (Set String -> Set String) -> CodeWriter ()
modifyAllReferences f = modify $ \as -> as { allReferences = f $ allReferences as }

modifyCommandCounter :: (Int -> Int) -> CodeWriter ()
modifyCommandCounter f = modify $ \as -> as { commandCounter = f $ commandCounter as }

modifyCurrentFunction :: (String -> String) -> CodeWriter ()
modifyCurrentFunction f = modify $ \as -> as { currentFunction = f $ currentFunction as}

buildFile :: [P.Command] -> String -> Either CWError String
buildFile file filename = result
  where
    result = evalStateT built (initialState filename)
    built = do
      ls <- mapM buildCommand file
      labels <- gets allLabels
      refs <- gets allReferences
      if isSubsetOf refs labels
        then
          return $ (unlines (fmap show (concat ls)))
        else
          failWriter ("Label(s) " ++
                      intercalate ", " (toList (difference refs labels))
                      ++ " not defined but used in flow.")

initialState filename = CWState
  { allLabels = empty
  , allReferences = empty
  , currentFunction = ""
  , commandCounter = 0
  , fileName = filename
  }

buildCommand :: P.Command -> CodeWriter [Line]
buildCommand cmd = do
  modifyCommandCounter (+1)
  case cmd of
    P.CArith x -> buildArith x
    P.CPush x y -> buildPush x y
    P.CPop x y -> buildPop x y
    P.CFlow x -> buildFlow x
    P.CFctFlow P.FReturn -> buildReturn
    P.CFctFlow (P.FCall x y) -> buildFCall x y
    P.CFctFlow (P.FDef x y) -> buildFDef x y

-- Arithmetic operations

buildArith P.Add = return $ get2Args ++
          [CIn (CAss (Ass (Single M) (Add (Register M) (Register D))))]

buildArith P.Sub = return $ get2Args ++
          [CIn (CAss (Ass (Single M) (Minus (Register M) (Register D))))]

buildArith P.And = return $ get2Args ++
          [CIn (CAss (Ass (Single M) (And (Register M) (Register D))))]

buildArith P.Or = return $ get2Args ++
          [CIn (CAss (Ass (Single M) (Or (Register M) (Register D))))]

buildArith P.Neg = return $ getArg ++ [CIn (CAss (Ass (Single M) (Negate (Register M))))] -- M=-M
buildArith P.Not = return $ getArg ++ [CIn (CAss (Ass (Single M) (Not (Register M))))] -- M=!M

buildArith P.Eq = buildCompare JEQ
buildArith P.Gt = buildCompare JGT
buildArith P.Lt = buildCompare JLT


buildCompare x = do
        i <- gets commandCounter
        f <- gets fileName
        return $ get2Args ++
          [ CIn (CAss (Ass (Single D) (Minus (Register M) (Register D))))
          , AIn (AtSymbol (UDefSymbol (f++"_TRUE"++ show i)))  -- @TRUE
          , CIn (JExpr (C (Register D)) x)      -- D; comparaison
          , CIn (CAss (Ass (Single D) Zero))      -- D=0
          , AIn (AtSymbol (UDefSymbol (f++"_CONTINUE"++ show i))) -- @CONTINUE
          , CIn (JExpr Zero JMP)              -- 0; JMP
          , LIn (f++"_TRUE"++ show i)          -- (TRUE)
          , CIn (CAss (Ass (Single D) (Negate One))) -- D=-1
          , LIn (f++"_CONTINUE"++ show i)       -- (CONTINUE)
          ]
          ++ getArg
          ++ [CIn (CAss (Ass (Single M) (C (Register D))))] -- M=D

getArg :: [Line]
getArg = [ AIn (AtSymbol (PP SP))  -- @SP
         , CIn (CAss (Ass (Single A) (Minus (Register M) One))) -- A=M-1
         ]

get2Args :: [Line]
get2Args = getArg ++
  [ CIn (CAss (Ass (Single D) (C (Register M)))) -- D=M
  , AIn (AtSymbol (PP SP))                      -- @SP
  , CIn (CAss (Ass (Single M) (Minus (Register M) One))) -- M=M-1
  , CIn (CAss (Ass (Single A) (Minus (Register M) One))) -- A=M-1
  ]

-- Pushing to stack

buildPush :: P.Segment -> Int -> CodeWriter [Line]
buildPush P.Constant x =
  return $ [ AIn (AtInt x)                 -- @x
           , CIn (CAss (Ass (Single D) (C (Register A)))) -- D=A
           ]
           ++ pushDToStack

buildPush P.Static x = do
  fn <- gets fileName
  return $ [ AIn (AtSymbol (UDefSymbol (fn ++ "." ++ show x)))
          , CIn (CAss (Ass (Single D) (C (Register M))))
          ] ++ pushDToStack

buildPush P.Pointer 0 = return $ [ AIn (AtSymbol (PP THIS))
                          , CIn (CAss (Ass (Single D) (C (Register M))))
                          ] ++ pushDToStack

buildPush P.Pointer 1 = return $ [ AIn (AtSymbol (PP THAT))
                          , CIn (CAss (Ass (Single D) (C (Register M))))
                          ] ++ pushDToStack

buildPush P.Temp x = return $ buildPushSimple (VR (R 5)) x A
buildPush P.Argument x = return $ buildPushSimple (PP ARG) x M
buildPush P.Local x = return $ buildPushSimple (PP LCL) x M
buildPush P.This x = return $ buildPushSimple (PP THIS) x M
buildPush P.That x = return $ buildPushSimple (PP THAT) x M

buildPushSimple :: Symbol -> Int -> Reg -> [Line]
buildPushSimple x y z = putTargetIn x y A z ++
                      [ CIn (CAss (Ass (Single D) (C (Register M))))] ++
                      pushDToStack


pushDToStack :: [Line]
pushDToStack = [ AIn (AtSymbol (PP SP))                             -- @SP
               , CIn (CAss (Ass (Single A) (C (Register M))))        -- A=M
               , CIn (CAss (Ass (Single M) (C (Register D))))        -- M=D
               , AIn (AtSymbol (PP SP))                             -- @SP
               , CIn (CAss (Ass (Single M) (Add (Register M) One))) -- M=M+1
               ]

-- Poping from stack
buildPop :: P.Segment -> Int -> CodeWriter [Line]
buildPop P.Constant _ = undefined
buildPop P.Static y = do
  fn <- gets fileName
  return $ popFromStackToD ++
         [ AIn (AtSymbol (UDefSymbol (fn ++ "." ++ show y)))
         , CIn (CAss (Ass (Single M) (C (Register D))))
         ]

buildPop P.Pointer 0 = return $ popFromStackToD ++
                       [ AIn (AtSymbol (PP THIS))
                       , CIn (CAss (Ass (Single M) (C (Register D))))
                       ]

buildPop P.Pointer 1 = return $ popFromStackToD ++
                       [ AIn (AtSymbol (PP THAT))
                       , CIn (CAss (Ass (Single M) (C (Register D))))
                       ]

buildPop P.Temp y = return $ buildPopSimple (VR (R 5)) y A
buildPop P.Argument y = return $ buildPopSimple (PP ARG) y M
buildPop P.Local y = return $ buildPopSimple (PP LCL) y M
buildPop P.This y = return $ buildPopSimple (PP THIS) y M
buildPop P.That y = return $ buildPopSimple (PP THAT) y M

buildPopSimple :: Symbol -> Int -> Reg -> [Line]
buildPopSimple x y z = putTargetIn x y D z ++
                  [ AIn (AtSymbol (VR (R 13)))
                  , CIn (CAss (Ass (Single M) (C (Register D))))
                  ] ++
                  popFromStackToD ++
                  [ AIn (AtSymbol (VR (R 13)))
                  , CIn (CAss (Ass (Single A) (C (Register M))))
                  , CIn (CAss (Ass (Single M) (C (Register D))))
                  ]
putTargetIn :: Symbol -> Int -> Reg -> Reg -> [Line]
putTargetIn x y z a = [ AIn (AtSymbol x)
                      , CIn (CAss (Ass (Single D) (C (Register a))))
                      , AIn (AtInt y)
                      , CIn (CAss (Ass (Single z) (Add (Register D) (Register A))))
                      ]
popFromStackToD :: [Line]
popFromStackToD = [ AIn (AtSymbol (PP SP))
                  , CIn (CAss (Ass (Single M) (Minus (Register M) One)))
                  , CIn (CAss (Ass (Single A) (C (Register M))))
                  , CIn (CAss (Ass (Single D) (C (Register M))))
                  ]

-- Flow building
buildFlow :: P.Flow -> CodeWriter [Line]
buildFlow (P.Label x) = do
  ls <- gets allLabels
  fn <- gets currentFunction
  let l = fn++"$"++x
  if (member x ls)
    then failWriter ("Label " ++ l ++ " used more than once")
    else do
      modifyAllLabels (insert l)
      return [LIn l]

buildFlow (P.GoTo x) = do
  fn <- gets currentFunction
  let l = fn++"$"++x
  modifyAllReferences (insert l)
  return [AIn (AtSymbol (UDefSymbol l))
          , CIn (JExpr Zero JMP)]

buildFlow (P.IfGoTo x) = do
  fn <- gets currentFunction
  let l = fn++"$"++x
  modifyAllReferences (insert l)
  return $ popFromStackToD ++
           [ AIn (AtSymbol (UDefSymbol l))
           , CIn (JExpr (C (Register D)) JNE)]

-- Function flow
buildReturn :: CodeWriter [Line]
buildReturn = return $
  (assignValueToD (PP LCL)) ++   --FRAME=LCL
  (assignDToValue (VR (R 13)))++
  (assignValueToD (VR (R 13)))++  --RET=*(FRAME-5)
  (subtractFromData 5) ++
  [ CIn (CAss (Ass (Single A) (C (Register D))))
  , CIn (CAss (Ass (Single D) (C (Register M))))]++
  (assignDToValue (VR (R 14)))++
  popFromStackToD ++  -- *ARG =pop()
  (assignDToPointer (PP ARG)) ++
  (assignValueToD (PP ARG)) ++   -- SP = ARG+1
  [ CIn (CAss (Ass (Single D) (Add (Register D) One)))] ++
  (assignDToValue (PP SP)) ++
  (assignValueToD (VR (R 13))) ++   -- THAT=*(FRAME-1)
  [ CIn (CAss (Ass (Single D) (Minus (Register D) One)))
  , CIn (CAss (Ass (Single A) (C (Register D))))
  , CIn (CAss (Ass (Single D) (C (Register M))))]++
  (assignDToValue (PP THAT)) ++
  (assignValueToD (VR (R 13))) ++   -- THIS=*(FRAME-2)
  (subtractFromData 2) ++
  [ CIn (CAss (Ass (Single A) (C (Register D))))
  , CIn (CAss (Ass (Single D) (C (Register M))))]++
  (assignDToValue (PP THIS)) ++
  (assignValueToD (VR (R 13))) ++   -- ARG=*(FRAME-3)
  (subtractFromData 3) ++
  [ CIn (CAss (Ass (Single A) (C (Register D))))
  , CIn (CAss (Ass (Single D) (C (Register M))))]++
  (assignDToValue (PP ARG)) ++
  (assignValueToD (VR (R 13))) ++   -- LCL=*(FRAME-4)
  (subtractFromData 4) ++
  [ CIn (CAss (Ass (Single A) (C (Register D))))
  , CIn (CAss (Ass (Single D) (C (Register M))))]++
  (assignDToValue (PP LCL)) ++
  [ AIn (AtSymbol (VR (R 14)) )             -- goto RET
  , CIn (CAss (Ass (Single A) (C (Register M))))
  , CIn (JExpr Zero JMP)]


buildFCall :: String -> Int -> CodeWriter [Line]
buildFCall f n = do
  i <- gets commandCounter
  let returnAddr = "RETURN_ADDRESS" ++ show i
  return $ (atSymbolAndPush (UDefSymbol returnAddr)) ++ --push retur-address
          (atSymbolAndPush (PP LCL)) ++  --push LCL
          (atSymbolAndPush (PP ARG)) ++  --push ARG
          (atSymbolAndPush (PP THIS)) ++  --push THIS
          (atSymbolAndPush (PP THAT)) ++  --push THAT
          (assignValueToD (PP SP)) ++     --ARG=SP-5
          (subtractFromData n) ++
          (subtractFromData 5) ++
          (assignDToValue (PP ARG)) ++
          (assignValueToD (PP SP)) ++     -- LCL=SP
          (assignDToValue (PP LCL)) ++
          [ AIn (AtSymbol (UDefSymbol f)) --goto f
          , CIn (JExpr Zero JMP)
          , LIn returnAddr]         -- (return-address)

buildFDef :: String -> Int -> CodeWriter [Line]
buildFDef f k = do
  ls <- gets allLabels
  modifyCurrentFunction (\ _ ->f)
  if (member f ls)
    then failWriter ("Function name " ++ f ++ " used more than once")
  else do
    modifyAllLabels (insert f)
    return (createLabel ++ push0yTimes)
    where
      createLabel = [LIn f]
      push0yTimes = concat $ replicate k $ ([AIn (AtInt 0),
                    CIn (CAss (Ass (Single D) (C (Register A))))]
                    ++ pushDToStack)

atSymbolAndPush (UDefSymbol s) = [AIn (AtSymbol (UDefSymbol s)),
                    CIn (CAss (Ass (Single D) (C (Register A))))]
                    ++ pushDToStack

atSymbolAndPush (PP s) = [AIn (AtSymbol (PP s)),
                    CIn (CAss (Ass (Single D) (C (Register M))))]
                    ++ pushDToStack

-- D=D-Number
subtractFromData x = [ AIn (AtInt x)
                     , CIn (CAss (Ass (Single D) (Minus (Register D) (Register A))))]
-- D=D+Number
addToData x = [ AIn (AtInt x)
              , CIn (CAss (Ass (Single D) (Add (Register D) (Register A))))]
-- Symbol=D
assignDToValue x = [ AIn (AtSymbol x)
                     , CIn (CAss (Ass (Single M) (C (Register D))))]
-- *Symbol=D
assignDToPointer x = [ AIn (AtSymbol x)
                     , CIn (CAss (Ass (Single A) (C (Register M))))
                     , CIn (CAss (Ass (Single M) (C (Register D))))]
-- D=*Symbol
assignPointerToD x = [ AIn (AtSymbol x)
                    , CIn (CAss (Ass (Single A) (C (Register M))))
                    , CIn (CAss (Ass (Single D) (C (Register M))))]
-- D==Symbol
assignValueToD x = [ AIn (AtSymbol x)
                , CIn (CAss (Ass (Single D) (C (Register M))))]

  -- Bootstrap
buildBootstrap = do
  get_call <- buildFCall "Sys.init" 0
  let boot = [ AIn (AtInt 256) -- SP=256
            , CIn (CAss (Ass (Single D) (C (Register A))))] ++
            assignDToValue (PP SP) ++ -- call Sys.init
            get_call
  return $ (unlines (map show boot))

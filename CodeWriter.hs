module CodeWriter where

import Text.Parsec(parse)
import qualified Parser as P
import Grammar



buildFile :: [P.Command] -> String -> String
buildFile file filename = unlines (fmap show (concat (zipWith (buildCommand filename) file [0..] )))

buildCommand :: String -> P.Command -> Int  -> [Line]
buildCommand _ (P.CArith x) i = buildArith x i
buildCommand fn (P.CPush x y) _ = buildPush x y fn
buildCommand fn (P.CPop x y) _ = buildPop x y fn

-- Arithmetic operations

buildArith P.Add _ = get2Args ++
          [CIn (CAss (Ass (Single M) (Add (Register M) (Register D))))]

buildArith P.Sub _ = get2Args ++
          [CIn (CAss (Ass (Single M) (Minus (Register M) (Register D))))]

buildArith P.And _ = get2Args ++
          [CIn (CAss (Ass (Single M) (And (Register M) (Register D))))]

buildArith P.Or _ = get2Args ++
          [CIn (CAss (Ass (Single M) (Or (Register M) (Register D))))]

buildArith P.Neg _ = getArg ++ [CIn (CAss (Ass (Single M) (Negate (Register M))))] -- M=-M
buildArith P.Not _ = getArg ++ [CIn (CAss (Ass (Single M) (Not (Register M))))] -- M=!M

buildArith P.Eq i = buildCompare JEQ i
buildArith P.Gt i = buildCompare JGT i
buildArith P.Lt i = buildCompare JLT i

buildCompare x i = (buildArith P.Sub i) ++
        [ AIn (AtSymbol (UDefSymbol ("TRUE"++ show i)))  -- @TRUE
        , CIn (JExpr (C (Register D)) x)      -- D; comparaison
        , CIn (CAss (Ass (Single D) Zero))      -- D=0
        , AIn (AtSymbol (UDefSymbol ("CONTINUE"++ show i))) -- @CONTINUE
        , CIn (JExpr Zero JMP)              -- 0; JMP
        , LIn ("TRUE"++ show i)          -- (TRUE)
        , CIn (CAss (Ass (Single D) (Negate One))) -- D=-1
        , LIn ("CONTINUE"++ show i)       -- (CONTINUE)
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
  , CIn (CAss (Ass (Double A M) (Minus (Register M) One))) -- AM=M-1
  ]

-- Pushing to stack

buildPush :: P.Segment -> Int -> String -> [Line]
buildPush P.Constant x _ =
  [ AIn (AtInt x)                 -- @x
  , CIn (CAss (Ass (Single D) (C (Register A)))) -- D=A
  ]
  ++ pushDToStack

buildPush P.Static x fn = [ AIn (AtSymbol (UDefSymbol (fn ++ "." ++ show x)))
                         , CIn (CAss (Ass (Single D) (C (Register M))))
                         ] ++ pushDToStack

buildPush P.Pointer 0 _ = [ AIn (AtSymbol (PP THIS))
                          , CIn (CAss (Ass (Single D) (C (Register M))))
                          ] ++ pushDToStack

buildPush P.Pointer 1 _ = [ AIn (AtSymbol (PP THAT))
                          , CIn (CAss (Ass (Single D) (C (Register M))))
                          ] ++ pushDToStack

buildPush P.Temp x _ = buildPushSimple (VR (R 5)) x A
buildPush P.Argument x _ = buildPushSimple (PP ARG) x M
buildPush P.Local x _ = buildPushSimple (PP LCL) x M
buildPush P.This x _ = buildPushSimple (PP THIS) x M
buildPush P.That x _ = buildPushSimple (PP THAT) x M

buildPushSimple :: Symbol -> Int -> Reg -> [Line]
buildPushSimple x y z = putTargetIn x y A z ++
                      [ CIn (CAss (Ass (Single D) (C (Register z))))] ++
                      pushDToStack


pushDToStack :: [Line]
pushDToStack = [ AIn (AtSymbol (PP SP))                             -- @SP
               , CIn (CAss (Ass (Single A) (C (Register M))))        -- A=M
               , CIn (CAss (Ass (Single M) (C (Register D))))        -- M=D
               , AIn (AtSymbol (PP SP))                             -- @SP
               , CIn (CAss (Ass (Single M) (Add (Register M) One))) -- M=M+1
               ]

-- Poping from stack
buildPop :: P.Segment -> Int -> String -> [Line]
buildPop P.Constant _ _ = undefined
buildPop P.Static y z = popFromStackToD ++
                       [ AIn (AtSymbol (UDefSymbol (z ++ "." ++ show y)))
                       , CIn (CAss (Ass (Single M) (C (Register D))))
                       ]
buildPop P.Pointer 0 _ = popFromStackToD ++
                       [ AIn (AtSymbol (PP THIS))
                       , CIn (CAss (Ass (Single M) (C (Register D))))
                       ]

buildPop P.Pointer 1 _ = popFromStackToD ++
                       [ AIn (AtSymbol (PP THAT))
                       , CIn (CAss (Ass (Single M) (C (Register D))))
                       ]

buildPop P.Temp y _ = buildPopSimple (VR (R 5)) y A
buildPop P.Argument y _ = buildPopSimple (PP ARG) y M
buildPop P.Local y _ = buildPopSimple (PP LCL) y M
buildPop P.This y _ = buildPopSimple (PP THIS) y M
buildPop P.That y _ = buildPopSimple (PP THAT) y M

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
                  , CIn (CAss (Ass (Double M A) (Minus (Register M) One)))
                  , CIn (CAss (Ass (Single D) (C (Register M))))
                  ]

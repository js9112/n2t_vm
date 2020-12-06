module Builder where

import Text.Parsec(parse)
import qualified Parser as P
import Grammar


buildFile file = do
  let x = parse P.parseFile "" file
  case x of
    Left err -> print err
    Right ls ->
      print $ concat (zipWith buildCommand ls [0..])
      --print $ unlines (concatMap (\l -> show <$> buildCommand l 0) ls)

buildCommand :: P.Command -> Int -> [Line]
buildCommand (P.CArith x) i = buildArith x i
buildCommand (P.CPush x y) _ = buildPush x y
buildCommand (P.CPop x y) _ = buildPop x y

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

buildPush :: P.Segment -> Int -> [Line]
buildPush P.Constant x =
  [ AIn (AtInt x)                 -- @x
  , CIn (CAss (Ass (Single D) (C (Register A)))) -- D=A
  ]
  ++ pushDToStack

buildPush P.Static y = undefined
buildPush P.Pointer 0 = [ AIn (AtSymbol (PP THIS))
                        , CIn (CAss (Ass (Single D) (C (Register M))))
                        ] ++ pushDToStack

buildPush P.Pointer 1 = [ AIn (AtSymbol (PP THAT))
                        , CIn (CAss (Ass (Single D) (C (Register M))))
                        ] ++ pushDToStack

buildPush P.Temp y = buildPushSimple (VR (R 5)) y A
buildPush P.Argument y = buildPushSimple (PP ARG) y M
buildPush P.Local y = buildPushSimple (PP LCL) y M
buildPush P.This y = buildPushSimple (PP THIS) y M
buildPush P.That y = buildPushSimple (PP THAT) y M

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

buildPop P.Constant y = undefined
buildPop P.Static y = undefined
buildPop P.Pointer 0 = popFromStackToD ++
                       [ AIn (AtSymbol (PP THIS))
                       , CIn (CAss (Ass (Single M) (C (Register D))))
                       ]

buildPop P.Pointer 1 = popFromStackToD ++
                       [ AIn (AtSymbol (PP THAT))
                       , CIn (CAss (Ass (Single M) (C (Register D))))
                       ]

buildPop P.Temp y = buildPopSimple (VR (R 5)) y A
buildPop P.Argument y = buildPopSimple (PP ARG) y M
buildPop P.Local y = buildPopSimple (PP LCL) y M
buildPop P.This y = buildPopSimple (PP THIS) y M
buildPop P.That y = buildPopSimple (PP THAT) y M

buildPopSimple x y z = putTargetIn x y D z ++
                  [ AIn (AtSymbol (VR (R 13)))
                  , CIn (CAss (Ass (Single M) (C (Register D))))
                  ] ++
                  popFromStackToD ++
                  [ AIn (AtSymbol (VR (R 13)))
                  , CIn (CAss (Ass (Single A) (C (Register M))))
                  , CIn (CAss (Ass (Single M) (C (Register D))))
                  ]

putTargetIn x y z a = [ AIn (AtSymbol x)
                 , CIn (CAss (Ass (Single D) (C (Register a))))
                 , AIn (AtInt y)
                 , CIn (CAss (Ass (Single z) (Add (Register D) (Register A))))
                 ]

popFromStackToD = [ AIn (AtSymbol (PP SP))
                  , CIn (CAss (Ass (Double M A) (Minus (Register M) One)))
                  , CIn (CAss (Ass (Single D) (C (Register M))))
                  ]

module Grammar where

type Program = [Line]

data Line = AIn AInstruction
          | CIn CInstruction
          | LIn String

instance Show Line where
  show (AIn x) = show x
  show (CIn x) = show x
  show (LIn x) = "(" ++ x ++ ")"

data VirtualRegister = R Int

instance Show VirtualRegister where
  show (R x) = 'R' : show x

data PredefinedPointer = SP
                        | LCL
                        | ARG
                        | THIS
                        | THAT
                        deriving (Show)

data IOPointer = SCREEN
               | KBD
               deriving (Show)

data Symbol = VR VirtualRegister
            | PP PredefinedPointer
            | IP IOPointer
            | UDefSymbol String

instance Show Symbol where
  show (VR x) = show x
  show (PP x) = show x
  show (IP x) = show x
  show (UDefSymbol x) = x

data AInstruction = AtInt Int
                  | AtSymbol Symbol

instance Show AInstruction where
  show (AtInt x) = '@' : show x
  show (AtSymbol x) = '@' : show x

data Reg = D | A | M deriving (Show, Eq)

data DestReg = Single Reg
             | Double Reg Reg
             | Triple Reg Reg Reg
             | RNull

instance Show DestReg where
  show (Single x) = show x
  show (Double x y) = show x ++ show y
  show (Triple x y z) = show x ++ show y ++ show z
  show (RNull) = "Null"

data ConstExpr = One
               | Register Reg
               deriving (Eq)

instance Show ConstExpr where
  show One = "1"
  show (Register x) = show x

data Expr = Zero
          | C ConstExpr
          | Add ConstExpr ConstExpr
          | Minus ConstExpr ConstExpr
          | And ConstExpr ConstExpr
          | Or ConstExpr ConstExpr
          | Not ConstExpr
          | Negate ConstExpr

instance Show Expr where
  show Zero = "0"
  show (C x) = show x
  show (Add x y) = show x ++ "+" ++ show y
  show (Minus x y) = show x ++ "-" ++ show y
  show (And x y) = show x ++ "&" ++ show y
  show (Or x y) = show x ++ "|" ++ show y
  show (Not x) = "!" ++ show x
  show (Negate x) = "-" ++ show x

data Assignment = Ass DestReg Expr

instance Show Assignment where
  show (Ass x y) = show x ++ "=" ++ show y

data Jump = JMP
          | JEQ
          | JNE
          | JLT
          | JLE
          | JGT
          | JGE
          | JNull
          deriving (Show)

data CInstruction = CAss Assignment
                  | JAss Assignment Jump
                  | JExpr Expr Jump

instance Show CInstruction where
  show (CAss x) = show x
  show (JAss x y) = show x ++ ";" ++ show y
  show (JExpr x y) = show x ++ ";" ++ show y

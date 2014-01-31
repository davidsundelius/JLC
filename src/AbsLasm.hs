module AbsLasm where

newtype Ident    = Ident String deriving (Eq,Ord,Show)
newtype Label    = Label String deriving (Eq,Ord,Show)
newtype Register = Register String deriving (Eq,Ord,Show)

data LProgram =
  Prog [LTop]
  deriving (Eq,Ord,Show)

data LTop =
    Fun    LType Ident [LArg] [LStm]
  | DFun   LType Ident [LType]
  | Global Register String
   deriving (Eq,Ord,Show)

data LType =
   TVoid
 | TInt
 | TDbl
 | TBool
 | TPtr LType
 | TArray LType
 | TStruct [LType]
  deriving (Eq,Ord,Show)

data LBrType =
   BEq
 | BNeq
 | BGt
 | BGEq
 | BLt
 | BLEq
  deriving (Eq,Ord,Show)

data LArg =
  Arg LType Ident
  deriving (Eq,Ord,Show)

data LValue = VInt Integer | VDbl Double | VBool Integer | VString Register Integer
  deriving (Eq,Ord,Show)

data Operand =
   Var   LType Register
 | Const LValue
  deriving (Eq,Ord,Show)

data LStm =
   SCall Register Ident LType [Operand]
 | SVCall Ident [Operand]
 | SMul Register LType Operand Operand
 | SDiv Register LType Operand Operand
 | SAdd Register LType Operand Operand
 | SSub Register LType Operand Operand
 | SMod Register LType Operand Operand
 | SOr  Register LType Operand Operand
 | SAnd Register LType Operand Operand
 | SXor Register LType Operand Operand
 | SStore Operand Register
 | SLoad Register LType Register
 | SAlloc Register LType
 | SJmp Label
 | SBr Register Label Label
 | SCmp Register LBrType LType Operand Operand
 | SLabel Label
 | SCast Register Operand
 | SCalloc Register Register LType Operand Register
 | SInsStruct Register LType Register Operand Integer
 | SExtStruct Register LType Register Integer
 | SGEPtr Register LType Register [Operand]
 | SReturn LType Operand
 | SVReturn
  deriving (Eq,Ord,Show)


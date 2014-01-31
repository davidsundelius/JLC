module AbsJasm where

newtype Ident = Ident String deriving (Eq,Ord,Show)

data JProgram =
  Prog Ident [JFun]
  deriving (Eq,Ord,Show)

data JFun =
  Fun JType Ident [JArg] [JStm]
  deriving (Eq,Ord,Show)

data JType =
   TVoid
 | TInt
 | TDbl
 | TString
 | TArray JType
  deriving (Eq,Ord,Show)

data JBranchType =
   BLt
 | BLeq
 | BGt
 | BGeq
 | BEq
 | BEqZ
 | BNeq
 | BNeqZ
  deriving (Eq,Ord,Show)

data JDCmpType =
   BDCmpG
 | BDCmpL
  deriving (Eq,Ord,Show)

data JArg =
  Arg JType Ident
  deriving (Eq,Ord,Show)

data JValue = VInt Integer | VDbl Double | VString String
  deriving (Eq,Ord,Show)

data JStm =
   SPush JValue
 | SPop JType
 | SDup2
 | SSwap
 | SCall Ident JType [JType]
 | SStore JType Ident
 | SDec JType Ident
 | SLoad JType Ident
 | SBStart
 | SBEnd
 | SCmp JBranchType String
 | SCmpD
 | SGoto String
 | SLabel String
 | SMul JType
 | SDiv JType
 | SAdd JType
 | SSub JType
 | SMod JType
 | SOr
 | SAnd
 | SReturn JType
 | SNewA JType
 | SALoad JType
 | SAStore JType
 | SALen
  deriving (Eq,Ord,Show)


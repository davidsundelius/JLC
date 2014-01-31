module PrintLasm where

import Data.Char
import Data.Map hiding (map, foldr)
import Prelude hiding (lookup)
import Control.Monad.State

import Debug.Trace

import AbsLasm

data LEnv = Env
type EnvState = State LEnv

-- Printer
printLASM :: LProgram -> String
printLASM prog = render progDoc where
  progDoc = evalState (prt prog) Env

type Doc = [ShowS] -> [ShowS]

render :: Doc -> String
render d = rend (map ($ "") $ d []) "" where
  rend ss =
    case ss of
    t        :ts -> showString t . rend ts
    _            -> id

doc :: ShowS -> Doc
doc = (:)

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

concatEnvDoc :: [EnvState Doc] -> EnvState Doc
concatEnvDoc ed = do
  ed' <- sequence ed
  return $ concatD ed'

nl :: EnvState Doc
nl = prt "\n"

class Print a where
  prt :: a -> EnvState Doc
  prtList :: [a] -> EnvState Doc
  prtList = concatEnvDoc . map prt

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt s = return $ doc (showChar s)
  prtList s = return $ doc (showString s)

instance Print Int where
  prt x = return $ doc (shows x)

instance Print Integer where
  prt x = return $ doc (shows x)

instance Print Double where
  prt x = return $ doc (shows x)

instance Print Ident where
  prt (Ident s) = return $ doc (showString s)

instance Print LProgram where
  prt (Prog tls) = concatEnvDoc [prt "declare void @printInt(i32)\n",
                                 prt "declare void @printDouble(double)\n",
                                 prt "declare void @printString(i8*)\n",
                                 prt "declare i32 @readInt()\n",
                                 prt "declare double @readDouble()\n",
                                 prt "declare i8* @calloc(i64, i64)\n\n",
                                 prt tls]

instance Print LTop where
  prt (Fun t i as ss) = concatEnvDoc [nl, prt "define ", prt t, prt " @", prt i,
                          prt "(", prt as, prt ") {\n", prt ss, prt "  unreachable\n}", nl, nl]
  prt (DFun t i ts)   = concatEnvDoc [prt "declare ", prt t, prt " @", prt i,
                          prt "(", prt ts, prt ")\n"]
  prt (Global (Register r) s) = concatEnvDoc [prt "@", prt r,
    prt " = internal constant [", prt lenS, prt " x i8] c\"", prt s,
    prt "\\00\"", nl] where lenS = length s + 1

instance Print LType where
  prt TVoid        = prt "void"
  prt TInt         = prt "i32"
  prt TDbl         = prt "double"
  prt TBool        = prt "i1"
  prt (TStruct ts) = concatEnvDoc [prt "{", prt ts, prt "}"]
  prt (TArray t)   = concatEnvDoc [prt "[0 x ", prt t, prt "]"]
  prt (TPtr t)     = concatEnvDoc [prt t, prt "*"]
  prtList ts    = case ts of
    []   -> prt ""
    [x]  -> prt x
    x:xs -> concatEnvDoc [prt x, prt ", ", prt xs]

instance Print LArg where
  prt (Arg t i) = concatEnvDoc [prt t, prt " %", prt i]
  prtList as    = case as of
    []   -> prt ""
    [x]  -> prt x
    x:xs -> concatEnvDoc [prt x, prt ", ", prt xs]

instance Print LValue where
  prt (VInt i)                 = concatEnvDoc [prt "i32 ", prt i]
  prt (VDbl d)                 = concatEnvDoc [prt "double ", prt d]
  prt (VBool i)                = concatEnvDoc [prt "i1 ", prt i]
  prt (VString (Register r) i) = concatEnvDoc [prt "i8* getelementptr ([",
    prt i, prt " x i8]* @", prt r, prt ", i32 0, i64 0)"]

instance Print Operand where
  prt (Var t r)     = concatEnvDoc [prt t, prt " ", prt r]
  prt (Const v)     = prt v
  prtList os        = case os of
    []   -> prt ""
    [x]  -> prt x
    x:xs -> concatEnvDoc [prt x, prt ", ", prt xs]

instance Print LBrType where
  prt BEq  = prt "eq"
  prt BNeq = prt "ne"
  prt BGt  = prt "sgt"
  prt BGEq = prt "sge"
  prt BLt  = prt "slt"
  prt BLEq = prt "sle"

instance Print Register where
  prt (Register s) = concatEnvDoc [prt "%", prt s]

instance Print Label where
  prt (Label s) = prt s

instance Print LStm where
  prt stm = case stm of
    SCall r i t os -> concatEnvDoc [prt "  ", prt r, prt " = call ", prt t,
                        prt " @", prt i, prt "(", prt os, prt ")\n"]
    SVCall i os    -> concatEnvDoc [prt "  call void @", prt i, prt "(",
                        prt os, prt ")\n"]
    SAdd r t o1 o2 -> case t of
      TInt -> concatEnvDoc [prt "  ", prt r, prt " = add ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
      TDbl -> concatEnvDoc [prt "  ", prt r, prt " = fadd ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
    SSub r t o1 o2 -> case t of
      TInt -> concatEnvDoc [prt "  ", prt r, prt " = sub ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
      TDbl -> concatEnvDoc [prt "  ", prt r, prt " = fsub ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
    SMul r t o1 o2 -> case t of
      TInt -> concatEnvDoc [prt "  ", prt r, prt " = mul ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
      TDbl -> concatEnvDoc [prt "  ", prt r, prt " = fmul ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
    SDiv r t o1 o2 -> case t of
      TInt -> concatEnvDoc [prt "  ", prt r, prt " = sdiv ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
      TDbl -> concatEnvDoc [prt "  ", prt r, prt " = fdiv ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
    SMod r t o1 o2 -> case t of
      TInt -> concatEnvDoc [prt "  ", prt r, prt " = srem ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
      TDbl -> concatEnvDoc [prt "  ", prt r, prt " = frem ", prt t, prt " ",
                prtOp' o1, prt ", ", prtOp' o2, nl]
    SOr  r t o1 o2 -> concatEnvDoc [prt "  ", prt r, prt " = or ", prt t, prt " ",
                        prtOp' o1, prt ", ", prtOp' o2, nl]
    SAnd r t o1 o2 -> concatEnvDoc [prt "  ", prt r, prt " = and ", prt t, prt " ",
                        prtOp' o1, prt ", ", prtOp' o2, nl]
    SXor r t o1 o2 -> concatEnvDoc [prt "  ", prt r, prt " = xor ", prt t,
                                    prt " ", prtOp' o1, prt ", ", prtOp' o2, nl]
    SStore o r     -> concatEnvDoc [prt "  store ", prt o, prt ", ",
                                    prt (oType o), prt "* ", prt r, nl]
    SLoad r1 t r2  -> concatEnvDoc [prt "  ", prt r1, prt " = load ",
                                    prt t, prt "* ", prt r2, nl]
    SAlloc r t     -> concatEnvDoc [prt "  ", prt r, prt " = alloca ",
                                    prt t, nl]
    SJmp l         -> concatEnvDoc [prt "  br label %", prt l, nl]
    SBr r l1 l2    -> concatEnvDoc [prt "  br i1 ", prt r, prt ", label %",
                           prt l1, prt ", label %", prt l2, nl]
    SCmp r bt t o1 o2 -> case t of
      TDbl -> concatEnvDoc [prt "  ", prt r, prt " = fcmp ", prtBrD bt, prt " ",
                prt t, prt " ", prtOp' o1, prt ", ", prtOp' o2, nl]
      _    -> concatEnvDoc [prt "  ", prt r, prt " = icmp ", prt bt, prt " ",
                prt t, prt " ", prtOp' o1, prt ", ", prtOp' o2, nl]
    SLabel l           -> concatEnvDoc [prt l, prt ":\n"]
    SCalloc r1 r2 t o1 r3 -> concatEnvDoc [prt "  ", prt r1, prt " = zext ", prt o1, prt " to i64\n",
               prt "  ", prt r2, prt " = call i8* @calloc(", prt "i64 ", prt r1 , prt ", i64 ",
               prt sz, prt ")\n", prt "  ", prt r3, prt " = bitcast i8* ",
               prt r2, prt " to ", prt (TPtr (TArray t)), prt"\n"]
      where sz = case t of
                   TDbl -> "8"
                   TInt -> "4"
                   t    -> error $ "[Print LStm] unsupported array type: " ++ show t
    SInsStruct r1 t r2 o1 i -> concatEnvDoc [prt "  ", prt r1,
               prt " = insertvalue ", prt t, prt " ", prt r2, prt ", ", prt o1,
               prt ", ", prt i, prt "\n"]
    SExtStruct r1 t r2 i    -> concatEnvDoc [prt "  ", prt r1,
               prt " = extractvalue ", prt t, prt " ", prt r2, prt ", ",
               prt i, prt "\n"]
    SGEPtr r1 t r2 os -> concatEnvDoc [prt "  ", prt r1, prt " = getelementptr ",
                           prt t, prt " ", prt r2, prt ", ", prt os, nl]
    SReturn t o       -> concatEnvDoc [prt "  ret ", prt o, nl]
    SVReturn          -> prt "  ret void\n"

prtBrD :: LBrType -> EnvState Doc
prtBrD BEq  = prt "oeq"
prtBrD BNeq = prt "one"
prtBrD BGt  = prt "ogt"
prtBrD BGEq = prt "oge"
prtBrD BLt  = prt "olt"
prtBrD BLEq = prt "sle"

prtOp' :: Operand -> EnvState Doc
prtOp' (Var _ r) = prt r
prtOp' (Const v) = case v of
  VInt i  -> prt i
  VDbl d  -> prt d
  VBool b -> prt b
  _       -> error $ "[prtOp'] not implemented for value: " ++ show v

oType :: Operand -> LType
oType (Var t r) = t
oType (Const v) = case v of
  VInt _  -> TInt
  VDbl _  -> TDbl
  VBool _ -> TBool
  _       -> error $ "[oType] not implemented for value: " ++ show v




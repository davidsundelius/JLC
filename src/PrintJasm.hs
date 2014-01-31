module PrintJasm where

import Data.Char
import Data.Map hiding (map, foldr)
import Prelude hiding (lookup)
import Control.Monad.State

import AbsJasm


-- Env Stuff
-- class name, var count, var table
data JEnv = Env Ident Integer [Context]
type Context = Map Ident Integer

newEnv :: JProgram -> JEnv
newEnv (Prog cn _) = Env cn 0 [empty]

-- State monad stuff
type EnvState = State JEnv

-- addVar, if ident already has an address, just return, otherwise assign the
-- ident to an address and increase var count
addVar :: JType -> Ident -> EnvState ()
addVar t i = do
  (Env cn vc (vt:vts)) <- get
  case lookup i vt of
    Nothing -> case t of
                 TInt     -> put (Env cn (vc + 1) ((insert i vc vt):vts))
                 TDbl     -> put (Env cn (vc + 2) ((insert i vc vt):vts))
                 TArray _ -> put (Env cn (vc + 1) ((insert i vc vt):vts))
    Just _  -> return ()

addVars :: [JArg] -> EnvState ()
addVars [] = return ()
addVars (Arg t i:as) = do
  addVar t i
  addVars as

getAddr :: Ident -> EnvState Integer
getAddr i = do
  (Env _ _ vts) <- get
  getAddr' i vts

getAddr' :: Ident -> [Context] -> EnvState Integer
getAddr' i []       = error $ "[getAddr'] Variable not in scope: " ++ show i
getAddr' i (vt:vts) = case lookup i vt of
                        Nothing  -> getAddr' i vts
                        Just adr -> return adr

className :: EnvState Ident
className = do
  (Env cn _ _) <- get
  return cn

numVars :: EnvState Integer
numVars = do
  (Env _ vc _) <- get
  return vc

newScope :: EnvState ()
newScope = do
  (Env cn vc vts) <- get
  put (Env cn vc (empty:vts))

popScope :: EnvState ()
popScope = do
  (Env cn vc (_:vts)) <- get
  put (Env cn vc vts)

resetScope :: EnvState ()
resetScope = do
  (Env cn _ _) <- get
  put (Env cn 0 [empty])

calcLocals :: JFun -> Integer
calcLocals (Fun t i as ss) = (argsSize as) + (stmsSize ss)

argsSize :: [JArg] -> Integer
argsSize [] = 0
argsSize (Arg t i:as) = case t of
    TInt     -> 1 + (argsSize as)
    TDbl     -> 2 + (argsSize as)
    TArray _ -> 1 + (argsSize as)
    TString  -> 1 + (argsSize as)

stmsSize :: [JStm] -> Integer
stmsSize [] = 0
stmsSize (s:ss) = case s of
  SDec t i -> case t of
    TInt     -> 1 + (stmsSize ss)
    TDbl     -> 2 + (stmsSize ss)
    TArray _ -> 1 + (stmsSize ss)
    TString  -> 1 + (stmsSize ss)
  _          -> stmsSize ss


-- Printer

printJASM :: JProgram -> String
printJASM prog = render progDoc where
  progDoc = evalState (prt prog) (newEnv prog)

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend (map ($ "") $ d []) "" where
  rend ss =
    case ss of
    t        :ts -> showString t . rend ts
    _            -> id

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

concatEnvDoc :: [EnvState Doc] -> EnvState Doc
concatEnvDoc ed = do
  ed' <- sequence ed
  return $ concatD ed'

class Print a where
  prt :: a -> EnvState Doc
  prtList :: [a] -> EnvState Doc
  prtList = concatEnvDoc . map prt

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt s = return $ doc (showChar s)
  prtList ss = return $ doc (showString ss)

instance Print Integer where
  prt x = return $ doc (shows x)

instance Print Double where
  prt x = return $ doc (shows x)

instance Print Ident where
  prt (Ident i) = return $ doc (showString (i))

instance Print JProgram where
  prt e = case e of
    Prog cn defs -> concatEnvDoc [prt ".class public ", prt cn, prt "\n",
                             prt ".super java/lang/Object\n",
                             prt ".method public <init>()V\n",
                             prt "  aload_0\n",
                             prt "  invokespecial java/lang/Object/<init>()V\n",
                             prt "  return\n",
                             prt ".end method\n\n",
                             prt ".method public static main([Ljava/lang/String;)V\n",
                             prt ".limit locals 1\n  invokestatic ", prt cn, prt "/main()I\n",
                             prt "  pop\n  return\n.end method\n\n",
                             prt defs]

instance Print JFun where
  prt e = case e of
    Fun t i args ss -> do
      resetScope
      newScope
      addVars args
      str <- concatEnvDoc [prt ".method public static ",
        prt i, prt "(", prt args, prt ")", prt t,
        prt "\n.limit locals ", prt (calcLocals e), prt "\n.limit stack ", prt (calcStack ss),
        prt "\n",
        prt ss, prt ".end method\n\n"]
      popScope
      return str
  prtList es = case es of
    [] -> (concatEnvDoc [])
    x:xs -> (concatEnvDoc [prt x , prt xs])

instance Print JArg where
  prt e = case e of
    Arg t id -> concatEnvDoc [prt t]
  prtList es = case es of
    []   -> (concatEnvDoc [])
    x:xs -> (concatEnvDoc [prt x , prt xs])

pushOp :: JValue -> EnvState Doc
pushOp (VInt i)
  | i == 0                = prt "  iconst_0\n"
  | i == 1                = prt "  iconst_1\n"
  | i == 2                = prt "  iconst_2\n"
  | i == 3                = prt "  iconst_3\n"
  | i == 4                = prt "  iconst_4\n"
  | i == 5                = prt "  iconst_5\n"
  | i >= -128 && i <= 127 = concatEnvDoc [prt "  bipush ", prt i, prt "\n"]
  | otherwise             = concatEnvDoc [prt "  ldc ", prt i, prt "\n"]
pushOp (VDbl d)
  | d == 0.0              = prt "  dconst_0\n"
  | d == 1.0              = prt "  dconst_1\n"
  | otherwise             = concatEnvDoc [prt "  ldc2_w ", prt d, prt "\n"]
pushOp (VString s)        = concatEnvDoc [prt "  ldc \"", prt s, prt "\"\n"]

instance Print JStm where
  prt e = case e of
    SPush v        -> pushOp v
    SPop t         -> case t of
                     TInt -> concatEnvDoc [prt "  pop\n"]
                     TDbl -> concatEnvDoc [prt "  pop2\n"]
    SDup2          -> concatEnvDoc [prt "  dup2\n"]
    SSwap          -> concatEnvDoc [prt "  swap\n"]
    SCall i rt ts  -> do
      cn <- (case i of
        (Ident "printInt")    -> return (Ident "Runtime")
        (Ident "printDouble") -> return (Ident "Runtime")
        (Ident "readInt")     -> return (Ident "Runtime")
        (Ident "readDouble")  -> return (Ident "Runtime")
        (Ident "printString") -> return (Ident "Runtime")
        _                     -> className)
      concatEnvDoc [prt "  invokestatic ", prt cn, prt "/",
                    prt i, prt "(", prt ts, prt ")", prt rt, prt "\n"]
    SStore t i     -> case t of
                     TInt -> do
                       adr <- getAddr i
                       concatEnvDoc [prt "  istore ", prt adr, prt "\n"]
                     TDbl -> do
                       adr <- getAddr i
                       concatEnvDoc [prt "  dstore ", prt adr, prt "\n"]
                     TArray _ -> do
                       adr <- getAddr i
                       concatEnvDoc [prt "  astore ", prt adr, prt "\n"]
    SLoad t i      -> case t of
                     TInt -> do
                       adr <- getAddr i
                       concatEnvDoc [prt "  iload ", prt adr, prt "\n"]
                     TDbl -> do
                       adr <- getAddr i
                       concatEnvDoc [prt "  dload ", prt adr, prt "\n"]
                     TArray _ -> do
                       adr <- getAddr i
                       concatEnvDoc [prt "  aload ", prt adr, prt "\n"]
    SDec t i      -> do addVar t i
                        concatEnvDoc []
    SCmp bt l      -> case bt of
                     BLt    -> concatEnvDoc [prt "  if_icmplt ", prt l, prt "\n"]
                     BLeq   -> concatEnvDoc [prt "  if_icmple ", prt l, prt "\n"]
                     BGt    -> concatEnvDoc [prt "  if_icmpgt ", prt l, prt "\n"]
                     BGeq   -> concatEnvDoc [prt "  if_icmpge ", prt l, prt "\n"]
                     BEq    -> concatEnvDoc [prt "  if_icmpeq ", prt l, prt "\n"]
                     BEqZ   -> concatEnvDoc [prt "  ifeq ", prt l, prt "\n"]
                     BNeq   -> concatEnvDoc [prt "  if_icmpne ", prt l, prt "\n"]
                     BNeqZ  -> concatEnvDoc [prt "  ifne ", prt l, prt "\n"]
    SGoto l       -> concatEnvDoc [prt "  goto ", prt l, prt "\n"]
    SLabel l      -> concatEnvDoc [prt (l++":\n")]
    SCmpD         -> concatEnvDoc [prt "  dcmpg\n"]
    SBStart       -> do newScope
                        concatEnvDoc []
    SBEnd         -> do popScope
                        concatEnvDoc []
    SMul t        -> case t of
                     TInt -> concatEnvDoc [prt "  imul\n"]
                     TDbl -> concatEnvDoc [prt "  dmul\n"]
    SDiv t        -> case t of
                     TInt -> concatEnvDoc [prt "  idiv\n"]
                     TDbl -> concatEnvDoc [prt "  ddiv\n"]
    SAdd t        -> case t of
                     TInt -> concatEnvDoc [prt "  iadd\n"]
                     TDbl -> concatEnvDoc [prt "  dadd\n"]
    SSub t        -> case t of
                     TInt -> concatEnvDoc [prt "  isub\n"]
                     TDbl -> concatEnvDoc [prt "  dsub\n"]
    SMod t        -> case t of
                     TInt -> concatEnvDoc [prt "  irem\n"]
                     TDbl -> concatEnvDoc [prt "  drem\n"]
    SOr           -> concatEnvDoc [prt "  ior\n"]
    SAnd          -> concatEnvDoc [prt "  iand\n"]
    SALoad t      -> case t of
                     TInt -> concatEnvDoc [prt "  iaload\n"]
                     TDbl -> concatEnvDoc [prt "  daload\n"]
    SAStore t     -> case t of
                     TInt -> concatEnvDoc [prt "  iastore\n"]
                     TDbl -> concatEnvDoc [prt "  dastore\n"]
    SALen         -> concatEnvDoc [prt "  arraylength\n"]
    SNewA t       -> concatEnvDoc [prt "  newarray ", prt t', prt "\n"]
                     where t' = case t of
                                  TInt -> "int"
                                  TDbl -> "double"
                                  _    -> error $ "[Print JStm] unsupported array type: " ++ show t
    SReturn t     -> case t of
                     TInt     -> concatEnvDoc [prt "  ireturn\n"]
                     TDbl     -> concatEnvDoc [prt "  dreturn\n"]
                     TVoid    -> concatEnvDoc [prt "  return\n"]
                     TArray _ -> concatEnvDoc [prt "  areturn\n"]
  prtList es = case es of
    [] -> (concatEnvDoc [])
    x:xs -> (concatEnvDoc [prt x , prt xs])

instance Print JType where
  prt e = case e of
    TVoid    -> prt "V"
    TInt     -> prt "I"
    TDbl     -> prt "D"
    TArray t -> concatEnvDoc [prt "[", prt t]
    TString  -> prt "Ljava/lang/String;"
  prtList es = case es of
    [] -> (concatEnvDoc [])
    [x] -> (concatEnvDoc [prt x])
    x:xs -> (concatEnvDoc [prt x , prt xs])

calcStack :: [JStm] -> Integer
calcStack []     = 0
calcStack (s:ss) = case s of
  SPush v      -> case v of
                   (VDbl _) -> calcStack ss + 2
                   _        -> calcStack ss + 1
  SDup2        -> calcStack ss + 2
  SSwap        -> calcStack ss
  SLoad t i    -> case t of
                   TInt     -> calcStack ss + 1
                   TDbl     -> calcStack ss + 2
                   TArray _ -> calcStack ss + 1
  SCall _ t as -> case t of
                   TVoid    -> max 0 (calcStack ss - (calcArgs as))
                   TInt     -> max 0 (calcStack ss + 1 - (calcArgs as))
                   TDbl     -> max 0 (calcStack ss + 2 - (calcArgs as))
                   TArray _ -> max 0 (calcStack ss + 1 - (calcArgs as))
  SPop t       -> case t of
                   TInt   -> max 0 (calcStack ss - 1)
                   TDbl   -> max 0 (calcStack ss - 2)
  SStore t i   -> case t of
                   TInt     -> max 0 (calcStack ss - 1)
                   TDbl     -> max 0 (calcStack ss - 2)
                   TArray _ -> max 0 (calcStack ss - 1)
  SCmp bt l    -> case bt of
                   BEqZ   -> max 0 (calcStack ss - 1)
                   BNeqZ  -> max 0 (calcStack ss - 1)
                   _      -> max 0 (calcStack ss - 2)
  SCmpD        -> max 0 (calcStack ss -3)
  SMul t       -> case t of
                   TInt   -> max 0 (calcStack ss - 1)
                   TDbl   -> max 0 (calcStack ss - 2)
  SDiv t       -> case t of
                   TInt   -> max 0 (calcStack ss - 1)
                   TDbl   -> max 0 (calcStack ss - 2)
  SAdd t       -> case t of
                   TInt   -> max 0 (calcStack ss - 1)
                   TDbl   -> max 0 (calcStack ss - 2)
  SSub t       -> case t of
                   TInt   -> max 0 (calcStack ss - 1)
                   TDbl   -> max 0 (calcStack ss - 2)
  SMod t       -> case t of
                   TInt   -> max 0 (calcStack ss - 1)
                   TDbl   -> max 0 (calcStack ss - 2)
  SOr          -> max 0 (calcStack ss - 1)
  SAnd         -> max 0 (calcStack ss - 1)
  SGoto l      -> calcStack ss
  SLabel l     -> calcStack ss
  SReturn t    -> calcStack ss
  SBStart      -> calcStack ss
  SBEnd        -> calcStack ss
  SDec _ _     -> calcStack ss
  SALoad t     -> case t of
                    TInt   -> max 0 (calcStack ss - 1)
                    TDbl   -> calcStack ss
  SAStore t    -> case t of
                    TInt   -> max 0 (calcStack ss - 3)
                    TDbl   -> max 0 (calcStack ss - 4)
  SALen        -> calcStack ss
  SNewA t      -> calcStack ss

calcArgs :: [JType] -> Integer
calcArgs []     = 0
calcArgs (t:ts) = case t of
                    TInt    -> calcArgs ts + 1
                    TDbl    -> calcArgs ts + 2
                    TArray _-> calcArgs ts + 1
                    TString -> calcArgs ts + 1
                    _       -> error $ "[calcArgs] wrong type of argument to function: " ++ show t






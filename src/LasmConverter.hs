module LasmConverter where

import Control.Monad.State
import Data.Map hiding (map, foldr)
import Prelude hiding (lookup)
import Debug.Trace

import AbsLasm
import AbsJavalette

--Environment monad
data LEnv = Env Integer Integer Integer [Context] Context
type EnvState = State LEnv

data Pointer = Val Integer | Ref Integer deriving (Eq, Ord, Show)
type Context = Map AbsLasm.Ident Pointer

newEnv :: LEnv
newEnv = Env 0 0 0 [empty] empty

getFreshLabel :: EnvState Label
getFreshLabel = do
  (Env l c t r s) <- get
  put (Env (l+1) c t r s)
  return (Label ("label" ++ (show l)))

countStringRegs :: EnvState Integer
countStringRegs = do
  (Env _ _ _ _ s) <- get
  return (fromIntegral(size s))
  
getReg :: AbsJavalette.Ident -> EnvState (Register, Bool)
getReg i = do
  (Env _ _ _ rs _) <- get
  return (getReg' (cIdentL i) rs)
            
getReg' :: AbsLasm.Ident -> [Context] -> (Register, Bool)
getReg' i []     = error $ "[getReg] Register not found " ++ (show i)
getReg' i (r:rs) = case (lookup i r) of
            Just reg -> case reg of
              Val x -> (Register ("reg" ++ (show x)), True)
              Ref x -> (Register ("reg" ++ (show x)), False)
            Nothing  -> getReg' i rs

openScope :: EnvState()
openScope = do
  (Env l c t r s) <- get
  put (Env l c t (empty:r) s)
  
closeScope :: EnvState()
closeScope = do
  (Env l c t (_:rs) s) <- get
  put (Env l c t rs s)

getFreshReg :: AbsJavalette.Ident -> Bool -> EnvState Register
getFreshReg i byVal = do
  (Env l c t (r:rs) s) <- get
  put (Env l (c+1) t ((insert (cIdentL i) (createPointer (c+1) byVal) r):rs) s)
  return (Register ("reg" ++ (show (c+1))))
  
createPointer :: Integer -> Bool -> Pointer
createPointer x byVal = case byVal of
  True  -> Val x
  False -> Ref x

getFreshTmpReg :: EnvState Register
getFreshTmpReg = do
  (Env l c t r s) <- get
  put (Env l c (t+1) r s)
  return (Register ("tmp"++(show (t))))
  
getFreshString :: String -> EnvState Register
getFreshString s = do
  (Env l c t r sc) <- get
  case (lookup (AbsLasm.Ident s) sc) of
    Just (Ref x) -> return (Register ("string" ++ (show x)))
    _            -> do
      cs <- countStringRegs
      put (Env l c t r (insert (AbsLasm.Ident s) (createPointer (cs+1) False) sc))
      return (Register ("string" ++ (show (cs+1))))
  
getStrings :: EnvState Context
getStrings = do
  (Env _ _ _ _ sc) <- get
  return (sc)
  
--Converter
cASTL :: Program -> LProgram
cASTL (Program defs) = AbsLasm.Prog (defsL)
                       where defsL = evalState (combineGlobals defs) newEnv

combineGlobals :: [Def] -> EnvState [LTop]
combineGlobals ds = do 
                      ds' <- cDefsL ds
                      ss  <- findStrings
                      return (ss ++ ds')

cDefsL :: [Def] -> EnvState [LTop]
cDefsL []                           = return []
cDefsL ((Def t i as (Block ss)):ds) = do
                                        openScope
                                        as' <- cArgsL as
                                        ss' <- (cStmsL ss)
                                        ds' <- cDefsL ds
                                        closeScope
                                        return ((AbsLasm.Fun (cTypeL t) (cIdentL i) as' ([SLabel (Label "entry")] ++ ss')):ds')

cStmsL :: [Stm] -> EnvState [LStm]
cStmsL []     = return []
cStmsL (s:ss) = case s of
                  SRet _ -> cStmL s
                  SVRet  -> cStmL s
                  _      -> do
                              s'  <- cStmL s
                              ss' <- cStmsL ss
                              return (s' ++ ss')

cStmL :: Stm -> EnvState [LStm]
cStmL s = case s of
  SEmpty            -> return []
  SBlock (Block ss) -> do 
                         openScope
                         ss' <- cStmsL ss
                         closeScope
                         return ss'
  SDecl t is        -> cDeclVarsL is (cTypeL t)
  SAss i e          -> do 
                         (o, ss)   <- cExpL e
                         (addr, _) <- getReg i
                         return (ss ++ [SStore o addr])
  SRefAss i e1 e2   -> do
                         (e1', ss)  <- cExpL e1
                         (ptr, _)   <- getReg i
                         tmp1       <- getFreshTmpReg
                         tmp2       <- getFreshTmpReg
                         addr       <- getFreshTmpReg
                         (e2', ss') <- cExpL e2
                         return (ss ++ ss' ++ 
                           [SGEPtr tmp1 (TPtr(TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray (cExpTypeL e2))])) ptr [(Const (VInt 0)),(Const (VInt 1))],
                           SLoad tmp2 (TPtr (AbsLasm.TArray (cExpTypeL e2))) tmp1,
                           SGEPtr addr (TPtr (AbsLasm.TArray (cExpTypeL e2))) tmp2 [(Const (VInt 0)), e1'],
                           SStore e2' addr])
  SIncr i           -> do 
                         (reg, byVal) <- getReg i
                         case byVal of
                           True  -> return [SAdd reg AbsLasm.TInt (Var AbsLasm.TInt reg) (Const (VInt 1))]
                           False -> do 
                             val <- getFreshTmpReg
                             return [SLoad val AbsLasm.TInt reg, SAdd val AbsLasm.TInt (Var AbsLasm.TInt val) (Const (VInt 1))]
  SDecr i           -> do
                         (reg, byVal) <- getReg i
                         case byVal of
                           True  -> return [SSub reg AbsLasm.TInt (Var AbsLasm.TInt reg) (Const (VInt 1))]
                           False -> do 
                             val <- getFreshTmpReg
                             return [SLoad val AbsLasm.TInt reg, SSub val AbsLasm.TInt (Var AbsLasm.TInt val) (Const (VInt 1))]
  SRet e            -> do
                         (o, ss) <- cExpL e
                         return (ss ++ [SReturn (cExpTypeL e) o])
  SVRet             -> return [SVReturn]
  SIf e s1          -> case e of
                         EType ETrue _  -> cStmL s1
                         EType EFalse _ -> return []
                         _              -> do
                           true      <- getFreshLabel
                           false     <- getFreshLabel
                           (cmp, ss) <- cExpL e
                           ss'       <- cStmL s1
                           return (ss ++ [SBr (getRegister cmp) true false] ++ [SLabel (true)] ++ ss' ++ [SJmp (false)] ++ [SLabel (false)])
  SIfElse e s1 s2   -> case e of
                         EType ETrue _  -> cStmL s1
                         EType EFalse _ -> cStmL s2
                         _              -> do
                           true      <- getFreshLabel
                           false     <- getFreshLabel
                           end       <- getFreshLabel
                           (cmp, ss) <- cExpL e
                           ss1       <- cStmL s1
                           ss2       <- cStmL s2
                           return (ss ++ [SBr (getRegister cmp) true false] ++ 
                             [SLabel (true)] ++ ss1 ++ [SJmp (end)] ++
                             [SLabel (false)] ++ ss2 ++ [SJmp (end)] ++ [SLabel (end)])
  SWhile e s1       -> case e of
                         EType ETrue _  -> do
                           into      <- getFreshLabel
                           ss1       <- cStmL s1
                           return ([SJmp (into)] ++ [SLabel (into)] ++ ss1 ++ [SJmp (into)])
                         EType EFalse _ -> return []
                         _              -> do
                           into      <- getFreshLabel
                           test      <- getFreshLabel
                           out       <- getFreshLabel
                           (cmp, ss) <- cExpL e
                           ss1       <- cStmL s1
                           return ([SJmp (into)] ++ [SLabel (into)] ++ ss1 ++ 
                             [SJmp (test)] ++ [SLabel (test)] ++ ss ++ [SBr (getRegister cmp) into out] ++ 
                             [SLabel (out)])
 
  SFor t i e s     -> do
                         into <- getFreshLabel
                         test <- getFreshLabel
                         body <- getFreshLabel
                         out  <- getFreshLabel
                         (ldarr, ss) <- cExpL e
                         array <- getFreshTmpReg
                         var  <- getFreshReg i False
                         j    <- getFreshTmpReg
                         len  <- getFreshTmpReg
                         tmp1 <- getFreshTmpReg
                         tmp2 <- getFreshTmpReg
                         tmp3 <- getFreshTmpReg
                         tmp4 <- getFreshTmpReg
                         tmp5 <- getFreshTmpReg
                         tmp6 <- getFreshTmpReg
                         tmp7 <- getFreshTmpReg
                         tmp8 <- getFreshTmpReg
                         tmp9 <- getFreshTmpReg
                         s'   <- cStmL s
                         return ([SJmp (into), SLabel (into),
                           SAlloc j AbsLasm.TInt, SStore (Const (VInt 0)) j] ++
                           ss ++
                           [SAlloc array (TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray AbsLasm.TInt)]), SStore ldarr array,
                           SExtStruct len (TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray AbsLasm.TInt)]) (getRegister ldarr) 0,
                           SJmp (test), SLabel (test),
                           SLoad tmp3 (AbsLasm.TInt) j, SCmp tmp4 BNeq AbsLasm.TInt (Var AbsLasm.TInt tmp3) (Var AbsLasm.TInt len),
                           SBr tmp4 body out, SLabel (body),
                           SGEPtr tmp5 (TPtr(TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray t')])) array [(Const (VInt 0)),(Const (VInt 1))],
                           SLoad tmp6 (TPtr (AbsLasm.TArray t')) tmp5,
                           SLoad tmp7 (AbsLasm.TInt) j, SGEPtr var (TPtr (AbsLasm.TArray t')) tmp6 [(Const (VInt 0)), (Var (AbsLasm.TInt) tmp7)]] ++
                           s' ++
                           [SLoad tmp8 (AbsLasm.TInt) j, SAdd tmp9 AbsLasm.TInt (Var AbsLasm.TInt tmp8) (Const (VInt 1)), SStore (Var AbsLasm.TInt tmp9) j,
                           SJmp (test), SLabel (out)])
                         where t' = cTypeL t
  SExp e            -> do 
                         (_, ss) <- cExpL e
                         return ss

cExpL :: Exp -> EnvState (Operand, [LStm])
cExpL et@(EType e t) = case e of
  EVar i        -> do
                     (reg, byVal) <- getReg i
                     case byVal of
                               True  -> return ((Var (cTypeL t) reg), [])
                               False -> do
                                          val <- getFreshTmpReg
                                          return ((Var (cTypeL t) val), [SLoad val (cTypeL t) reg])
  EInt n        -> return (Const (VInt n), [])
  EDbl d        -> return (Const (VDbl d), [])
  ETrue         -> return (Const (VBool 1), [])
  EFalse        -> return (Const (VBool 0), [])
  EApp i es     -> case t of
                     AbsJavalette.TVoid -> do
                       (os, ss) <- cAppArgsL es [] []
                       return (Const (VInt 0), ss ++ [SVCall (cIdentL i) os])
                     _                  -> do
                       (os, ss) <- cAppArgsL es [] []
                       reg <- getFreshTmpReg
                       return (Var (cTypeL t) reg, ss ++ [SCall reg (cIdentL i) (cTypeL t) os])
  EString s     -> do
                     reg <- getFreshString s
                     return (Const (VString reg (fromIntegral (length s)+1)), [])
  ERef i e1     -> do
                     (e1', ss) <- cExpL e1
                     (ptr, _)  <- getReg i
                     tmp1      <- getFreshTmpReg
                     tmp2      <- getFreshTmpReg
                     addr      <- getFreshTmpReg
                     val       <- getFreshTmpReg
                     return (Var (cTypeL t) val, ss ++
                       [SGEPtr tmp1 (TPtr(TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray (cTypeL t))])) ptr [(Const (VInt 0)),(Const (VInt 1))],
                       SLoad tmp2 (TPtr (AbsLasm.TArray (cTypeL t))) tmp1,
                       SGEPtr addr (TPtr (AbsLasm.TArray (cTypeL t))) tmp2 [(Const (VInt 0)), e1'], 
                       SLoad val (cTypeL t) addr])
  ENew t'' e1   -> do
                     (e1', ss) <- cExpL e1
                     tmp0      <- getFreshTmpReg
                     tmp1      <- getFreshTmpReg
                     tmp2      <- getFreshTmpReg
                     tmp3      <- getFreshTmpReg
                     tmp4      <- getFreshTmpReg
                     tmp5      <- getFreshTmpReg
                     tmp6      <- getFreshTmpReg
                     reg       <- getFreshTmpReg
                     return (Var t' reg, [SAlloc tmp0 t', SLoad tmp1 t' tmp0] ++ 
                       ss ++ [SCalloc tmp2 tmp3 (cTypeL t'') e1' tmp4,
                       SInsStruct tmp6 t' tmp1 e1' 0, SInsStruct reg t' tmp6 (Var (TPtr (AbsLasm.TArray (cTypeL t''))) tmp4) 1])
                   where t' = cTypeL t
  EALen i       -> do 
                     (addr, _) <- getReg i
                     tmp       <- getFreshTmpReg
                     reg       <- getFreshTmpReg
                     return (Var (AbsLasm.TInt) reg, [SLoad tmp (TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray AbsLasm.TInt)]) addr,
                       SExtStruct reg (TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray AbsLasm.TInt)]) tmp 0])
  ENeg e1       -> do
                     (e1', ss)  <- cExpL e1
                     reg        <- getFreshTmpReg
                     return (case t of
                       AbsJavalette.TInt -> ((Var (cTypeL t) reg), (ss ++ [SMul reg (cTypeL t) e1' (Const (VInt (-1)))]))
                       AbsJavalette.TDbl -> ((Var (cTypeL t) reg), (ss ++ [SMul reg (cTypeL t) e1' (Const (VDbl (-1.0)))])))
  ENot e1       -> do
                     (e1', ss)  <- cExpL e1
                     reg        <- getFreshTmpReg
                     return ((Var (cTypeL t) reg), (ss ++ [SXor reg (cTypeL t) e1' (Const (VInt (1)))]))
  EMul e1 op e2 -> do
                     (e1', ss)  <- cExpL e1
                     (e2', ss') <- cExpL e2
                     reg        <- getFreshTmpReg
                     return (case op of
                       OTimes -> ((Var (cTypeL t) reg), (ss ++ ss' ++ [SMul reg (cTypeL t) e1' e2']))
                       ODiv   -> ((Var (cTypeL t) reg), (ss ++ ss' ++ [SDiv reg (cTypeL t) e1' e2']))
                       OMod   -> ((Var (cTypeL t) reg), (ss ++ ss' ++ [SMod reg (cTypeL t) e1' e2'])))
  EAdd e1 op e2 -> do
                     (e1', ss)  <- cExpL e1
                     (e2', ss') <- cExpL e2
                     reg        <- getFreshTmpReg
                     return (case op of
                       OPlus  -> ((Var (cTypeL t) reg), (ss ++ ss' ++ [SAdd reg (cTypeL t) e1' e2']))
                       OMinus -> ((Var (cTypeL t) reg), (ss ++ ss' ++ [SSub reg (cTypeL t) e1' e2'])))
  ERel e1 op e2 -> do
                     (e1', ss)  <- cExpL e1
                     (e2', ss') <- cExpL e2
                     reg        <- getFreshTmpReg
                     return ((Var (cTypeL t) reg), (ss ++ ss' ++ [SCmp reg (cRelOpL op) (cExpTypeL e1) e1' e2']))
  EAnd e1 e2    -> do
                     (e1', ss)  <- cExpL e1
                     (e2', ss') <- cExpL e2
                     tmp1       <- getFreshTmpReg
                     tmp2       <- getFreshTmpReg
                     addr       <- getFreshTmpReg
                     reg        <- getFreshTmpReg
                     true       <- getFreshLabel
                     false      <- getFreshLabel
                     end        <- getFreshLabel
                     return ((Var AbsLasm.TBool reg), ([SAlloc addr AbsLasm.TBool] ++ ss ++ [SCmp tmp1 BNeq AbsLasm.TBool e1' (Const (VBool 0))] ++
                              [SBr tmp1 true false, SLabel true] ++ ss' ++
                              [SAnd tmp2 AbsLasm.TBool e1' e2', SStore (Var AbsLasm.TBool tmp2) addr, SJmp end] ++
                              [SLabel false, SStore (Const (VBool 0)) addr, SJmp end] ++
                              [SLabel end, SLoad reg AbsLasm.TBool addr]))
  EOr e1 e2     -> do
                     (e1', ss)  <- cExpL e1
                     (e2', ss') <- cExpL e2
                     tmp1       <- getFreshTmpReg
                     tmp2       <- getFreshTmpReg
                     addr       <- getFreshTmpReg
                     reg        <- getFreshTmpReg
                     true       <- getFreshLabel
                     false      <- getFreshLabel
                     end        <- getFreshLabel
                     return ((Var AbsLasm.TBool reg), ([SAlloc addr AbsLasm.TBool] ++ ss ++ [SCmp tmp1 BNeq AbsLasm.TBool e1' (Const (VBool 1))] ++
                              [SBr tmp1 true false, SLabel true] ++ ss' ++
                              [SOr tmp2 AbsLasm.TBool e1' e2', SStore (Var AbsLasm.TBool tmp2) addr, SJmp end] ++
                              [SLabel false, SStore (Const (VBool 1)) addr, SJmp end] ++
                              [SLabel end, SLoad reg AbsLasm.TBool addr]))
  EType e t     -> error $ "[cExpL] Should not try to convert a EType " ++ (show e)

cAppArgsL :: [Exp] -> [Operand] -> [LStm] -> EnvState ([Operand], [LStm])
cAppArgsL [] os ss     = return (os, ss)                       
cAppArgsL (e:es) os ss = do
                           (o, ss') <- cExpL e
                           case o of
                             Var (TStruct ts) r -> do 
                                                    tmp      <- getFreshTmpReg
                                                    cAppArgsL es (os++[Var (TPtr (TStruct ts)) tmp]) (ss++ss'++[SAlloc tmp (TStruct ts), SStore o tmp])
                                                    
                             _                  -> cAppArgsL es (os++[o]) (ss++ss')
                           
                           

cDeclVarsL :: [Item] -> LType -> EnvState [LStm]
cDeclVarsL [] _       = return []
cDeclVarsL (it:its) t = case it of
  INoInit i -> case t of 
                 AbsLasm.TInt       -> do
                                         newreg <- getFreshReg i False
                                         ss     <- (cDeclVarsL its t)
                                         return ([SAlloc newreg t, SStore (Const (VInt 0)) newreg] ++ ss)
                 AbsLasm.TDbl       -> do
                                         newreg <- getFreshReg i False
                                         ss     <- (cDeclVarsL its t)
                                         return ([SAlloc newreg t, SStore (Const (VDbl 0.0)) newreg] ++ ss)
                 AbsLasm.TArray t'  -> do
                                         newreg <- getFreshReg i False
                                         ss     <- (cDeclVarsL its t)
                                         return ([SAlloc newreg t] ++ ss)
  IInit i e -> do (o, ss) <- cExpL e
                  newreg  <- getFreshReg i False
                  ss'     <- (cDeclVarsL its t)
                  return ([SAlloc newreg t] ++ ss ++ [SStore o newreg] ++ ss')

cExpTypeL :: Exp -> LType
cExpTypeL (EType _ t) = cTypeL t
cExpTypeL e           = error $ "[cExpTypeL] not an EType, instead: " ++ show e

cArgsL :: [Arg] -> EnvState [LArg]
cArgsL []                          = return []
cArgsL ((AbsJavalette.Arg t i):as) = do (Register s) <- getFreshReg i byVal
                                        as' <- (cArgsL as)
                                        return ((AbsLasm.Arg t' (AbsLasm.Ident s)):as')
                                     where t' = case t of
                                                  AbsJavalette.TArray t'' -> TPtr (TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray (cTypeL t''))])
                                                  _                       -> cTypeL t
                                           byVal = case t of
                                                     AbsJavalette.TArray _ -> False
                                                     _                     -> True

cTypeL :: Type -> LType
cTypeL t = case t of
  AbsJavalette.TInt        -> AbsLasm.TInt
  AbsJavalette.TDbl        -> AbsLasm.TDbl
  AbsJavalette.TBool       -> AbsLasm.TBool
  AbsJavalette.TVoid       -> AbsLasm.TVoid
  (AbsJavalette.TArray t') -> TStruct [AbsLasm.TInt, TPtr (AbsLasm.TArray (cTypeL t'))]

cArrayTypeL :: Type -> LType
cArrayTypeL t = case t of
  (AbsJavalette.TArray t') -> cTypeL t'
  _                        -> error $ "[cTypeArray] Input type is not array but " ++ show t

cRelOpL :: RelOp -> LBrType
cRelOpL op = case op of
  OLt  -> BLt
  OLeq -> BLEq
  OGt  -> BGt
  OGeq -> BGEq
  OEq  -> BEq
  ONEq -> BNeq

cIdentL :: AbsJavalette.Ident -> AbsLasm.Ident
cIdentL (AbsJavalette.Ident s) = AbsLasm.Ident s

getRegister :: Operand -> Register
getRegister (Var t reg) = reg
getRegister op          = error $ "[getRegister] Operand not register: " ++ (show op)

findStrings :: EnvState [LTop]
findStrings = do
  sc <- getStrings
  return (cGlobalsL (toList sc))
  
cGlobalsL :: [(AbsLasm.Ident, Pointer)]-> [LTop]
cGlobalsL []                            = []
cGlobalsL ((AbsLasm.Ident s, Ref x):ss) = (Global (Register ("string"++show x)) s):(cGlobalsL ss)


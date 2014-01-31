module TypeChecker where

import Prelude hiding (lookup)

import Data.Map
import Data.Maybe

import Debug.Trace

import AbsJavalette
import PrintJavalette
import ErrM

type Env = (Sig, [Context])
type Sig = Map Ident ([Type], Type)
type Context = Map Ident Type

lookupVar :: Ident -> Env -> Err Type
lookupVar i  (_, [])  = fail $ "[lookupVar] Variable not in scope: " ++ printTree i
lookupVar i (s, c:cs) = case (lookup i c) of
  Nothing -> lookupVar i (s, cs)
  Just t  -> return t

lookupFun :: Ident -> Env -> Err ([Type], Type)
lookupFun i (s, _) = case lookup i s of
  Nothing -> fail $ "[lookupFun] Function not in scope: " ++ printTree i
  Just t  -> return t

addFun :: Ident -> ([Type], Type) -> Env -> Err Env
addFun i fs (s, cs) = case lookup i s of
  Nothing -> return (insert i fs s, cs)
  Just _  -> fail $ "[addFun] Function identifier already in use: " ++ printTree i

addVar :: Ident -> Type -> Env -> Err Env
addVar i t (s, c:cs) = case lookup i c of
  Nothing -> return (s, (insert i t c):cs)
  Just _  -> fail $ "[addVar] Variable identifier already in use: " ++ printTree i

updateVar :: Ident -> Env -> Type -> Err Env
updateVar i (_, []) _    = fail $ "[updateVar] Variable not in scope: " ++ printTree i
updateVar i (s, c:cs) t  = case (lookup i c) of
  Nothing -> do
    (_, cs') <- updateVar i (s, cs) t
    return (s, c:cs')
  Just _ -> return (s, (insert i t c):cs)

openScope :: Env -> Env
openScope (s, c) = (s, empty:c)

closeScope :: Env -> Env
closeScope (s, [])   = (s, [])
closeScope (s, c:cs) = (s, cs)

emptyEnv :: Env
emptyEnv = (empty,[empty])

argTypes :: [Arg] -> [Type]
argTypes []               = []
argTypes ((Arg t _):as) = t:(argTypes as)

addArgs :: [Arg] -> Env -> Err Env
addArgs [] env             = return env
addArgs ((Arg t i):as) env = do
  env' <- addVar i t env
  addArgs as env'

infer :: Exp -> Env -> Err Type
infer e env = case e of
  EVar i         -> lookupVar i env
  EInt n         -> return TInt
  EDbl d         -> return TDbl
  ETrue          -> return TBool
  EFalse         -> return TBool
  EApp i exps    -> do
   (ts, rt) <- lookupFun i env
   its      <- sequence [infer exp env | exp <- exps]
   if its == ts
     then return rt
     else fail $ "[infer] Type mismatch in arguments to function: " ++ printTree i
  EString s      -> return TString
  ERef i e       -> inferArray i env
  ENew t e       -> return (TArray t)
  EALen i        -> do
    t <- lookupVar i env
    (case t of
      TArray _  -> return TInt
      _         -> fail $ "[infer] type of input expression is not array " ++ printTree i)
  ENeg e1        -> infer e1 env
  ENot e1        -> infer e1 env
  EMul e1 _ e2   -> inferBin e1 e2 env [TInt, TDbl]
  EAdd e1 _ e2   -> inferBin e1 e2 env [TInt, TDbl]
  ERel e1 _ e2   -> do
    t <- infer e2 env
    t' <- checkExp e1 t env
    if elem t' [TInt, TDbl, TBool]
      then return TBool
      else fail $ "[infer] Can not compare expressions: " ++
                  show e1 ++ " and " ++ show e2
  EAnd e1 e2     -> inferBin e1 e2 env [TBool]
  EOr e1 e2      -> inferBin e1 e2 env [TBool]
  EType e t      -> return t

inferBin :: Exp -> Exp -> Env -> [Type] -> Err Type
inferBin e1 e2 env types = do
  t <- infer e1 env
  if elem t types 
    then checkExp e2 t env
    else fail $ "[inferBin] wrong type of expression: " ++ printTree e1
    
inferArray :: Ident -> Env -> Err Type
inferArray i env = do
  t <- lookupVar i env
  (case t of
     TArray t' -> return t'
     _         -> fail $ "[inferArray] type of input expression is not array " ++ printTree i)

compareTypes :: Exp -> Exp -> Env -> Err Type
compareTypes e0 e1 env = do
  t0 <- infer e0 env
  t1 <- infer e1 env
  if t0 == t1
    then return t0
    else fail $ "[compareTypes] Types do not match " ++ printTree e0 ++ " and " ++ printTree e1

checkExp :: Exp -> Type -> Env -> Err Type
checkExp e t env = do
  t' <- infer e env
  if t' == t
    then return t
    else
      fail $ "[checkExp] Type of " ++ printTree e ++
             " expected " ++ printTree t ++
             " but found " ++ printTree t'

annotate :: Exp -> Type -> Env -> Err Exp
annotate e t env = case e of
  EApp i es     -> do
    ts  <- sequence [infer e' env | e' <- es]
    es' <- sequence [annotate e'' t' env | (e'', t') <- (zip es ts)]
    return (EType (EApp i es') t)
  ERef i e      -> do
    t'' <- infer e env
    e'  <- annotate e t'' env
    et  <- inferArray i env
    return (EType (ERef i e') et)
  ENew i e      -> do
    t'  <- infer e env
    e'  <- annotate e t' env
    return (EType (ENew i e') t)
  ENeg e1       -> do
    t'  <- infer e1 env
    e1' <- annotate e1 t' env
    return (EType (ENeg e1') t)
  ENot e1       -> do
    t'  <- infer e1 env
    e1' <- annotate e1 t' env
    return (EType (ENot e1') t)
  EMul e1 op e2 -> do
    t1  <- infer e1 env
    e1' <- annotate e1 t1 env
    t2  <- infer e2 env
    e2' <- annotate e2 t2 env
    return (EType (EMul e1' op e2') t)
  EAdd e1 op e2 -> do
    t1  <- infer e1 env
    e1' <- annotate e1 t1 env
    t2  <- infer e2 env
    e2' <- annotate e2 t2 env
    return (EType (EAdd e1' op e2') t)
  ERel e1 op e2 -> do
    t1  <- infer e1 env
    e1' <- annotate e1 t1 env
    t2  <- infer e2 env
    e2' <- annotate e2 t2 env
    return (EType (ERel e1' op e2') t)
  EAnd e1 e2    -> do
    t1  <- infer e1 env
    e1' <- annotate e1 t1 env
    t2  <- infer e2 env
    e2' <- annotate e2 t2 env
    return (EType (EAnd e1' e2') t)
  EOr  e1 e2    -> do
    t1  <- infer e1 env
    e1' <- annotate e1 t1 env
    t2  <- infer e2 env
    e2' <- annotate e2 t2 env
    return (EType (EOr e1' e2') t)
  _             -> return (EType e t)

declItems :: Type -> [Item] -> Env -> Err ([Item], Env)
declItems _ [] env     = return ([], env)
declItems t (i:is) env = case i of
                           (INoInit id)   -> do
                             (is', env') <- declItems t is env
                             env'' <- addVar id t env'
                             return (i:is', env'')
                           (IInit id exp) -> do
                             (is', env') <- declItems t is env
                             t' <- checkExp exp t env
                             exp' <- (annotate exp t' env)
                             env'' <- addVar id t env'
                             return ((IInit id exp'):is', env'')

checkStm :: Stm -> Type -> Env -> Err (Stm, Env)
checkStm stm rt env = case stm of
  SEmpty            -> return (stm, env)
  SBlock (Block ss) -> do
    (ss', _) <- checkStms ss rt (openScope env)
    return (SBlock (Block ss'), env)
  SDecl t is        -> case t of
    TArray t' -> do
      (is', env') <- declItems (TArray t') is env
      return (SDecl (TArray t') is', env')
    _         -> do 
      (is', env') <- declItems t is env
      return (SDecl t is', env')
  SAss i e          -> do
    t  <- infer e env
    t' <- checkExp (EVar i) t env
    e' <- (annotate e t' env)
    return (SAss i e', env)
  SRefAss i e1 e2   -> do
    t1'  <- checkExp e1 TInt env
    e1'  <- (annotate e1 t1' env)
    t2   <- infer e2 env
    t2'  <- inferArray i env
    if t2 == t2' 
    then (do
      e2'  <- (annotate e2 t2 env)
      return (SRefAss i e1' e2', env))
    else fail $ "[checkStm] Types do not match " ++ show t2 ++ " and " ++ show t2'
  SIncr i           -> checkStm (SAss i (EAdd (EVar i) OPlus  (EInt 1))) rt env
  SDecr i           -> checkStm (SAss i (EAdd (EVar i) OMinus (EInt 1))) rt env
  SRet e            -> do
    t  <- checkExp e rt env
    e' <- annotate e t env
    return (SRet e', env)
  SVRet             -> if rt == TVoid
    then return (SVRet, env)
    else fail $ "[checkStm] found void return in function with return type: " ++ show rt
  SIf e s           -> do
    (s', env') <- checkStm s rt env
    t          <- checkExp e TBool env
    e'         <- annotate e t env
    return (SIf e' s', env')
  SIfElse e s1 s2   -> do
    (s2', env')  <- checkStm s2 rt env
    (s1', env'') <- checkStm s1 rt env'
    t  <- checkExp e TBool env
    e' <- annotate e t env
    return (SIfElse e' s1' s2', env'')
  SWhile e s        -> do
    (s', env') <- checkStm s rt env
    t  <- checkExp e TBool env
    e' <- annotate e t env
    return (SWhile e' s', env')
  SFor t i e s      -> do
    env'    <- addVar i t (openScope env)
    (s', _) <- checkStm s rt env'
    t'      <- checkExp e (TArray t) env
    e'      <- annotate e t' env
    return (SFor t i e' s', env)
  SExp e            -> do
    t  <- infer e env
    e' <- annotate e t env
    return (SExp e', env)

checkStms :: [Stm] -> Type -> Env -> Err ([Stm], Env)
checkStms [] _ env         = return ([], env)
checkStms (s:ss) rtype env = do
  (s', env')   <- checkStm s rtype env
  (ss', env'') <- checkStms ss rtype env'
  return (s':ss',env'')

checkReturn :: [Stm] -> Bool
checkReturn [] = False
checkReturn (s:ss) = case s of
  (SIf e s)            -> case e of
                            ETrue  -> if checkReturn [s] then True else checkReturn ss
                            _      -> checkReturn ss
  (SIfElse e s1 s2)    -> case e of
                            ETrue  -> if checkReturn [s1] then True else checkReturn ss
                            EFalse -> if checkReturn [s2] then True else checkReturn ss
                            _      -> if checkReturn [s1] && checkReturn [s2] then True else checkReturn ss
  (SWhile e s)         -> case e of
                            ETrue  -> if checkReturn [s] then True else checkReturn ss
                            _      -> checkReturn ss
  (SRet e)             -> True
  SVRet                -> True
  (SBlock (Block ss')) -> if checkReturn ss' then True else checkReturn ss
  _                    -> checkReturn ss

checkMain :: Env -> Err ()
checkMain (s,_) = case lookup (Ident "main") s of
  Nothing         -> fail $ "[checkMain] Could not find main function"
  (Just (ts, rt)) -> if ts == [] && rt == TInt
                       then return ()
                       else fail $ "[checkMain] Mismatching types of arguments or returntype of main, should be int main()"

checkProgram :: [Def] -> Env -> Err ([Def], Env)
checkProgram [] env                           = return ([], env)
checkProgram ((Def t i as (Block ss)):ds) env =
    if t == TVoid || checkReturn ss
    then do env'          <- addArgs as (openScope env)
            (ss'', env'') <- checkStms (ss') t env'
            (ds', env''') <- checkProgram ds env''
            return ((Def t i as (Block ss'')):ds', env''')
    else fail $ "[checkProgram] missing/unreachable return statement in function: " ++ printTree i
  where ss' = if t==TVoid && (checkReturn ss)==False then ss++[SVRet]else ss


buildEnv :: [Def] -> Env -> Err Env
buildEnv [] env                  = return env
buildEnv ((Def t i as _):ds) env = do
  env' <- addFun i (argTypes as, t) env
  buildEnv ds env'

addBuiltIns :: Env -> Env
addBuiltIns (s, cs) = (insert (Ident "printInt") ([TInt], TVoid)
                      (insert (Ident "printDouble") ([TDbl], TVoid)
                      (insert (Ident "readInt") ([],TInt)
                      (insert (Ident "readDouble") ([], TDbl)
                      (insert (Ident "printString") ([TString], TVoid) s)))), cs)

typecheck :: Program -> Err Program
typecheck (Program defs) = do
  env <- buildEnv defs (addBuiltIns emptyEnv)
  checkMain env
  (defs',_) <- checkProgram defs env
  return (Program (defs'))


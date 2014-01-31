module JasmConverter where

import AbsJasm
import AbsJavalette

--Jasm converter
cASTJ :: Program -> String -> JProgram
cASTJ (Program defs) name = AbsJasm.Prog (AbsJasm.Ident name) (cDefsJ defs)

cDefsJ :: [Def] -> [JFun]
cDefsJ []                           = []
cDefsJ ((Def t i as (Block ss)):ds) = (AbsJasm.Fun (cTypeJ t) (cIdentJ i) (cArgsJ as) ([SLabel "entry"] ++ stms)):(cDefsJ ds)
                                      where (stms,_) = (cStmsJ ss' 0)
                                            ss'      = case checkReturn ss of
                                                         True  -> ss
                                                         False -> ss ++ [SVRet]

cStmsJ :: [Stm] -> Int -> ([JStm],Int)
cStmsJ [] l     = ([], l)
cStmsJ (s:ss) l = case s of
                    SRet _ -> (s',l')
                    SVRet  -> (s',l')
                    _      -> (s'++(ss'), l'')
                  where (ss',l'') = cStmsJ ss l'
                        (s',l') = cStmJ s l

cStmJ :: Stm -> Int -> ([JStm],Int)
cStmJ s l = case s of
  SEmpty            -> ([], l)
  SBlock (Block ss) -> ([SBStart] ++ ss' ++ [SBEnd], l')
                       where (ss', l') = cStmsJ ss l
  SDecl t is        -> case t of
                         AbsJavalette.TArray t' -> (cDeclVars is (AbsJasm.TArray (cTypeJ t)) l, l)
                         _                      -> (cDeclVars is (cTypeJ t) l, l)
  SAss i e          -> (e' ++ [SStore (cExpTypeJ e) (cIdentJ i)], l')
                       where (e', l') = (cExpJ e True l)
  SRefAss i e1 e2   -> ([SLoad (AbsJasm.TArray (cExpTypeJ e2)) (cIdentJ i)] ++ e1' ++ e2' ++ [SAStore (cExpTypeJ e2)], l)
                       where (e1', lu) = (cExpJ e1 True l')
                             (e2', l') = (cExpJ e2 True l)
  SIncr i           -> ([SLoad AbsJasm.TInt (cIdentJ i), SPush (VInt 1), SAdd AbsJasm.TInt, SStore AbsJasm.TInt (cIdentJ i)], l)
  SDecr i           -> ([SLoad AbsJasm.TInt (cIdentJ i), SPush (VInt (-1)), SAdd AbsJasm.TInt, SStore AbsJasm.TInt (cIdentJ i)], l)
  SRet e            -> (e' ++ [SReturn (cExpTypeJ e)], l')
                       where (e', l') = (cExpJ e True l)
  SVRet             -> ([SReturn AbsJasm.TVoid], l)
  SIf e s1          -> case e of
                         EType ETrue _  -> (s1',l')
                         EType EFalse _ -> ([],l)
                         _              -> (cmps ++ [SCmp BEqZ (getLabel lu)] ++ s1' ++ [SLabel (getLabel lu)], lu+1)
                       where (cmps,lu) = (cRelJ e l')
                             (s1',l')  = cStmJ s1 l
  SIfElse e s1 s2   -> case e of
                         EType ETrue _  -> (s1',l'')
                         EType EFalse _ -> (s2',l')
                         _              -> case checkReturn [s2] of
                                             True  -> (cmps ++ [SCmp BEqZ (getLabel lu)] ++ s1' ++ [SLabel (getLabel lu)] ++ s2', lu+1)
                                             False -> (cmps ++ [SCmp BEqZ (getLabel lu)] ++ s1' ++ [SGoto (getLabel (lu+1))] ++ 
                                                      [SLabel (getLabel lu)] ++ s2' ++ [SLabel (getLabel (lu+1))], lu+2)
                       where (cmps,lu) = (cRelJ e l'')
                             (s1',l'') = cStmJ s1 l'
                             (s2',l')  = cStmJ s2 l
  SWhile e s1       -> ([SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++ s1' ++ [SLabel (getLabel (lu+1))] ++ cmps ++ 
                        [SCmp BNeqZ (getLabel lu)], lu+2)
                       where (cmps,lu) = (cRelJ e l')
                             (s1',l')  = cStmJ s1 l
  SFor t i e' s     -> (e'' ++ [SPush (VInt 0)] ++
                        [SLabel (getLabel lu)] ++ [SDup2] ++ [SSwap] ++ [SALen] ++ [SCmp BEq (getLabel (lu+1))] ++
                        [SDup2] ++ [SBStart] ++ [SDec t' i'] ++ [SALoad t'] ++ [SStore t' i'] ++ s' ++ [SBEnd] ++
                        [SPush (VInt 1)] ++ [SAdd AbsJasm.TInt] ++ [SGoto (getLabel lu)] ++ [SLabel (getLabel (lu+1))], (lu+2))
                       where (s',lu)  = cStmJ s l'
                             (e'',l') = (cExpJ e' True l)
                             t'       = (cTypeJ t)
                             i'       = (cIdentJ i)
  SExp e            -> cExpJ e False l

cExpJ :: Exp -> Bool -> Int -> ([JStm], Int)
cExpJ et@(EType e t) sc l = case e of
  EApp i es     -> (es' ++ [SCall (cIdentJ i) (cTypeJ t) [cExpTypeJ exp | exp <- es]], l')
                   where (es', l') = (cPushArgsJ es l)
  EMul e1 op e2 -> case op of
                     OTimes -> (p ++ [SMul (cExpTypeJ e1)], l'')
                     ODiv   -> (p ++ [SDiv (cExpTypeJ e1)], l'')
                     OMod   -> (p ++ [SMod (cExpTypeJ e1)], l'')
                   where p = e1' ++ e2'
                         (e1', l'') = (cExpJ e1 sc l')
                         (e2', l')  = (cExpJ e2 sc l)
  EAdd e1 op e2 -> case op of
                     OPlus  -> (e1' ++ e2' ++ [SAdd (cExpTypeJ e1)], l'')
                     OMinus -> (e1' ++ e2' ++ [SSub (cExpTypeJ e1)], l'')
                   where (e1', l'') = (cExpJ e1 sc l')
                         (e2', l')  = (cExpJ e2 sc l)
  ERel e1 op e2 -> cRelJ et l
  EType e t     -> error $ "[cExpJ] Should not push a type to the stack"
  exp           -> case sc of
    True -> case exp of
      EVar i        -> ([SLoad (cTypeJ t) (cIdentJ i)], l)
      EInt n        -> ([SPush (VInt n)], l)
      EDbl d        -> ([SPush (VDbl d)], l)
      EString s     -> ([SPush (VString s)], l)
      ENew t' e     -> (e' ++ [SNewA (cArrayTypeJ t)], l')
                       where (e', l') = (cExpJ e sc l)
      ERef i e      -> ([SLoad (AbsJasm.TArray (cTypeJ t)) (cIdentJ i)] ++ e' ++ [SALoad (cTypeJ t)],l')
                       where (e', l') = (cExpJ e sc l)
      EALen i       -> ([SLoad (AbsJasm.TArray (cArrayTypeJ t)) (cIdentJ i)] ++ [SALen], l)
      ENeg e        -> case t of
                         AbsJavalette.TDbl -> ([SPush (VDbl (-1.0))] ++ e' ++ [SMul AbsJasm.TDbl], l')
                         _                 -> ([SPush (VInt (-1))] ++ e' ++ [SMul AbsJasm.TInt], l')
                       where (e', l') = (cExpJ e sc l)
      ENot e        -> ([SPush (VInt 1)] ++ e' ++ [SAdd AbsJasm.TInt] ++ [SPush (VInt (2))] ++ [SMod AbsJasm.TInt], l')
                       where (e', l') = (cExpJ e sc l)
      ETrue         -> ([SPush (VInt 1)],l)
      EFalse        -> ([SPush (VInt 0)],l)
      EAnd e1 e2    -> (e1' ++ [SCmp BEqZ (getLabel l'')] ++ [SPush (VInt 1)] ++ e2' ++ [SAnd] ++
                         [SGoto (getLabel (l''+1))] ++ [SLabel (getLabel l'')] ++ [SPush (VInt 0)] ++
                         [SLabel (getLabel (l''+1))], (l''+2))
                       where (e2',l'') = (cRelJ e2 l')
                             (e1',l')  = (cRelJ e1 l)
      EOr e1 e2     -> (e1' ++ [SCmp BNeqZ (getLabel l'')] ++ [SPush (VInt 0)] ++ e2' ++ [SOr] ++
                         [SGoto (getLabel (l''+1))] ++ [SLabel (getLabel l'')] ++ [SPush (VInt 1)] ++
                         [SLabel (getLabel (l''+1))], (l''+2))
                       where (e2',l'') = (cRelJ e2 l')
                             (e1',l')  = (cRelJ e1 l)
      g             -> error $ "[cExtJ] Unsupported node in AST: " ++ show g
    False -> ([], l)

cRelJ :: Exp -> Int -> ([JStm],Int)
cRelJ et@(EType e t) l = case e of
  ERel e1 op e2 -> case e1 of
                     (EType _ AbsJavalette.TBool) -> (p ++ [SCmp opI (getLabel lu)] ++ [SPush (VInt 0)] ++
                                                     [SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++ 
                                                     [SPush (VInt 1)] ++ [SLabel (getLabel (lu+1))],(lu+2))
                     (EType _ AbsJavalette.TInt) -> (p ++ [SCmp opI (getLabel lu)] ++ [SPush (VInt 0)] ++
                                                    [SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++ 
                                                    [SPush (VInt 1)] ++ [SLabel (getLabel (lu+1))],(lu+2))
                     (EType _ AbsJavalette.TDbl) -> case op of
                                                      OLt  -> (p ++ [SCmpD] ++ [SPush (VInt (-1))] ++ [SCmp BEq (getLabel lu)] ++
                                                                [SPush (VInt (0))] ++ [SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++
                                                                [SPush (VInt (1))] ++ [SLabel (getLabel (lu+1))],lu+2)
                                                      OLeq -> (p ++ [SCmpD] ++ [SPush (VInt (1))] ++ [SCmp BNeq (getLabel lu)] ++
                                                                [SPush (VInt (0))] ++ [SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++
                                                                [SPush (VInt (1))] ++ [SLabel (getLabel (lu+1))],lu+2)
                                                      OGt  -> (p ++ [SCmpD] ++ [SPush (VInt (1))] ++ [SCmp BEq (getLabel lu)] ++
                                                                [SPush (VInt (0))] ++ [SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++
                                                                [SPush (VInt (1))] ++ [SLabel (getLabel (lu+1))],lu+2)
                                                      OGeq -> (p ++ [SCmpD] ++ [SPush (VInt (-1))] ++ [SCmp BNeq (getLabel lu)] ++
                                                                [SPush (VInt (0))] ++ [SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++
                                                                [SPush (VInt (1))] ++ [SLabel (getLabel (lu+1))],lu+2)
                                                      OEq  -> (p ++ [SCmpD] ++ [SPush (VInt (0))] ++ [SCmp BEq (getLabel lu)] ++
                                                                [SPush (VInt (0))] ++ [SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++
                                                                [SPush (VInt (1))] ++ [SLabel (getLabel (lu+1))],lu+2)
                                                      ONEq -> (p ++ [SCmpD] ++ [SPush (VInt (0))] ++ [SCmp BNeq (getLabel lu)] ++
                                                                [SPush (VInt (0))] ++ [SGoto (getLabel (lu+1))] ++ [SLabel (getLabel lu)] ++
                                                                [SPush (VInt (1))] ++ [SLabel (getLabel (lu+1))],lu+2)
                  where p        = e1' ++ e2'
                        (e2',lu) = (cRelJ e2 l')
                        (e1',l') = (cRelJ e1 l)
                        opI      = case op of
                                     OLt  -> BLt
                                     OLeq -> BLeq
                                     OGt  -> BGt
                                     OGeq -> BGeq
                                     OEq  -> BEq
                                     ONEq -> BNeq
  _             -> cExpJ et True l

cDeclVars :: [Item] -> JType -> Int -> [JStm]
cDeclVars [] _ _       = []
cDeclVars (it:its) t l = case it of
  INoInit i -> case t of 
                 AbsJasm.TInt       -> [SPush (VInt 0)] ++ [SDec t i'] ++ s
                 AbsJasm.TDbl       -> [SPush (VDbl 0.0)] ++ [SDec t i'] ++ s  
                 (AbsJasm.TArray _) -> [SDec t i']
               where s = [SStore t i'] ++ (cDeclVars its t l)
                     i' = cIdentJ i
  IInit i e -> e' ++ [SDec t i'] ++ [SStore t i'] ++ (cDeclVars its t l)
               where (e', l) = (cExpJ e True l)
                     i' = cIdentJ i

cExpTypeJ :: Exp -> JType
cExpTypeJ (EType _ t) = cTypeJ t
cExpTypeJ e = error $ "[cExpTypeJ] not an EType, instead: " ++ show e

cArgsJ :: [Arg] -> [JArg]
cArgsJ []                          = []
cArgsJ ((AbsJavalette.Arg t i):as) = (AbsJasm.Arg (cTypeJ t) (cIdentJ i)):(cArgsJ as)

cPushArgsJ :: [Exp] -> Int -> ([JStm], Int)
cPushArgsJ [] l     = ([], l)
cPushArgsJ (e:es) l = (e' ++ es', l'')
                      where (es', l'') = (cPushArgsJ es l')
                            (e', l') = (cExpJ e True l)

cTypeJ :: Type -> JType
cTypeJ t = case t of
  AbsJavalette.TInt        -> AbsJasm.TInt
  AbsJavalette.TDbl        -> AbsJasm.TDbl
  AbsJavalette.TBool       -> AbsJasm.TInt
  AbsJavalette.TVoid       -> AbsJasm.TVoid
  (AbsJavalette.TArray t') -> AbsJasm.TArray (cTypeJ t')
  AbsJavalette.TString     -> AbsJasm.TString

cIdentJ :: AbsJavalette.Ident -> AbsJasm.Ident
cIdentJ (AbsJavalette.Ident s) = AbsJasm.Ident s

cArrayTypeJ :: Type -> JType
cArrayTypeJ t = case t of
  AbsJavalette.TArray t' -> cTypeJ t'
  _                      -> error $ "[cArrayTypeJ] Input type not array " ++ show t

getLabel :: Int -> String
getLabel i = "label" ++ (show i)

checkReturn :: [Stm] -> Bool
checkReturn []     = False
checkReturn (s:ss) = case s of
                       SRet _             -> True
                       SVRet              -> True
                       SBlock (Block ss') -> (checkReturn ss' || checkReturn ss)
                       _                  -> checkReturn ss


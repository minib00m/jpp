{-# LANGUAGE FlexibleContexts #-}
module Evaluator where
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits
import           Data.IORef
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust, isJust)
import           Structures
import           Text.PrettyPrint.HughesPJClass

funRetCell :: VarName
funRetCell = "$funRetCell$"

type MemCell = IORef ReturnValue

numFromMem :: ReturnValue -> Integer
numFromMem (MLit (NumLit l)) = l
numFromMem x = error $ "Develop error in numFromMem " ++ show x

boolFromMem :: ReturnValue -> Bool
boolFromMem (MLit (BoolLit l)) = l
boolFromMem x = error $ "Develop error in boolFromMem " ++ show x

funFromMem :: ReturnValue -> FunDef
funFromMem (MFun f) = f
funFromMem x = error $ "Develop error in funFromMem " ++ show x

funName :: FunDef -> VarName
funName (FunDef (FunHeader name _ _) _ _ _) = fromJust name

funArgs :: FunDef -> FunArgs
funArgs (FunDef (FunHeader _ fargs _) _ _ _) = fargs

type Memory = Map.Map VarName MemCell
type Eval a = ExceptT String (StateT Memory IO) a

runEval :: Memory -> Eval a -> IO (Either String a, Memory)
runEval startVal ev = runStateT (runExceptT ev) startVal

checkOut :: Program -> IO (Either String ReturnValue, Memory)
checkOut e = runEval Map.empty (evalProgram e)

processVariableList :: VarDefs -> Eval ()
processVariableList = mapM_ processList
  where
    processList (VarList vars) = mapM_ processVar vars
    processVar (Variable name val _) =
      if isJust val then do
        e <- evalExpr . fromJust $ val
        insertVar name e
      else
        insertVar name Void


loadFuns :: FunDefs -> Eval ()
loadFuns = mapM_ (liftM2 insertVar funName MFun)


loadArgs :: VarDefs -> [Expr] -> Eval ()
loadArgs vars args = mapM_ processList (zip (flattenVarList vars) args)
  where
    flattenVarList :: [VarList] -> [Variable]
    flattenVarList vs = mconcat [a | VarList a <- vs]
    processList :: (Variable, Expr) -> Eval ()
    processList (Variable name _ _, expr) = evalExpr expr >>= insertVar name


evalProgram :: Program -> Eval ReturnValue
evalProgram (Program _ vars funs stmt) = do
  processVariableList vars
  loadFuns funs
  evalStatement stmt
  return . MLit . NumLit $ 0 -- success


evalFun :: FunDef -> Eval ReturnValue
evalFun (FunDef _ vars funs stmt) = do
  processVariableList vars
  loadFuns funs
  evalStatement stmt
  liftIO . readIORef =<< getVar funRetCell


adjustVar :: (ReturnValue -> ReturnValue) -> VarName -> Eval ()
adjustVar f name = do
  mem <- get
  liftIO $ modifyIORef (mem Map.! name) f


insertVar :: VarName -> ReturnValue -> Eval ()
insertVar name val = do
  mem <- get
  newRef <- liftIO . newIORef $ val
  put $ Map.insert name newRef mem


insertArray :: VarName -> VarCall -> ReturnValue -> Eval ()
insertArray name vc val = do
  mem <- get
  case Map.lookup name mem of
    (Just v) -> do
      var <- liftIO . readIORef $ v
      newvar <- case var of
                  Void -> go vc (MArray Map.empty) val
                  _    -> go vc var val
      insertVar name newvar
    Nothing -> undefined
  where
    go :: VarCall -> ReturnValue -> ReturnValue -> Eval ReturnValue
    go (Single _) _ newval = return newval
    go (Arr next e) (MArray m) newval = do
      (MLit (NumLit idx)) <- evalExpr e
      newVal <- go next (m Map.! idx) newval
      return . MArray $ Map.insert idx newVal m
    go _ _ _ = error "go - insertArray"



getVar :: VarName -> Eval MemCell
getVar name = do
  mem <- get
  case Map.lookup name mem of
    (Just v) -> return v
    Nothing  -> throwError ("Function "++name++" returns no value")


printMemory :: Eval ()
printMemory = do
  mem <- get
  elems <- mapM (liftIO . readIORef) (Map.elems mem)
  let lst = zip (Map.keys mem) elems
  liftIO $ print (pPrint lst)


--------------STATEMENTS--------------
extractSingleName :: VarCall -> VarName
extractSingleName (Single name) = name
extractSingleName (Fun vc _) = extractSingleName vc
extractSingleName (Arr vc _) = extractSingleName vc

evalStatement :: Statement -> Eval ()
evalStatement (JustExpr e) = void $ evalExpr e
evalStatement (Seq s) = mapM_ evalStatement s

evalStatement a@(Assign lhs AMutate expr) = do
  let name = extractSingleName lhs
  e <- evalExpr expr
  case lhs of
    (Arr _ toRange) -> do
      (MLit (NumLit toIdx)) <- evalExpr toRange
      if toIdx < 0 then
        throwError ("index "++ show toIdx ++" out of bounds in " ++ render (pPrint a))
      else
        insertArray  name lhs e
    _ -> insertVar name e

evalStatement (Assign lhs op expr) = do
  let name = extractSingleName lhs
  e <- evalExpr expr
  case e of
    (MLit (NumLit val))  -> fNum (numAssignOp op val) name
    (MLit (BoolLit val)) -> fBool (boolAssignOp op val) name
    x                    -> throwError $ "Develop error in evalStatement assign " ++ show x
    where
      fNum f = adjustVar (\(MLit (NumLit l)) -> MLit . NumLit $ f l)
      fBool f = adjustVar (\(MLit (BoolLit l)) -> MLit . BoolLit $ f l)

evalStatement (If expr stmt elseStmt) = do
  bExpr <- evalExpr expr
  if boolFromMem bExpr then
    evalStatement stmt
  else
    evalStatement $ fromJust elseStmt


evalStatement (For (ForHeader startAssign expr changeAssign) stmt) = do
  evalStatement startAssign
  evalFor expr changeAssign stmt
  where
    evalFor expr' assign stmt' = do
      bExpr <- evalExpr expr'
      when (boolFromMem bExpr) $ do
        evalStatement stmt'
        evalStatement assign
        evalFor expr' assign stmt'


evalStatement w@(While expr stmt) = do
  bExpr <- evalExpr expr
  when (boolFromMem bExpr) $
    evalStatement stmt >> evalStatement w

evalStatement (BuiltInMethod (Print e)) = evalExpr e >>= liftIO . print . pPrint

evalStatement (Return e) = do
  val <- evalExpr e
  mem <- get
  case val of
    (MFun f) ->
      insertVar funRetCell (MLambda f mem)
    _        -> insertVar funRetCell val


--------------EXPRESSIONS--------------
evalExpr :: Expr -> Eval ReturnValue
evalExpr (Lit l) = return $ MLit l
evalExpr (Lambda f) = return $ MFun f
evalExpr (Array exprs) = do
  evaledExprs <- mapM evalExpr exprs
  return . MArray . Map.fromList $ zip [0..] evaledExprs
evalExpr (Var (Single name)) = getVar name >>= liftIO . readIORef
evalExpr (Var (Arr vc arg)) = do
  (MArray arr) <- evalExpr (Var vc)
  (MLit (NumLit evaledArg)) <- evalExpr arg
  case Map.lookup evaledArg arr of
    (Just x) -> return x
    Nothing -> throwError ("Array at index " ++ show evaledArg ++ " is uninitialized.")

evalExpr (Var (Fun vc args)) = do
  mem <- get
  retVal <- evalExpr (Var vc)
  case retVal of
    (MLambda f newMem) -> do
      let fargs = funArgs f
      loadArgs fargs args
      put $ Map.union (Map.difference newMem (argsToMap fargs)) mem
      evalFun f <* put mem
    (MFun f) -> do
      loadArgs (funArgs f) args
      evalFun f <* put mem
    _ -> undefined
  where
    argsToMap :: FunArgs -> Memory
    argsToMap fargs = Map.fromList $ zip (extractVarNames fargs) (repeat undefined)
    extractVarNames :: [VarList] -> [VarName]
    extractVarNames (VarList v:vs) = mappend (fmap (\(Variable name _ _) -> name) v) (extractVarNames vs)
    extractVarNames []             = []


evalExpr (Not expr) = MLit . BoolLit . not . boolFromMem <$> evalExpr expr
evalExpr (Negate expr) = MLit . NumLit . negate . numFromMem <$> evalExpr expr
evalExpr (NumExpr Divide e1 e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case e2' of
    (MLit (NumLit 0)) -> throwError "Divide by 0!"
    _ ->
      return . MLit . NumLit $ evalNumBinOp Divide (numFromMem e1') (numFromMem e2')
evalExpr (NumExpr op e1 e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  return . MLit . NumLit $ evalNumBinOp op (numFromMem e1') (numFromMem e2')

evalExpr (BoolExpr op e1 e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  return . MLit . BoolLit $ evalBoolBinOp op (boolFromMem e1') (boolFromMem e2')

evalExpr (RelExpr op e1 e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (MLit (NumLit val1), MLit (NumLit val2)) -> return . MLit . BoolLit $ evalRelBinOp op val1 val2
    (MLit (BoolLit val1), MLit (BoolLit val2)) -> return . MLit . BoolLit $ evalRelBinOp op val1 val2
    (Void, _) -> throwError $ "Uninitialized variable in " ++ show e1
    (_, Void) -> throwError $ "Uninitialized variable in " ++ show e2
    _         -> throwError "Development error in evalExpr RelExpr"


evalNumBinOp :: NumBinOp -> Integer -> Integer -> Integer
evalNumBinOp Plus     = (+)
evalNumBinOp Minus    = (-)
evalNumBinOp Multiply = (*)
evalNumBinOp Divide   = div
evalNumBinOp Power    = (^)

evalBoolBinOp :: BoolBinOp -> Bool -> Bool -> Bool
evalBoolBinOp And = (&&)
evalBoolBinOp Or  = (||)
evalBoolBinOp Xor = xor

evalRelBinOp :: Ord a => RelBinOp -> a -> a -> Bool
evalRelBinOp Equal = (==)
evalRelBinOp NotEqual = (/=)
evalRelBinOp Greater = (>)
evalRelBinOp GreaterEqual = (>=)
evalRelBinOp Less = (<)
evalRelBinOp LessEqual = (<=)

numAssignOp :: AssignOp -> Integer -> Integer -> Integer
numAssignOp AAdd v      = (+ v)
numAssignOp AMinus v    =  subtract v
numAssignOp AMultiply v = (* v)
numAssignOp ADivide v   = (`div` v)
numAssignOp APower v    = (^ v)
numAssignOp _ _         = error "Develop error in numAssignOp"

boolAssignOp :: AssignOp -> Bool -> Bool -> Bool
boolAssignOp AAnd v = (v .&.)
boolAssignOp AOr v  = (v .|.)
boolAssignOp AXor v = xor v
boolAssignOp _ _    = error "Develop error in boolAssignOp"

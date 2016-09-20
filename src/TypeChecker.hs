{-# LANGUAGE LambdaCase #-}
module TypeChecker where
import           Control.Monad.Except
import           Control.Monad.State
import           Data.IORef
import qualified Data.Map                       as Map
import           Data.Maybe
import           Structures                     hiding (MFun, ReturnValue)
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

activeScope :: VarName
activeScope = "$activeScope$"

data ReturnValue
  = MType VarType
  | MFun FunDef
  deriving (Show, Eq)

typeOfName :: VarName -> TCM VarType
typeOfName name = do
  var <- liftIO . readIORef =<< getVar name
  case var of
    (MType t) -> return t
    (MFun (FunDef (FunHeader _ _ t) _ _ _)) -> return t


getType :: ReturnValue -> VarType
getType (MType t)                               = t
getType (MFun (FunDef (FunHeader _ _ t) _ _ _)) = t


getActualFunType :: FunDef -> VarType
getActualFunType (FunDef (FunHeader _ args t) _ _ _) = TRec (typedArgs args) t
  where
    typedArgs :: FunArgs -> [VarType]
    typedArgs a = join $ mapM (fmap processVar) (fmap flattenArgsList (pure a))
    processVar :: Variable -> VarType
    processVar (Variable _ _ vt) = vt


isBasicType :: VarType -> Bool
isBasicType TInt  = True
isBasicType TBool = True
isBasicType _     = False

type MemCell = IORef ReturnValue

funName :: FunDef -> VarName
funName (FunDef (FunHeader name _ _) _ _ _) = fromJust name

adjustVar :: (ReturnValue -> ReturnValue) -> VarName -> TCM ()
adjustVar f name = do
  mem <- get
  liftIO $ modifyIORef (mem Map.! name) f

insertVar :: VarName -> ReturnValue -> TCM ()
insertVar name val = do
  mem <- get
  newRef <- liftIO . newIORef $ val
  put $ Map.insert name newRef mem


getVar :: VarName -> TCM MemCell
getVar name = do
  mem <- get
  case Map.lookup name mem of
    (Just v) -> return v
    Nothing -> throwError $ mappend "Nothing bound to " name


type Memory = Map.Map VarName MemCell
type TCM a = ExceptT String (StateT Memory IO) a

runTCM :: Memory -> TCM a -> IO (Either String a, Memory)
runTCM startVal ev = runStateT (runExceptT ev) startVal

goTC :: Program -> IO (Either String (), Memory)
goTC e = runTCM Map.empty (tcProgram e)


flattenArgsList :: FunArgs -> [Variable]
flattenArgsList vars = mconcat [a | VarList a <- vars]


tcVariableLists :: VarDefs -> TCM ()
tcVariableLists = mapM_ processList
  where
    processList (VarList vars) = mapM_ processVar vars
    processVar (Variable name expr t@(TArray _ _)) = do
      when (isJust expr) $
        getType <$> tcExpr (fromJust expr) >>= \case
          exprA@(TArray _ _) -> tcArray t exprA
          _ -> throwError "Given variable is not an array!"
      insertVar name (MType t)

    processVar (Variable name expr t) = do
      when (isJust expr) $ do
        typedExpr <- tcExpr (fromJust expr) >>= \case
          MFun f -> return $ getActualFunType f
          x -> return $ getType x
        when (typedExpr /= t && Any /= typedExpr) $
          throwError "Wrong initialization type"
      insertVar name (MType t)

    tcArray :: VarType -> VarType -> TCM ()
    tcArray (TArray t rangeTo) (TArray exprT exprRangeTo)
      | rangeTo < exprRangeTo =
        throwError "Initialized array is too long"
      | otherwise =
        case (t, exprT) of
          (TArray t' rangeTo', TArray exprT' exprRangeTo') ->
            if rangeTo' < exprRangeTo' then
              throwError "Initialization array is too long."
            else
              tcArray t' exprT'
          (x, y) -> unless (x == y) (wrongTypeError x y "Array initialization")
    tcArray t1 t2 =
      unless (t1 == t2) (wrongTypeError t1 t2 "Array initialization")


loadFuns :: FunDefs -> TCM ()
loadFuns = mapM_ (liftM2 insertVar funName (MType . getActualFunType))


tcArgsCall :: FunArgs -> [VarType] -> TCM ()
tcArgsCall fArgs args
  | length flatfArgs /= length args =
    throwError "Application to wrong number of arguments"
  | otherwise = zipWithM_ processArgs flatfArgs args
  where
    flatfArgs = flattenArgsList fArgs
    processArgs :: Variable -> VarType -> TCM ()
    processArgs (Variable _ _ varType) t =
      when (varType /= t) $ throwError "Wrong types passed to function"


tcProgram :: Program -> TCM ()
tcProgram (Program _ vars funs stmt) = do
  tcVariableLists vars
  loadFuns funs
  mem <- get
  mapM_ tcFun funs
  put mem
  tcStatement stmt

tcFun :: FunDef -> TCM ()
tcFun fdef@(FunDef (FunHeader _ fArgs _) vars funs stmt) = do
  tcVariableLists vars
  tcVariableLists fArgs
  loadFuns funs
  mem <- get
  mapM_ tcFun funs
  put mem
  insertVar activeScope (MFun fdef)
  tcStatement stmt
  put mem


--------------TYPECHECK STATEMENT--------------
extractSingleName :: VarCall -> VarName
extractSingleName (Single name) = name
extractSingleName (Fun vc _) = extractSingleName vc
extractSingleName (Arr vc _) = extractSingleName vc


tcStatement :: Statement -> TCM ()
tcStatement (JustExpr e) = void $ tcExpr e
tcStatement (Seq s) = mapM_ tcStatement s
tcStatement a@(Assign lhs AMutate expr) = do
  nameT <- typeOfName $ extractSingleName lhs
  let lhsType = case lhs of
                t@(Arr _ _) -> reduceArrType t nameT
                _ -> nameT
  rhsType <- tcExpr expr >>= \case
    MFun f  -> return $ getActualFunType f
    x -> return $ getType x

  case (lhsType, rhsType) of
    (t1@(TArray _ _), t2@(TArray _ _)) ->
      checkArrType t1 t2
    (x, y) ->
      when (x /= y && y /= Any) (wrongTypeError x y a)
  where
    checkArrType :: VarType -> VarType -> TCM ()
    checkArrType (TArray t1 toRange1) (TArray t2 toRange2) =
      if toRange1 < toRange2 then
        throwError "Initialization array is too long."
      else
        checkArrType t1 t2
    checkArrType x y = when (x /= y && y /= Any) (wrongTypeError x y "Array assignment")
    reduceArrType :: VarCall -> VarType -> VarType
    reduceArrType (Arr vc _) (TArray t _) = reduceArrType vc t
    reduceArrType (Single _) t = t
    reduceArrType _ _ = error "reduceArrType"

tcStatement a@(Assign lhs op expr) = do
  exprT <- getType <$> tcExpr expr
  valT <- typeOfName $ extractSingleName lhs
  unless (isBasicType valT) (throwError "Only := operator is allowed on functions!")
  when (exprT /= valT || (op `notElem` ops exprT))
      $ wrongTypeError exprT valT a
  where
    ops TInt = [AAdd, AMinus, AMultiply, ADivide, APower]
    ops TBool = [AAnd, AOr, AXor]
    ops _ = error "ops"

tcStatement (If expr stmt elseStmt) = do
  exprT <- getType <$> tcExpr expr
  when (exprT /= TBool) . void $ wrongTypeError TBool exprT expr
  tcStatement stmt
  forM_ elseStmt tcStatement

tcStatement (For (ForHeader startAssign expr changeAssign) stmt) = do
  tcStatement startAssign
  exprT <- getType <$> tcExpr expr
  when (exprT /= TBool) $ void (wrongTypeError TBool exprT expr)
  tcStatement changeAssign
  tcStatement stmt

tcStatement (While expr stmt) = do
  exprT <- getType <$> tcExpr expr
  when (exprT /= TBool) $ void (wrongTypeError TBool exprT expr)
  tcStatement stmt

tcStatement (BuiltInMethod (Print expr)) = void $ tcExpr expr

tcStatement r@(Return e) = do
  eType <- trans <$> tcExpr e
  scopeType <- typeOfName activeScope
  when (scopeType /= eType) (wrongTypeError eType scopeType r)
  where
    trans (MFun f) = getActualFunType f
    trans x = getType x


wrongTypeError :: Pretty b => VarType -> VarType -> b -> TCM a
wrongTypeError t1 t2 line = throwError . render $
  hcat [
    text "Couldn't match type ",
    pPrint t1,
    text " with type ",
    pPrint t2,
    text " in ",
    pPrint line
  ]


printMemory :: TCM ()
printMemory = do
  mem <- get
  elems <- mapM (liftIO . readIORef) (Map.elems mem)
  let lst = zip (Map.keys mem) elems
  liftIO $ print (pPrint lst)

instance Pretty ReturnValue where
  pPrint (MFun f) = pPrint f
  pPrint (MType t) = pPrint t


--------------TYPECHECK EXPRESSION--------------
tcExpr :: Expr -> TCM ReturnValue
tcExpr (Lit (NumLit _))    = return . MType $ TInt
tcExpr (Lambda f)          = tcFun f >> return (MFun f)
tcExpr (Array exs)         = do
  typedExprs <- mapM tcExpr exs
  let trimmedExprs = trimAny typedExprs
  if null trimmedExprs then
    return . MType $ Any
  else
    if all (comp (head trimmedExprs)) (tail trimmedExprs) then
      let th = case head trimmedExprs of
                MType (TArray _ _) -> getType (longestArray trimmedExprs)
                t@(MType _) -> getType t
                MFun f -> getActualFunType f
        in
          return . MType $ TArray th (toInteger . length $ typedExprs)
    else
      throwError ("Arrays can't be polymorphic " ++ render (pPrint typedExprs))
    where
      longestArray :: [ReturnValue] -> ReturnValue
      longestArray (r:rs) = go r rs
      longestArray [] = error "longestArray"
      go :: ReturnValue -> [ReturnValue] -> ReturnValue
      go l@(MType (TArray _ i1)) (r@(MType (TArray _ i2)):rs)
        | i1 < i2 = go r rs
        | otherwise = go l rs
      go l [] = l
      go l (MType Any:rs) = go l rs
      go _ _ = error "go - longestArray"
      trimAny :: [ReturnValue] -> [ReturnValue]
      trimAny (MType Any:rs) = trimAny rs
      trimAny rs = rs
      comp :: ReturnValue -> ReturnValue -> Bool
      comp base t =
        extractTypedLayer t == extractTypedLayer base
        || extractTypedLayer t == Any
      extractTypedLayer :: ReturnValue -> VarType
      extractTypedLayer (MType (TArray t _)) = t
      extractTypedLayer (MType x) = x
      extractTypedLayer _ = error "extractTypedLayer"
tcExpr (Lit (BoolLit _))   = return . MType $ TBool
tcExpr (Var (Single name)) = liftIO . readIORef =<< getVar name

tcExpr (Var v) = do
  mem <- get
  (MType fType) <- extractSingle v
  targs <- mapM (mapM (fmap getType . tcExpr)) (extractArgs v)

  applyCalls fType targs <* put mem
  where
    -- get arguments types passed to consecutive function calls
    -- eg. x(1 + 1)(2,true) gives [[1+1], [2, true]]
    --     x[1+1][2] gives [[1+1], [2]] for consistency
    extractSingle :: VarCall -> TCM ReturnValue
    extractSingle (Single name) = liftIO . readIORef =<< getVar name
    extractSingle (Fun vc _)    = extractSingle vc
    extractSingle (Arr vc _)    = extractSingle vc
    extractArgs :: VarCall -> [[Expr]]
    extractArgs = go []
      where
        go acc (Fun vc args) = go (args : acc) vc
        go acc (Single _)    = acc
        go acc (Arr vc arg)  = go ([arg] : acc) vc
    applyCalls :: VarType -> [[VarType]] -> TCM ReturnValue
    applyCalls t [] = return $ MType t
    applyCalls (TRec args t) (varL:varLs) =
      if args == varL then
        applyCalls t varLs
      else
        throwError ("Wrong types in function call! " ++ render (pPrint args) ++ " vs " ++ render (pPrint varL))
    applyCalls (TArray t _) (varL:varLs) =
      if varL == [TInt] then
        applyCalls t varLs
      else
        throwError "Arrays can be indexed only with integers"
    applyCalls _ _ = throwError "Too many calls (not a function or array!)"

tcExpr (Not expr)          = do
  exprT <- getType <$> tcExpr expr
  when (exprT /= TBool) $ void (wrongTypeError TBool exprT expr)
  return . MType $ TBool


tcExpr e@(Negate expr)     = do
  exprT <- getType <$> tcExpr expr
  when (exprT /= TInt) $ void (wrongTypeError TInt exprT e)
  return . MType $ TInt

tcExpr e@(NumExpr _ e1 e2) = do
  exprT1 <- getType <$> tcExpr e1
  exprT2 <- getType <$> tcExpr e2
  case (exprT1, exprT2) of
    (TInt, TInt) -> return . MType $ TInt
    (TInt, _)    -> wrongTypeError TInt exprT2 e
    _            -> wrongTypeError TInt exprT1 e

tcExpr e@(BoolExpr _ e1 e2) = do
  exprT1 <- getType <$> tcExpr e1
  exprT2 <- getType <$> tcExpr e2
  case (exprT1, exprT2) of
    (TBool, TBool) -> return . MType $ TBool
    (TBool, _)     -> wrongTypeError TBool exprT2 e
    _              -> wrongTypeError TBool exprT1 e

tcExpr e@(RelExpr _ e1 e2) = do
  exprT1 <- getType <$> tcExpr e1
  exprT2 <- getType <$> tcExpr e2
  if exprT1 == exprT2 then
    if isBasicType exprT1 then
      return . MType $ TBool
    else
      throwError $ render (pPrint exprT1) ++ " is not a basic type and can't be compared."
  else
    wrongTypeError exprT1 exprT2 e

module Gibbon.Passes.BoundCheck where
import           Gibbon.L2.Syntax
import           Gibbon.Common

calculateBounds :: Prog2 -> PassM Prog2
calculateBounds Prog { ddefs, fundefs, mainExp } = do
  fundefs' <- mapM calculateBoundsFun fundefs
  mainExp' <- case mainExp of
    Nothing       -> return Nothing
    Just (mn, ty) -> Just . (, ty) <$> calculateBoundsExp mn
  return $ Prog ddefs fundefs' mainExp'

calculateBoundsFun :: FunDef2 -> PassM FunDef2
calculateBoundsFun f@FunDef { funBody } = do
  funBody' <- calculateBoundsExp funBody
  return $ f { funBody = funBody' }

calculateBoundsExp :: Exp2 -> PassM Exp2
-- ? need type info too here? int 4 bytes, char 1 bytes, something like that?
calculateBoundsExp ex = case ex of
  VarE{}       -> return ex
  LitE{}       -> return ex
  FloatE{}     -> return ex
  LitSymE{}    -> return ex
  AppE{}       -> return ex
  PrimAppE{}   -> return ex
  DataConE{}   -> return ex
  ProjE{}      -> return ex
  IfE{}        -> return ex
  MkProdE{}    -> return ex
  LetE{}       -> return ex
  CaseE{}      -> return ex
  TimeIt{}     -> return ex
  SpawnE{}     -> pure ex
  SyncE{}      -> pure ex
  WithArenaE{} -> return ex
  MapE{}       -> return ex
  FoldE{}      -> return ex
  Ext ext      -> calculateBoundsExt ext

calculateBoundsExt :: Exp2 -> PassM Exp2
calculateBoundsExt ext = case ext of
  BenchE{}            -> return ext
  AddFixed{}          -> return ext
  LetRegionE{}        -> return ext
  LetParRegionE{}     -> return ext
  LetLocE{}           -> return ext
  RetE{}              -> return ext
  FromEndE{}          -> return ext
  BoundsCheck{}       -> return ext
  AddFixed{}          -> return ext
  IndirectionE{}      -> return ext
  GetCilkWorkerNum{}  -> return ext
  LetAvail{}          -> return ext
  ReadScalar{}        -> return ext
  WriteScalar{}       -> return ext
  ReadTag{}           -> return ext
  WriteTag{}          -> return ext
  ReadCursor{}        -> return ext
  WriteCursor{}       -> return ext
  ReadList{}          -> return ext
  WriteList{}         -> return ext
  ReadVector{}        -> return ext
  WriteVector{}       -> return ext
  AddCursor{}         -> return ext
  SubPtr{}            -> return ext
  NewBuffer{}         -> return ext
  ScopedBuffer{}      -> return ext
  NewParBuffer{}      -> return ext
  ScopedParBuffer{}   -> return ext
  InitSizeOfBuffer{}  -> return ext
  MMapFileSize{}      -> return ext
  SizeOfPacked{}      -> return ext
  SizeOfScalar{}      -> return ext
  BoundsCheck{}       -> return ext
  BumpRefCount{}      -> return ext
  BumpArenaRefCount{} -> return ext
  NullCursor{}        -> return ext
  RetE{}              -> return ext
  GetCilkWorkerNum{}  -> return ext
  LetAvail{}          -> return ext


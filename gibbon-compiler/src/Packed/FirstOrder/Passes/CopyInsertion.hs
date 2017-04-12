module Packed.FirstOrder.Passes.CopyInsertion
  ( addCopies, genCopyFn
  ) where

-- | standard library
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

-- | gibbon internals
import Packed.FirstOrder.Common
import Packed.FirstOrder.L2_Traverse as L2
import qualified Packed.FirstOrder.L1_Source as L1

-- | Chatter level for this module:
lvl :: Int
lvl = 4


-- | FIXME: This was the original intent;
-- Add calls to an implicitly-defined, polymorphic "copy" function,
-- of type `p -> p` that works on all packed data `p`.  A copy is
-- added every time constraints conflict disallowing an argument of
-- a data constructor to be unified with the needed output location.
--
-- But here, we're just generating call's to a copyDDef function,
-- for each Packed input argument in identity functions

addCopies :: L2.Prog -> SyM L1.Prog
addCopies p@L2.Prog{fundefs} = do
  let idFnNames = M.keys $ M.filter isId fundefs -- L2 fundefs
      (L1.Prog ddfs fndefs mnExp) = L2.revertToL1 p
      idFns = M.filterWithKey (\k _ -> k `elem` idFnNames) fndefs -- L1 fndefs

      go :: L1.FunDef L1.Ty Exp -> SyM (L1.FunDef L1.Ty Exp)
      go f@L1.FunDef{funArg=(arg,ty), funBody} =
        if L1.hasPacked ty
        then
          case ty of
            ProdTy tys      -> do
              let locs :: [(L1.Ty, Int)]
                  locs = filter (\(ty',_) -> isPacked ty') (zip tys [0..])

              newbod  <- foldrM (\(ty',l) acc -> do
                                    let dcon = tyToDataCon ty'
                                    x <- gensym $ toVar "x"
                                    return $ LetE (x, ty', AppE (mkCopyName $ toVar dcon) (ProjE l (VarE arg)))
                                             (L1.substE (ProjE l (VarE arg)) (VarE x) acc))
                         funBody locs
              return f{funBody = newbod}
            PackedTy dcon r -> do
              x <- gensym $ toVar "x"
              let newbod = LetE (x, PackedTy dcon r, AppE (mkCopyName $ toVar dcon) (VarE arg))
                           (L1.substE (VarE arg) (VarE x) funBody)
              return f{funBody = newbod}
            oth -> error $ "addCopies: handle " ++ show oth
        else return f

  copyFns <- mapM genCopyFn (M.elems ddfs)
  dbgTrace lvl ("\n[addCopies] Adding copy fn calls in :" ++ show idFnNames) return()
  fndefs' <- mapM go idFns
  let fndefs'' = M.unions [(M.fromList copyFns), fndefs', fndefs]
  return (L1.Prog ddfs fndefs'' mnExp)


-- | Generate a copy function for a data definition
genCopyFn :: DDef L1.Ty -> SyM (Var, L1.FunDef L1.Ty Exp)
genCopyFn DDef{tyName, dataCons} = do
  arg <- gensym $ toVar "arg"
  -- casebod :: [(DataCon, [Var], Exp)]
  casebod <- mapM (\(dcon, tys) -> do
                      xs <- mapM (\ty -> gensym (toVar "x")) tys
                      ys <- mapM (\ty -> gensym (toVar "y")) tys
                      let ys' = map VarE ys
                          exp = foldr (\(ty,x,y) acc ->
                                        if isPacked ty
                                        then LetE (y,ty,AppE (mkCopyName $ toVar $ tyToDataCon ty) (VarE x)) acc
                                        else LetE (y,ty,VarE x) acc)
                                (L1.MkPackedE dcon ys')
                                (zip3 tys xs ys)
                      return (dcon, xs, exp))
             dataCons
  return (mkCopyName tyName,
          L1.FunDef { funName = mkCopyName tyName
                    , funArg = (arg, L1.PackedTy (fromVar tyName) ())
                    , funRetTy = L1.PackedTy (fromVar tyName) ()
                    , funBody = L1.CaseE (L1.VarE arg) casebod
                    })

-- | A function is considered an identity if it has a type; Tree α -> Tree α
--
-- TODO(cskksc): Handle fns with complex types,
-- Eg: (Tree α, Tree β , Tree γ) -> Tree α
isId :: L2.FunDef -> Bool
isId L2.FunDef{funty=ArrowTy inT ef outT} =
  case inT of
    PackedTy _ _ -> inT == outT && S.null ef
    ProdTy tys   -> outT `elem` tys
    _            -> False

-- |
mkCopyName :: Var -> Var
mkCopyName v = varAppend (toVar "copy") v

-- |
isPacked :: L1.Ty -> Bool
isPacked PackedTy{} = True
isPacked _ = False

-- |
tyToDataCon :: L1.Ty -> DataCon
tyToDataCon (PackedTy dcon _) = dcon
tyToDataCon oth = error $ "tyToDataCon: " ++ show oth ++ " is not packed"

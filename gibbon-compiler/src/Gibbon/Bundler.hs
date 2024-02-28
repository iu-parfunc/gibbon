-- | Union all the modules in a program bundle in to a single program
module Gibbon.Bundler (bundleModules) where
import qualified Data.Foldable                   as F
import qualified Data.Set                        as S
import           Gibbon.L0.Syntax                as L0
import           Gibbon.Common
import           Data.Map                        as M



-- | Main bundler, runs all imported modules through a union that combines
-- their function defintions and data definitions with main's
-- Names should be globally unique at this point
bundleModules :: ProgBundle0 -> PassM Prog0
bundleModules bundle = do
    let (ProgBundle modules main) = bundle
    let (ProgModule main_name (Prog main_defs main_funs main_exp) main_imports) = main
    let (defs, funs) = F.foldr _bundleModule (main_defs, main_funs) modules
    return $ Prog defs funs main_exp

-- | Bundle fold function
-- builds the full program by folding definitons and functions into the main
_bundleModule :: ProgModule0 -> (DDefs0, FunDefs0) -> (DDefs0, FunDefs0)
_bundleModule (ProgModule mod_name (Prog {ddefs, fundefs}) _) (defs1, funs1) = 
    -- conflict checking,,, extract definition and function names
    let ddef_names1 = M.keysSet defs1
        ddef_names2 = M.keysSet ddefs
        fn_names1 = M.keysSet funs1
        fn_names2 = M.keysSet fundefs
        em1 = S.intersection ddef_names1 ddef_names2
        em2 = S.intersection fn_names1 fn_names2
        conflicts1 = F.foldr (\d acc ->
                    if (ddefs M.! d) /= (defs1 M.! d)
                        then d : acc
                        else acc)
                    [] em1
        conflicts2 = F.foldr (\f acc ->
                    if (fundefs M.! f) /= (funs1 M.! f)
                        then dbgTraceIt
                                (sdoc ((fundefs M.! f), (funs1 M.! f)))
                                (f : acc)
                        else acc)
                    [] em2
    in case (conflicts1, conflicts2) of
        ([], []) ->
            (M.union ddefs defs1, M.union fundefs funs1)
        (_x:_xs, _) ->
            error $
            "Conflicting definitions of " ++
            show conflicts1 ++ " found in " ++ mod_name
        (_, _x:_xs) ->
            error $
            "Conflicting definitions of " ++
            show (S.toList em2) ++ " found in " ++ mod_name

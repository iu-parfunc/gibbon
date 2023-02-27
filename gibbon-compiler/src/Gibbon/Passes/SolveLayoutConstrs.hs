module Gibbon.Passes.SolveLayoutConstrs (solveConstrs) where

import Gibbon.Common
import Gibbon.L1.Syntax

import           Control.Monad.State ( lift )
import qualified Data.List as L
import qualified Data.Set as S
import           Data.Maybe ( fromJust )
import           Data.Int
import           GHC.Float
import           System.Process
import           System.Random
import           System.Exit
import           System.IO
import Prelude as P

import qualified Language.Python.Common.AST as Py
import qualified Language.Python.Common.Pretty as Py
import qualified Language.Python.Common.PrettyAST as Py

--------------------------------------------------------------------------------

type Edge = ( Integer {- from -}
            , Integer {- to   -}
            )

type Constr = ( Edge    {- directed edge -}
              , Integer {- weight        -}
              )

-- new positions of fields
type Layout = [(Int,Int)]

-- change if required
python3 :: String
python3 = "python3.8"

solveConstrs :: [Constr] -> IO Layout
solveConstrs constrs = do
  let pycode = fst $ defaultRunPassM (pythonCodegen constrs)
  uniq <- randomIO :: IO Int8
  let fp = "./solve_ilp_" ++ show uniq ++ ".py"
  writeFile fp pycode
  let cmd = python3 ++ " " ++ fp
  (_,Just hout, Just herr, phandle) <- createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
  exitCode <- waitForProcess phandle
  case exitCode of
    ExitSuccess   -> do out <- hGetContents hout
                        let new_positions :: [(Int,Int)] = map ((\(x,y) -> (float2Int x, float2Int y))  . read) (lines out)
                        pure new_positions
                        --pure $ map (\((x,_),_) -> ( fromIntegral x
                        --                          , fromIntegral $ fromJust $ lookup (fromIntegral x) new_positions))
                        --           constrs
    ExitFailure n -> error $ "Running docplex program in file " ++ fp  ++ " exited with error code  " ++ show n

pythonCodegen :: [Constr] -> PassM String
pythonCodegen constrs = do
  let idxs = (L.nub $ concatMap ((\(a,b) -> [a,b]) . fst) constrs)
  let num_edges = P.length $ P.map fst constrs
  let (lb,ub) = (fromIntegral $ minimum idxs, fromIntegral $ maximum idxs) :: (Integer, Integer)
  node_map <- mapM (\i -> (i,) <$> fromVar <$> gensym (toVar ("x_" ++ show i))) idxs
  let node_vars = map snd node_map
  cost_map <- mapM (\i -> (i,) <$> fromVar <$> gensym (toVar "cost")) idxs
  let cost_vars = map snd cost_map
  let import_doplex = Py.FromImport (Py.ImportRelative 0 (Just [pyident "docplex", pyident "mp", pyident "model"]) ())
                                    (Py.FromItems [Py.FromItem (pyident "Model") Nothing ()] ()) ()
  model_var <- (\i -> "model_" ++ show i) <$> newUniq
  let init_model = Py.Assign [Py.Var (pyident model_var) ()] (Py.Call (Py.Var (pyident "Model") ()) [] ()) ()
  let init_nodes = map (\x -> pyassign1 x
                                        (Py.Call (Py.Dot (pyvar model_var) (pyident "integer_var") ())
                                                 [ Py.ArgKeyword (pyident "lb") (Py.Int lb (show lb) ()) ()
                                                 , Py.ArgKeyword (pyident "ub") (Py.Int ub (show ub) ()) ()
                                                 , Py.ArgKeyword (pyident "name") (Py.Strings [show x] ()) ()
                                                 ]
                                                 ()))
                       node_vars
  let init_costs = map (\x -> pyassign1 x
                                        (Py.Call (Py.Dot (pyvar model_var) (pyident "integer_var") ())
                                                 [Py.ArgKeyword (pyident "name") (Py.Strings [show x] ()) ()]
                                                 ()))
                       cost_vars
  minimize_parts <- mapM (\_ -> fromVar <$> gensym (toVar "min")) [num_edges]
  let constrs_for_edge = (\(((from,to), wt), cost, minimize_part) -> do
                            let x = fromJust $ lookup from node_map
                            let y = fromJust $ lookup to node_map
                            -- let cost_x = fromJust $ lookup from cost_map -- Vidush review
                            eq_minus_one <- fromVar <$> (gensym "cost")
                            leq_minus_one <- fromVar <$> (gensym "cost")
                            eq_one <- fromVar <$> (gensym "cost")
                            geq_one <- fromVar <$> (gensym "cost")
                            neq_minus_one <- fromVar <$> (gensym "cost")
                            neq_one <- fromVar <$> (gensym "cost")
                            x_minus_y <- fromVar <$> (gensym "x_minus_y")
                            pure $ [ pyassign1 x_minus_y (Py.Paren (Py.BinaryOp (Py.Minus ()) (pyvar x) (pyvar y) ()) ())
                                   , pyassign1 eq_minus_one (Py.Paren (Py.BinaryOp (Py.Equality ()) (pyvar x_minus_y) (Py.Int (-1) (show (-1)) ()) ()) ())
                                   , pyassign1 leq_minus_one (Py.Paren (Py.BinaryOp (Py.LessThanEquals ()) (pyvar x_minus_y) (Py.Int (-1) (show (-1)) ()) ()) ())
                                   , pyassign1 eq_one (Py.Paren (Py.BinaryOp (Py.Equality ()) (pyvar x_minus_y) (Py.Int 1 (show 1) ()) ()) ())
                                   , pyassign1 geq_one (Py.Paren (Py.BinaryOp (Py.GreaterThanEquals ()) (pyvar x_minus_y) (Py.Int 1 (show 1) ()) ()) ())
                                   , pyassign1 neq_minus_one (Py.Paren (Py.BinaryOp (Py.NotEquals ()) (pyvar x_minus_y) (Py.Int (-1) (show (-1)) ()) ()) ())
                                   , pyassign1 neq_one (Py.Paren (Py.BinaryOp (Py.NotEquals ()) (pyvar x_minus_y) (Py.Int 1 (show 1) ()) ()) ())
                                   , Py.StmtExpr (Py.Call (Py.Dot (pyvar model_var) (pyident "add") ())
                                                          [ Py.ArgExpr
                                                              (Py.BinaryOp (Py.LessThanEquals ())
                                                                           (pyvar eq_minus_one)
                                                                           (Py.Paren (Py.BinaryOp (Py.Equality ()) (pyvar cost) (Py.Int 0 (show 0) ()) ()) ())
                                                                           ())
                                                              ()
                                                          ]
                                                          ())
                                                 ()
                                   , Py.StmtExpr (Py.Call (Py.Dot (pyvar model_var) (pyident "add") ())
                                                          [ Py.ArgExpr
                                                              (Py.BinaryOp (Py.LessThanEquals ())
                                                                           (Py.Paren (Py.BinaryOp (Py.BinaryAnd ()) (pyvar leq_minus_one) (pyvar neq_minus_one) ()) ())
                                                                           (Py.Paren (Py.BinaryOp (Py.Equality ()) (pyvar cost) (Py.Int 100 (show 100) ()) ()) ())
                                                                           ())
                                                              ()
                                                          ]
                                                          ())
                                                 ()
                                   , Py.StmtExpr (Py.Call (Py.Dot (pyvar model_var) (pyident "add") ())
                                                          [ Py.ArgExpr
                                                              (Py.BinaryOp (Py.LessThanEquals ())
                                                                           (pyvar eq_one)
                                                                           (Py.Paren (Py.BinaryOp (Py.Equality ()) (pyvar cost) (Py.Int 200 (show 200) ()) ()) ())
                                                                           ())
                                                              ()
                                                          ]
                                                          ())
                                                 ()
                                   , Py.StmtExpr (Py.Call (Py.Dot (pyvar model_var) (pyident "add") ())
                                                          [ Py.ArgExpr
                                                              (Py.BinaryOp (Py.LessThanEquals ())
                                                                           (Py.Paren (Py.BinaryOp (Py.BinaryAnd ()) (pyvar geq_one) (pyvar neq_one) ()) ())
                                                                           (Py.Paren (Py.BinaryOp (Py.Equality ()) (pyvar cost) (Py.Int 300 (show 300) ()) ()) ())
                                                                           ())
                                                              ()
                                                          ]
                                                          ())
                                                 ()
                                   , pyassign1 minimize_part (Py.BinaryOp (Py.Multiply ()) (pyvar cost) (Py.Int wt (show wt) ()) ())
                                   ]) :: ((Constr, String, String) -> PassM [(Py.Statement ())])
  constrs_for_edges <- concat <$> mapM constrs_for_edge (zip3 constrs cost_vars minimize_parts)
  let expr_to_minimize = foldr (\x acc -> Py.BinaryOp (Py.Plus ()) (pyvar x) acc ()) (Py.Int 0 (show 0) ()) minimize_parts
  let call_minimize = Py.StmtExpr (Py.Call (Py.Dot (pyvar model_var) (pyident "minimize") ()) [(Py.ArgExpr expr_to_minimize ())] ()) ()
  let uniq_constrs = map (\[i,j] ->
                            Py.StmtExpr
                              (Py.Call (Py.Dot (pyvar model_var) (pyident "add") ())
                                       [Py.ArgExpr (Py.BinaryOp (Py.NotEquals ()) (pyvar i) (pyvar j) ()) ()]
                                       ())
                              ())
                         (combinations 2 node_vars)
  soln_var <- fromVar <$> (gensym "soln")
  let call_solve = pyassign1 soln_var (Py.Call (Py.Dot (pyvar model_var) (pyident "solve") ()) [] ())
  let answer_format_str = "({},{})"
  let print_left_bracket = Py.StmtExpr (Py.Call (pyvar "print") [Py.ArgExpr (Py.Strings [(show "[")] ()) ()] ()) ()
  let print_right_bracket = Py.StmtExpr (Py.Call (pyvar "print") [Py.ArgExpr (Py.Strings [(show "]")] ()) ()] ()) ()
  let print_answers = map (\(i,v) ->
                             Py.StmtExpr (Py.Call (pyvar "print")
                                                  [ (Py.ArgExpr (Py.Call (Py.Dot (Py.Strings [show answer_format_str] ()) (pyident "format") ())
                                                                         [ Py.ArgExpr (Py.Int i (show i) ()) ()
                                                                         , Py.ArgExpr
                                                                           (Py.Call (Py.Dot (pyvar soln_var) (pyident "get_value") ())
                                                                                    [ Py.ArgExpr (pyvar v) () ]
                                                                                    ())
                                                                           ()
                                                                         ]
                                                                         ())
                                                                ()) ]
                                                  ())
                                         ())
                          node_map
  let stmts = [ import_doplex, init_model ] ++ init_nodes ++ init_costs ++
              constrs_for_edges ++ uniq_constrs ++ [call_minimize, call_solve] ++ print_answers
  let pycode = Py.prettyText (Py.Module stmts)
  pure pycode

-- https://stackoverflow.com/a/52605612
combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==) . length) $ L.subsequences ns

--------------------------------------------------------------------------------

pyident :: String -> Py.Ident ()
pyident s = Py.Ident s ()

pyvar :: String -> Py.Expr ()
pyvar s = Py.Var (pyident s) ()

pyassign1 :: String -> Py.Expr () -> Py.Statement ()
pyassign1 v rhs = Py.Assign [pyvar v] rhs ()

input1 = [((0,1),80), ((0,2), 20), ((1,2), 20)]

run :: IO Layout
run = solveConstrs input1
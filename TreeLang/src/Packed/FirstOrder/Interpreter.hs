module Packed.FirstOrder.Interpreter where

--------------------------------------------------------------------------------

import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq, ViewL ((:<)), (|>))
import qualified Data.Sequence as Seq

import Packed.FirstOrder.Target

--------------------------------------------------------------------------------

data Val
  = FunVal FunDecl
  | IntVal Int
  | TagVal Tag
  | BufVal (Seq Int)
      -- ^ Tags are also written as integers.
  deriving (Eq, Show)

execProg :: Prog -> [Val]
execProg (Prog _ Nothing) = error "Can't evaluate program: No expression given"
execProg (Prog funs (Just expr)) = exec env expr
  where
    env = M.fromList (map (\f -> (funName f, FunVal f)) funs)

type Env = M.Map String Val

eval :: Env -> Triv -> Val
eval env (VarTriv v) = M.findWithDefault (error ("Unbound var: " ++ v)) v env
eval _   (IntTriv i) = IntVal i
eval _   (TagTriv t) = TagVal t

exec :: Env -> Tail -> [Val]

exec env (RetValsT ts) = map (eval env) ts

exec env (LetCallT binds op args body) =
    exec env' body
  where
    rets = apply env (eval env (VarTriv op)) (map (eval env) args)
    env' = extendEnv env (zip (map fst binds) rets)

exec env (LetPrimCallT binds op args body) =
    exec env' body
  where
    rets = applyOp op (map (eval env) args)
    env' = extendEnv env (zip (map fst binds) rets)

exec env (IfEqT v1 v2 then_ else_) =
    if v1' == v2' then exec env then_ else exec env else_
  where
    v1' = eval env (VarTriv v1)
    v2' = eval env (VarTriv v2)

exec _ ErrT =
    error "ErrT"

exec env (TimeT e) =
    -- We don't time in the interpreter
    exec env e

exec env (Switch tr alts def) =
    case final_alt of
      Nothing -> error "Switch: No branch to choose."
      Just br -> exec env br
  where
    tr' = eval env tr

    intAlts
      | IntAlts alts' <- alts
      = alts'
      | otherwise
      = error "intAlts: Found TagAlts"

    tagAlts
      | TagAlts alts' <- alts
      = alts'
      | otherwise
      = error "tagAlts: Found IntAlts"

    chooseIntAlt i = snd <$> listToMaybe (filter ((i ==) . fst) intAlts)
    chooseTagAlt t = snd <$> listToMaybe (filter ((t ==) . fst) tagAlts)

    final_alt =
      maybe def return $
        case tr' of
          IntVal i -> chooseIntAlt i
          TagVal t -> chooseTagAlt t
          _        -> error ("Switch: invalid value in scrutinee position: " ++ show tr')

exec env (TailCall fn args) =
    apply env fn' args'
  where
    fn' = eval env (VarTriv fn)
    args' = map (eval env) args

extendEnv :: Env -> [(String, Val)] -> Env
extendEnv = foldr (uncurry M.insert)

apply :: Env -> Val -> [Val] -> [Val]

apply env (FunVal (FunDecl _ as _ body)) args =
    exec (extendEnv env (zip (map fst as) args)) body

apply _ notFun _ =
    error ("apply to a non-function: " ++ show notFun)

--------------------------------------------------------------------------------

applyOp :: Prim -> [Val] -> [Val]

applyOp AddP [IntVal i1, IntVal i2] = [IntVal (i1 + i2)]
applyOp SubP [IntVal i1, IntVal i2] = [IntVal (i1 - i2)]
applyOp MulP [IntVal i1, IntVal i2] = [IntVal (i1 * i2)]

applyOp NewBuf [] = [BufVal Seq.empty]

applyOp WriteTag [TagVal tag, BufVal is] = [BufVal (is |> fromIntegral tag)]
applyOp WriteInt [IntVal i,   BufVal is] = [BufVal (is |> i)]

applyOp ReadTag [BufVal is] = case Seq.viewl is of
                                Seq.EmptyL -> error "ReadTag: Empty buffer"
                                t :< is'   -> [TagVal (fromIntegral t), BufVal is']

applyOp ReadInt [BufVal is] = case Seq.viewl is of
                                Seq.EmptyL -> error "ReadInt: Empty buffer"
                                i :< is'   -> [IntVal i, BufVal is']

applyOp op args = error ("applyOp: Unsupported form: " ++ show op ++ " " ++ show args)

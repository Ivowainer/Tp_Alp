module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple 
import qualified Data.Foldable as M


-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty 

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = 
    case M.lookup v s of
        Just val -> val
        Nothing -> error "La variable no fue declarada"
    

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm c s =
  case c of
    Skip -> error "Skip no tiene paso"
    Let v e ->
      let (n :!: s') = evalExp e s
      in Skip :!: update v n s'
    Seq Skip c2 ->
      c2 :!: s
    Seq c1 c2 ->
      let (c1' :!: s') = stepComm c1 s
      in Seq c1' c2 :!: s'
    IfThenElse b c1 c2 ->
      let (cond :!: s') = evalExp b s
      in if cond then c1 :!: s' else c2 :!: s'
    RepeatUntil comm b ->
      Seq comm (IfThenElse b Skip (RepeatUntil comm b)) :!: s


-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp e s =
    case e of
        Const v -> v :!: s
        Var v -> lookfor v s :!: s
        VarInc v -> 
            let value = lookfor v s
            in value+1 :!: update v (value+1) s
        VarDec v ->
            let value = lookfor v s
            in value-1 :!: update v (value-1) s
        UMinus ei -> 
            let (v :!: s') = evalExp ei s
            in (-v) :!: s'
        Plus ei1 ei2 ->
            let (v1 :!: s')  = evalExp ei1 s
                (v2 :!: s'') = evalExp ei2 s'
            in v1+v2 :!: s''
        Minus ei1 ei2 ->
            let (v1 :!: s')  = evalExp ei1 s
                (v2 :!: s'') = evalExp ei2 s'
            in v1-v2 :!: s''
        Times ei1 ei2 ->
            let (v1 :!: s')  = evalExp ei1 s
                (v2 :!: s'') = evalExp ei2 s'
            in v1*v2 :!: s''
        Div ei1 ei2 ->
            let (v1 :!: s')  = evalExp ei1 s
                (v2 :!: s'') = evalExp ei2 s'
            in v1 `div` v2 :!: s''
        -- Expresiones booleanas
        BTrue -> True:!:s
        BFalse -> False:!:s
        Lt ei1 ei2 -> 
            let (v1 :!: s')  = evalExp ei1 s
                (v2 :!: s'') = evalExp ei2 s'
            in v1<v2 :!: s''
        Gt ei1 ei2 -> 
            let (v1 :!: s')  = evalExp ei1 s
                (v2 :!: s'') = evalExp ei2 s'
            in v1>v2 :!: s'' 
        And eb1 eb2 ->
            let (v1 :!: s')  = evalExp eb1 s
                (v2 :!: s'') = evalExp eb2 s'
            in (v1 && v2) :!: s'' 
        Or eb1 eb2 ->
            let (v1 :!: s')  = evalExp eb1 s
                (v2 :!: s'') = evalExp eb2 s'
            in (v1 || v2) :!: s'' 
        Not eb1 ->
            let (v :!: s')  = evalExp eb1 s
            in not v :!: s' 
        Eq ei1 ei2 ->
            let (v1 :!: s')  = evalExp ei1 s
                (v2 :!: s'') = evalExp ei2 s'
            in v1==v2 :!: s'' 
        NEq ei1 ei2 ->
            let (v1 :!: s')  = evalExp ei1 s
                (v2 :!: s'') = evalExp ei2 s'
            in v1/=v2 :!: s'' 

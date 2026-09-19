module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = 
    let (state, _) = s
    in case M.lookup v state of
        Just val -> Right val
        Nothing -> Left UndefVar
    

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v i s = 
    let (state, trace) = s
    in (M.insert v i state, trace)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace t' (s,t) = (s, if null t then t' else t ++ " " ++ t')

-- Muestra una asignación ya evaluada en la traza, p.ej. "Let x 1"
trazaLet :: Variable -> Int -> String
trazaLet v n = "Let " ++ v ++ " " ++ show n        -- "Let x 1"

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do (c' :!: s') <- stepComm c s
                         stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm c s =
  case c of
    Skip -> error "Skip no tiene paso"
    Let v e ->
      do  (n :!: s') <- evalExp e s
          let s'' = addTrace (trazaLet v n) (update v n s')
          return (Skip :!: s'')
    Seq Skip c2 -> Right (c2 :!: s)
    Seq c1 c2 ->
      do  (c1' :!: s') <- stepComm c1 s
          return (Seq c1' c2 :!: s')
    IfThenElse b c1 c2 ->
      do  (cond :!: s') <- evalExp b s
          return (if cond then c1 :!: s' else c2 :!: s')
    RepeatUntil comm b -> Right (Seq comm (IfThenElse b Skip (RepeatUntil comm b)) :!: s)


-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp e s =
    case e of
        Const v -> Right (v :!: s)
        Var v -> 
            do  n <- lookfor v s 
                return (n :!: s)
        VarInc v -> 
            do  value <- lookfor v s
                return (value + 1 :!: update v (value+1) s)
        VarDec v ->
            do  value <- lookfor v s
                return (value-1 :!: update v (value-1) s)
        UMinus ei -> 
            do  (v :!: s') <- evalExp ei s
                return (-v :!: s')
        Plus ei1 ei2 ->
            do  (v1 :!: s')  <- evalExp ei1 s
                (v2 :!: s'') <- evalExp ei2 s'
                return (v1+v2 :!: s'')
        Minus ei1 ei2 ->
            do  (v1 :!: s')  <- evalExp ei1 s
                (v2 :!: s'') <- evalExp ei2 s'
                return (v1-v2 :!: s'')
        Times ei1 ei2 ->
            do  (v1 :!: s')  <- evalExp ei1 s
                (v2 :!: s'') <- evalExp ei2 s'
                return (v1*v2 :!: s'')
        Div ei1 ei2 ->
            do  (v1 :!: s')  <- evalExp ei1 s
                (v2 :!: s'') <- evalExp ei2 s'
                if v2 /= 0 then return ((v1 `div` v2) :!: s'') else Left DivByZero
        -- Expresiones booleanas
        BTrue -> Right (True:!:s)
        BFalse -> Right (False:!:s)
        Lt ei1 ei2 -> 
            do  (v1 :!: s')  <- evalExp ei1 s
                (v2 :!: s'') <- evalExp ei2 s'
                return (v1<v2 :!: s'')
        Gt ei1 ei2 -> 
            do  (v1 :!: s')  <- evalExp ei1 s
                (v2 :!: s'') <- evalExp ei2 s'
                return (v1>v2 :!: s'')
        And eb1 eb2 ->
            do  (v1 :!: s')  <- evalExp eb1 s
                (v2 :!: s'') <- evalExp eb2 s'
                return ((v1 && v2) :!: s'')
        Or eb1 eb2 ->
            do  (v1 :!: s')  <- evalExp eb1 s
                (v2 :!: s'') <- evalExp eb2 s'
                return ((v1 || v2) :!: s'')
        Not eb1 ->
            do  (v :!: s')  <- evalExp eb1 s
                return (not v :!: s' )
        Eq ei1 ei2 ->
            do  (v1 :!: s')  <- evalExp ei1 s
                (v2 :!: s'') <- evalExp ei2 s'
                return (v1==v2 :!: s'')
        NEq ei1 ei2 ->
            do  (v1 :!: s')  <- evalExp ei1 s
                (v2 :!: s'') <- evalExp ei2 s'
                return (v1/=v2 :!: s'')

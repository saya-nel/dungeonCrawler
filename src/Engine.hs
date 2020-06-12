module Engine where

import GameMap as GM
import System.Random as R
import Environnement as Env
import Entite as Ent
import Model as Mdl
import Keyboard as K
import Entite as Ent

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Debug.Trace as T
import Data.Maybe as Maybe


-- Etat du jeux
data State = Loose
            | Win
            | Round {
                round :: Int,
                map_round :: GM.GameMap,
                env_round :: Env.Environnement,
                gen_round :: StdGen,
                ent_round :: (M.Map Int Ent.Entite),
                log_round :: String
            }

-- créer un nouveau State
makeEngine :: Model -> State
makeEngine (Model gamemap env gen log kbd) = Round 0 gamemap env gen (setEntRound env) log

-- True si State respecte l'invariant, False sinon
--invariant : respecter les invariant de gamemap et environnement + ent_round et env_round doivent etre coherent
engineInv :: State -> Bool
engineInv state@(Round _ _ _ _ _ _) = gameMap_inv (map_round state) && environnementInv (env_round state)
   && (setEntRound (env_round state)) == (ent_round state)
engineInv Loose = True
engineInv Win = True

-- Extrait une map des entités disponibles a partir de l'environnement
setEntRound :: Environnement -> M.Map Int Entite
setEntRound (Environnement {content=env}) = M.fromList newL where
    newL = fmap (\ent -> ((Ent.id $ head(snd ent)),head(snd ent))) l where
        l = M.toList $ M.filter (/= []) env

setEntRound_pre :: Environnement -> Bool
setEntRound_pre env = environnementInv env

-- True si la partie est gagnée, False sinon
victory_conditions :: GameMap -> Environnement -> M.Map Int Ent.Entite -> Bool
victory_conditions map_rnd env ents = 
    let isjoueur = (\ent -> case ent of
                Joueur _ _ _ _ _-> True 
                otherwise -> False
                ) in 
    let fct = (\ent winner -> if (isjoueur ent) then 
            (findId (Ent.id ent) env) >>= (\(coord,ent) -> 
                if (checkCell (cx coord) (cy coord) map_rnd) == Exit && elem Treasure (inventory ent) then 
                    (||) <$> Just True <*> winner else 
                        (||) <$> Just False <*> winner) 
                        else winner) in

            case M.foldr fct ( Just False ) ents of
                Just winner -> winner
                Nothing -> False

-- True si la partie est perdue, False sinon
loose_conditions :: M.Map Int Ent.Entite -> Bool
loose_conditions ents = 
  let values = M.elems ents in 
  L.foldl (\acc entity -> case entity of 
                            Joueur _ _ _ _ _ -> False
                            _ -> acc
  ) True values


--fonction auxilliaire pour créer le nouvel état en fonction du modèle reçu
newState :: Int -> String -> Model -> State
newState round log_rnd (Model map env gen log _) = if victory_conditions map env (setEntRound env) then Win else if loose_conditions (setEntRound env) then Loose else Round (round+1) map env gen (setEntRound env) (log <> log_rnd)

newState_pre :: Int -> String -> Model -> Bool
newState_pre round log model = round > 0 && modelInv model

-- Renvoie la gamemap du State, si le jeux est finis, on renvoie une GameMap identité
getGameMap :: State -> GameMap
getGameMap (Round _ gamemap _ _ _ _ ) = gamemap
getGameMap _ = makeGameMap 0 0 (M.fromList [])

-- Pareil que getGameMap
getEnv :: State -> M.Map Coord [Entite]
getEnv (Round _ _ (Environnement {content=env}) _ _ _ ) = env
getEnv _ = M.fromList []

--Fonction principale du moteur, elle appel la fonction tour sur chacune des entités, et construit un nouvel état a partir du modèle reçu après
--que les tours de toute les entités soient finit
state_round :: State -> Keyboard -> Int -> State
state_round Loose _ _ = Loose
state_round Win _ _ = Win
state_round (Round rnd map_rnd env_rnd gen_rnd ents log_rnd) kbd nb_tours = 
  newState rnd log_rnd newModel2 where
  newModel = if nb_tours `mod` 30 == 0 then M.foldl cleanEntities model ents else model
  newModel2 = if nb_tours `mod` 30 /= 0 then M.foldl playerRound newModel ents else M.foldl mobsRound newModel ents  where
  -- fonction nettoyant toute les entités 
  cleanEntities = (\model ent -> cleanAttacked model ent)
  -- fonction jouant le tour du Joueur
  playerRound = (\model ent -> 
    case ent of 
      Joueur id _ _ _ _ -> if isJust $ findId id (env model) then Mdl.tour model ent else model
      otherwise -> model )
  -- fonction jouant le tour des mobs
  mobsRound = (\model ent -> 
      case ent of
          Zombie id _ _ -> if isJust $ findId id (env model) then Mdl.tour model ent else model
          Joueur id _ _ _ _ -> if isJust $ findId id (env model) then Mdl.tour model ent else model
          otherwise -> model)
  model = Model {gamemap=map_rnd,env=env_rnd,gen=gen_rnd,gamelog=(show rnd)++"\n"++"",keyboard=kbd} --le nouveau keyboard avec les events du tour courrant
  --a chaque tour on remplis un log qui contient juste le tour courant, qu'on va concatener au log_rnd general une fois les tours terminé

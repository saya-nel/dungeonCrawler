module Environnement where 

import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import qualified Debug.Trace as T


import GameMap as GM
import Entite as Ent


-- Environnement du jeux
newtype Environnement = Environnement {content :: Map.Map Coord [Entite]} deriving (Eq, Show)

-- Constructeur d'Environnement
makeEnvironnement :: Map.Map Coord [Entite] -> Environnement
makeEnvironnement = Environnement

-- Invariant de l'Environnement 
-- 1. Chaque Entite présente dans l'Environnement doit respecté l'invariant des Entite.
-- 2. Les entités doivent toute avoir des id différents (implique aussi qu'une meme entité 
--    ne peut pas etre dans l'env en double)
environnementInv :: Environnement -> Bool
environnementInv env =
  checkEntitiesInv && checkIds where
  checkEntitiesInv = 
    Map.foldr err_inv_l False (content env) == False
    where
      err_inv_l entities acc = 
        acc || (List.foldl (err_inv) acc entities)
      err_inv acc entity = 
        acc || not (Ent.entiteInv entity)
  checkIds =
    Map.foldr exist_double_l False (content env) == False
    where
      exist_double_l entities acc = 
        acc || (List.foldl (exist_double) acc entities)
      exist_double acc entity = 
        let new_env = removeId (Ent.id entity) env in
        acc || (isJust $ findId (Ent.id entity) new_env)


-- True si la case de GM.Coordonnées coord dans l'env est franchissable, false sinon
-- si la case n'existe pas = False
isPassable :: Coord -> Environnement -> Bool
isPassable coord env = 
  case Map.lookup coord (content env) of
    Just [] -> True
    _ -> False

-- Renvoie le couple (Coord, Entite) correspondant à la position de l'entite
-- d'id id dans l'environnement env si elle existe, sinon Nothing.
findId :: Int -> Environnement -> Maybe (Coord, Entite)
findId id env = 
  -- on récupère la map filtré avec la coord qui contient l'entité d'id id, on aura donc une map 
  -- avec un seul couple (k,v) ou une map vide, qu'on transforme en liste
  let l = Map.toList $ Map.filter isInside (content env) in
  case l of 
    [(k,(x:[]))] -> Just (k,x)
    [(k,v@(x:xs))] ->  let entite = Maybe.fromJust $ List.find (\e -> Ent.id e == id) v in Just (k, entite)
    _ -> Nothing
  where 
  -- True si l'entite d'id id est dans la liste l, False sinon
  isInside :: [Entite] -> Bool
  isInside l = List.any (\e -> Ent.id e == id) l

-- Renvoie l'environnement, ou l'entité d'id id à été supprimé de env
-- pre : l'entité d'id id doit existé dans env
removeId :: Int -> Environnement -> Environnement
removeId id env = 
  -- on recupere les coordonnées l'entite qu'on veut supprimer
  let (coord, _) = Maybe.fromJust $ findId id env in
  -- on recupere la liste de toute les entités a cette coordonnée
  let entities = (content env) Map.! coord in
  -- on supprime l'entite de la liste
  let new_list = List.filter (\e -> Ent.id (e) /= id) entities  in
  -- on supprime la valeur de coord dans env et on la remet avec la nouvelle liste
  let new_env = Environnement {content = Map.delete coord (content env)} in
  Environnement {content = Map.insert coord new_list (content new_env)}

-- verifie la précondition de removeId 
-- True si l'entité d'id id existe dans env, False sinon
removeIdPre :: Int -> Environnement -> Bool
removeIdPre id env = 
  case findId id env of
    Nothing -> False
    Just _ -> True

-- Deplace l'entité d'id id dans l'env aux nouvelles coordonnées coord
-- pre : l'entité d'id id doit existé dans env
-- pre : coord doit existé dans env
moveId :: Int -> Coord -> Environnement -> Environnement
moveId id coord env =
  -- on récupère l'entité à déplacé 
  
  let (_,entity) = Maybe.fromJust $ findId id env in
  -- on supprime l'entité de l'environnement
  let new_env =  removeId id env in
  -- on récupère la liste ou doit etre inséré l'entité
  let entities = (content new_env) Map.! coord in 
  -- on insère l'entite dans la liste 
  let new_entities = entity:entities in
  -- on supprime l'ancienne liste liée a coord et insère la nouvelle
  let new_env2 =  Environnement {content = Map.delete coord (content new_env)} in
   Environnement {content = Map.insert coord new_entities (content new_env2)}

-- Verifie la precondition de moveId
moveIdPre :: Int -> Coord -> Environnement -> Bool
moveIdPre id coord env = 
  let id_ok = (case findId id env of
                Nothing -> False
                Just _ -> True) in
  let coord_ok = (case Map.lookup coord (content env) of
                  Nothing -> False
                  Just _ -> True) in
  id_ok && coord_ok

-- Met à jour l'Entite entity en Entite newEntity
-- pre : l'entite entity doit existé dans env
-- pre : les id de entity et newEntity doivent etre egaux
updateEntity :: Entite -> Entite -> Environnement -> Environnement
updateEntity entity newEntity env =
  -- on recupère les coordonnées de l'entite à mettre à jour
  let (coord, _) = Maybe.fromJust $ findId (Ent.id entity) env in
  -- on supprime l'entite de l'environnement
  let new_env = removeId (Ent.id entity) env in
  -- on ajoute la nouvelle entite a l'environnement
  let entities = (content new_env) Map.! coord in
  let new_entities = newEntity:entities in
  let new_env2 = Environnement {content = Map.delete coord (content new_env)} in
  Environnement {content = Map.insert coord new_entities (content new_env2)}

-- verifie la pré-condition de updateEntity
updateEntityPre :: Entite -> Entite -> Environnement -> Bool
updateEntityPre entity newEntity env =
  let exist = removeIdPre (Ent.id entity) env in
  (Ent.id entity) == (Ent.id newEntity) && exist

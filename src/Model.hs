module Model where

import System.Random as R
import Data.Maybe as Maybe
import Data.List as L
import Data.Bool as B
import SDL
import qualified Debug.Trace as T

import GameMap as GM
import Entite as Ent
import Environnement as Env
import Keyboard as K
import Data.Map.Strict as M


-- Model du jeux
data Model = Model{ gamemap :: GameMap,
                    env :: Environnement,
                    gen :: StdGen,
                    gamelog :: String,
                    keyboard :: Keyboard
                   } deriving (Eq, Show)

instance Eq StdGen where 
  (==) g1 g2 = show g1 == show g2

data IDoor = Nord | Sud | Est | Ouest deriving (Show,Eq) --direction des portes
data Ordre = N | S | E | O | R | A | ID IDoor | IC | IS deriving (Show, Eq) -- ID = interact door, IC = interact chest, IS = interact sink

-- constructeur de Model
makeModel :: GameMap -> Environnement -> StdGen -> String -> Keyboard -> Model
makeModel = Model

-- Invariant du Model
-- Les invariant de GameMap et Environnement doivent etre respectés
-- L'ensemble des coordonnées de GameMap doit etre égal a l'ensemble des 
-- Coordonnées de Environnement
-- Min/max un trésor contenu dans un coffre
modelInv :: Model -> Bool
modelInv model = gameMap_inv (gamemap model) && environnementInv (env model) && M.keysSet (mapCells $ gamemap model) == M.keysSet (content $ env model) && treasure_prop model

-- invariant des tresors, il doit y avoir uniquement un trésor contenu dans un coffre
treasure_prop :: Model -> Bool
treasure_prop model@(Model (GameMap _ _ map) env gen log keyboard) = 
  (M.foldr (\cell cnt -> if cell == Chest Closed (Just Treasure) then (cnt+1) else cnt ) 0 map ) == 1

-- une entite a marché sur un piège, il reçoit des dégats
trapActivation :: Entite -> Coord -> GameMap -> Entite
trapActivation entity coord gm = let trapDamage = 3 in
  case checkCell (cx coord) (cy coord) gm of
    Trap Activated -> entity{stats = (stats entity){vie=(vie (stats entity)) - trapDamage}, attacked=True}
    otherwise -> entity

trapActivationPre :: Entite -> Coord -> GameMap -> Bool
trapActivationPre entity coord gm = getCell_pre (cx coord) (cy coord) gm

--une entite marche sur un portail, il se fait teleporter a l'autre portail
portalActivation :: Entite -> Coord -> GameMap -> Environnement -> Environnement
portalActivation entity coord gm env = 
  case checkCell (cx coord) (cy coord) gm of
    Portal Activated (Just otherside) -> moveId (Ent.id entity) otherside env
    otherwise -> env

portalActivationPre :: Entite -> Coord -> GameMap -> Environnement -> Bool
portalActivationPre entity coord gm env = moveIdPre (Ent.id entity) coord env

-- Bouge l'Entite à la Coord donnée sur le Model
-- pré : l'Entite doit existé dans env
-- pré : la Coord doit existé dans env
move :: Model -> Entite -> Coord -> Model
move (Model map env gen log keyboard) entity coord = 
  makeModel map newEnv gen newLog keyboard where
    newLog = log ++ show (Ent.id entity) ++ ": a bougé sur " ++ show coord ++ "\n"
    newEnv = portalActivation entity coord map newEnv' where
      newEnv' = if (vie $ stats newEntity) > 0 then updateEntity (entity) (newEntity) (moveId (Ent.id entity) coord env)
                else removeId (Ent.id newEntity) env where
                  newEntity = trapActivation entity coord map
      

-- verifie la précondition de move
-- True si l'Entite et la Coord existent dans l'Environnement du Model
movePre :: Model -> Entite -> Coord -> Bool
movePre model entity coord =
  moveIdPre (Ent.id entity) coord (env model)

-- Joue un tour pour une entité donnée
-- pré : l'Entite doit existé dans l'Environnement du Model
tour :: Model -> Entite -> Model
tour model entity@(Joueur id _ _ inv _) = 
  let possibleMoves = possibleSolutions model entity in
  let kb = (keyboard model) in
    
    -- on regarde quelle touche est appuyé et fait une action en conséquence
    if keypressed KeycodeZ kb then 
      if elem N possibleMoves then move model entity (northCoord coord) else model
    else if keypressed KeycodeS kb then 
      if elem S possibleMoves then move model entity (southCoord coord) else model
    else if keypressed KeycodeQ kb then 
      if  elem O possibleMoves then move model entity (ouestCoord coord) else model
    else if keypressed KeycodeD kb then 
      if elem Model.E possibleMoves then move model entity (estCoord coord) else model
    else if keypressed KeycodeSpace kb then
      interactWithSink (interactWithDoor (interactWithChest model id coord) coord) coord
    else if keypressed KeycodeP kb then
      let attackables = canAttack model entity in
      attack model entity attackables
    else model -- si aucune touche n'est appuyée, on ne fait rien
    where
      coord = fst $ Maybe.fromJust $ Env.findId (Ent.id entity) (env model)

tour model entity@(Zombie _ _ _) = 
  let l = provide model entity in
  decide model l entity

-- Verifie la pré-condition de tour
-- True si l'Entite existe dans l'Environnement du Model, False sinon
tourPre :: Model -> Entite -> Bool
tourPre model entity =
  removeIdPre (Ent.id entity) (env model)


-- On intéragis avec les leviers autour de coord
-- Pré : la coord existe dans la gamemap du model
interactWithSink :: Model -> Coord -> Model
interactWithSink model coord = 
  let gamemap = (Model.gamemap model) in 
  let ordres = canOpen model coord in
  let gmp1 = interactSink (ouestCoord coord) gamemap in 
  let gmp2 = interactSink (estCoord coord) gmp1 in 
  let gmp3 = interactSink (northCoord coord) gmp2 in 
  let gmp4 = interactSink (southCoord coord) gmp3 in 
  let new_gamemap = if elem IS ordres then gmp4 else gamemap in 
  model {gamemap = new_gamemap}

interactWithSinkPre :: Model -> Coord -> Bool
interactWithSinkPre model coord = checkCellPre (cx coord) (cy coord) (gamemap model)

-- On intéragis avec les coffres autour de coord
-- Pré : la coord existe dans la gamemap du model
interactWithChest :: Model -> Int -> Coord -> Model
interactWithChest model id coord =
  let gamemap = (Model.gamemap model) in
  let ordres = canOpen model coord in
  let currentEnv = (env model) in
  let (coord,joueur) = let res = findId id currentEnv  in case res of
          Just res ->  res
          Nothing -> error "bad id joueur interact chest" in
  let (new_joueur,new_gamemap) = if elem IC ordres then checkadjacent else (joueur,gamemap) where
        checkadjacent =
          let (loot1, gmp) = interactChest (ouestCoord coord) gamemap in
          let (loot2, gmp') = interactChest (estCoord coord) gmp in
          let (loot3, gmp) = interactChest (northCoord coord) gmp' in
          let (loot4, gmp') = interactChest (southCoord coord) gmp in

          (L.foldl (\j loot -> expandInventory loot j) joueur (loot1:loot2:loot3:loot4:[]),gmp') -- on étend l'inventaire avec les 4 contenus des 4 coffres
          in
  model {env = makeEnvironnement $ M.adjust (\lent -> [new_joueur]) coord (content currentEnv), gamemap = new_gamemap} 


interactWithChestPre :: Model -> Int -> Coord -> Bool
interactWithChestPre model id coord = case findId id (env model) of 
  Just (coord,(Joueur _ _ _ _ _)) -> checkCellPre (cx coord) (cy coord) (gamemap model) 
  Nothing -> False

-- On intéragit avec les portes autour de coord 
-- Pré : la Coord coord existe dans GameMap du Model
interactWithDoor :: Model -> Coord -> Model
interactWithDoor model coord = 
  let gamemap = (Model.gamemap model) in
  let ordres = canOpen model coord in
  model {gamemap = interactWithDoorAux coord ordres gamemap} where
  interactWithDoorAux :: Coord -> [Ordre] -> GameMap -> GameMap
  interactWithDoorAux cd@(Coord x y) [] gmp = gmp
  interactWithDoorAux cd@(Coord x y) (el:els) gmp = case el of
                                          ID Nord -> interactWithDoorAux cd els (interactDoor (Coord x (y-1)) gmp)
                                          ID Sud -> interactWithDoorAux cd els (interactDoor (Coord x (y+1)) gmp)
                                          ID Ouest -> interactWithDoorAux cd els (interactDoor (Coord (x-1) y) gmp)
                                          ID Est -> interactWithDoorAux cd els (interactDoor (Coord (x+1) y) gmp)
                                          _ -> gmp

-- Verifie la pré-condition de interactWithDoor
interactWithDoorPre :: Model -> Coord -> Bool
interactWithDoorPre model coord =
  checkCellPre (cx coord) (cy coord) (gamemap model)


-- Renvoie la liste des cellules adjcentes franchissables
-- par ex si on peut aller seulement au nord et à l'est, on aura : [N,E]
-- pré : l'Entite doit existé dans l'Environnement du Model
canMoveTo :: Model -> Entite -> [Ordre]
canMoveTo model entity  = 
  let gameMap = Model.gamemap model in
  let env = Model.env model in
  let coord = fst $ Maybe.fromJust $ findId (Ent.id entity) env in
  -- pour chaque case on regarde si on peut la franchir dans l'environnement et dans la carte
  let topCell = Env.isPassable (northCoord coord) env && (GM.cellCrossable entity $ GM.checkCell (cx $ northCoord coord) (cy $ northCoord coord) gameMap) in
  let bottomCell = Env.isPassable (southCoord coord) env && (GM.cellCrossable entity $ GM.checkCell (cx $ southCoord coord) (cy $ southCoord coord) gameMap) in
  let leftCell =  Env.isPassable (ouestCoord coord) env && (GM.cellCrossable entity $ GM.checkCell (cx $ ouestCoord coord) (cy $ ouestCoord coord) gameMap) in
  let rightCell = Env.isPassable (estCoord coord) env && (GM.cellCrossable entity $ GM.checkCell (cx $ estCoord coord) (cy $ estCoord coord) gameMap) in
  -- si la case est franchissable, on l'ajoute dans la liste
  (bool [] [N] topCell)++(bool [] [S] bottomCell)++(bool [] [O] leftCell)++(bool [] [Model.E] rightCell)++[]

-- Vérifie la pré-condition de canMoveTo
-- True si l'Entite existe dans l'Environnement du Model, False sinon
canMoveToPre :: Model -> Entite -> Bool
canMoveToPre model entity = 
  removeIdPre (Ent.id entity) (env model)


-- Donne la liste des cases adjacentes sur lesquels on peut appliquer un Ordre d'interaction
-- pré : La cellule de Coord coord doit existé dans GameMap du Model
canOpen :: Model -> Coord -> [Ordre]
canOpen model coord = L.map sinkMapping (L.map chestMapping (L.map doorMapping initOrders)) L.\\ initOrders where
  doorMapping x = case x of
    N -> if isDoor $ (checkCell (cx $ northCoord coord) (cy $ northCoord coord) (gamemap model)) then ID Nord else x
    S -> if isDoor $ (checkCell (cx $ southCoord coord) (cy $ southCoord coord) (gamemap model)) then ID Sud  else x
    O -> if isDoor $ (checkCell (cx $ ouestCoord coord) (cy $ ouestCoord coord) (gamemap model)) then ID Ouest else x
    Model.E -> if isDoor $ (checkCell (cx $ estCoord coord) (cy $ estCoord coord) (gamemap model)) then ID Est else x
    _ -> x
  chestMapping x = case x of
    N -> if isChest $ (checkCell (cx $ northCoord coord) (cy $ northCoord coord) (gamemap model)) then IC else x
    S -> if isChest $ (checkCell (cx $ southCoord coord) (cy $ southCoord coord) (gamemap model)) then IC else x
    O -> if isChest $ (checkCell (cx $ ouestCoord coord) (cy $ ouestCoord coord) (gamemap model)) then IC else x
    Model.E -> if isChest $ (checkCell (cx $ estCoord coord) (cy $ estCoord coord) (gamemap model)) then IC else x
    _ -> x
  sinkMapping x = case x of
    N -> if isSink $ (checkCell (cx $ northCoord coord) (cy $ northCoord coord) (gamemap model)) then IS else x
    S -> if isSink $ (checkCell (cx $ southCoord coord) (cy $ southCoord coord) (gamemap model)) then IS else x
    O -> if isSink $ (checkCell (cx $ ouestCoord coord) (cy $ ouestCoord coord) (gamemap model)) then IS else x
    Model.E -> if isSink $ (checkCell (cx $ estCoord coord) (cy $ estCoord coord) (gamemap model)) then IS else x
    _ -> x
  initOrders = [ N ,S ,O , Model.E ]

-- Vérifie la pré-condition de canOpen 
-- True si la cellule de Coord coord existe dans GameMap, False sinon
canOpenPre :: Model -> Coord -> Bool
canOpenPre model coord =
  checkCellPre (cx coord) (cy coord) (gamemap model)

-- Renvoie la liste des entites attaquables autours de l'entite attaquante
-- pré : l'Entité doit existé dans l'environnement du model
canAttack :: Model -> Entite -> [Entite]
canAttack model entity =
  let playerCoord = fst $ Maybe.fromJust $ findId (Ent.id entity) (env model) in
  -- on récupères les entités sur les cases adjacentes
  let coordsArround = [northCoord playerCoord, southCoord playerCoord, ouestCoord playerCoord, estCoord playerCoord] in
  let entitiesArround = L.map (\coord -> M.lookup coord (content $ env model)) coordsArround in
  -- on parcours les listes d'entités sur les cases adjacente et renvoie celles attaquables
  -- puis on fusionne toute les listes en une seule liste d'entités attaquables
  concat $ L.map 
    (\maybe_entities -> 
      case maybe_entities of 
        Nothing -> []
        Just entities -> 
          case entity of 
            Joueur _ _ _ _ _-> 
              L.filter
                (\entity -> case entity of 
                  Zombie _ _ _ -> True
                  _ -> False
                )
                entities
            Zombie _ _ _ ->
              L.filter
                (\entity -> case entity of
                  Joueur _ _ _ _ _ -> True
                  _ -> False  
                )
                entities
    )
  entitiesArround

canAttackPre :: Model -> Entite -> Bool
canAttackPre model entity =
  canMoveToPre model entity

-- L'Entite entity attaque les entites de la liste
-- pré : l'Entité doit existé dans le model
-- pré : la liste des entités attaquables doit correspondre au resultat de canAttack
attack :: Model -> Entite -> [Entite] -> Model
attack model entity [] = model
attack model entity entities@(t:q) = 
  let damages = case entity of 
                  Zombie _ stats _ -> atk stats
                  Joueur _ stats _ _ Unarmed -> atk stats
                  Joueur _ stats _ _ (Armed bonus) -> (atk stats) + bonus
                  _ -> 0 in 
  -- on décremente les vies, si il n'y en a plus on renvoie nothing
  let new_entity = case t of 
                      Zombie id stats _ -> if ((vie stats) - damages) > 0 then Just $ makeZombie id (Stats ((vie stats) - damages) (atk stats)) True else Nothing
                      Joueur id stats _ inventory state -> if ((vie stats) - damages) > 0 then Just $ makeJoueur id (Stats ((vie stats) - damages) (atk stats)) True inventory state else Nothing
  in 
  case new_entity of
    Nothing -> 
      -- l'entité n'a plus de vie, on la détruit
      let removed_env = Env.removeId (Ent.id t) (env model) in
      attack (model{env = removed_env}) entity q
    Just ent -> 
      -- l'entité à encore de la vie, on la met a jour
      let new_env = updateEntity t ent (env model) in
      attack (model {env = new_env}) entity q

-- verifie les pré-conditions de attack
attackPre :: Model -> Entite -> [Entite] -> Bool
attackPre model entity entities =
  let exist = canMoveToPre model entity in
  exist && ((canAttack model entity) == entities)

-- passe l'attribut attacked de l'Entite à false si nécéssaire
-- pré : l'Entite doit existé dans model
cleanAttacked :: Model -> Entite -> Model
cleanAttacked model entity =
  model{env=updateEntity entity entity{attacked=False} (env model)}

cleanAttackedPre :: Model -> Entite -> Bool
cleanAttackedPre model entity = 
  canMoveToPre model entity

-- Donne une liste d'actions possibles pour une Entite
-- pré : l'Entite doit existé dans l'Environnement du Model
possibleSolutions :: Model -> Entite -> [Ordre]
possibleSolutions model entity =
  -- on récupère la position de l'entite
  let coord = fst $ Maybe.fromJust $ Env.findId (Ent.id entity) (env model) in
  -- on récupère la liste des directions où on peut aller
  let movements = canMoveTo model entity in
  -- ici on voit si il y'a des elements qu'on peut ouvrir (coffres, portes)
  let res = canOpen model coord in
    -- on ajoute les autres possibilités propre à chaque entité
  case entity of 
    Zombie _ _ _ -> movements++[R]
    Joueur _ _ _ _ _-> 
      let jCanAttack = if (canAttack model entity) == [] then [] else [A] in
      jCanAttack++res++movements++[R]

possibleSolutionsPre :: Model -> Entite -> Bool
possibleSolutionsPre model entity =
  removeIdPre (Ent.id entity) (env model) 

-- Si un joueur est dans le rayon de detection de l'entite, renvoie l'ordre à
-- executer pour s'en rapprocher, sinon renvoie nothing
-- pré : l'entité doit existé dans l'Environnement du Model
playerIsNear :: Model -> Entite -> Maybe Ordre
playerIsNear model zomb@(Zombie id _ _) = 
  -- coordonnées du zombie
  let coord = fst $ fromJust $ Env.findId id (env model) in
  -- on récupère la distance jusqu'a un joueur pour les quatre cases autours 
  let northDist = tryApply 0 (northCoord coord) in 
  let southDist = tryApply 0 (southCoord coord) in
  let ouestDist = tryApply 0 (ouestCoord coord) in
  let estDist = tryApply 0 (estCoord coord) in
  -- on recupere la distance minimum
  let minDist = L.minimum [northDist, southDist, ouestDist, estDist] in
  -- si la distance minimum est supérieur a 3 (champ de vision du zombie, on renvoie nothing)
  if minDist > 3 then Nothing
  -- sinon, on renvoie l'Ordre a effectué pour aller vers cette distance minimum
  else 
    if northDist == minDist then Just N
    else if southDist == minDist then Just S
    else if ouestDist == minDist then Just O
    else if estDist == minDist then Just Model.E
    else Nothing
  where
  -- fonction recursive de l'algo, se rappele sur les case adjacentes tans que la range n'est pas dépassé 
  -- on applique cette fonction via tryApply pour vérifié que les conditions nécéssaires sont vérifiés avant application
  -- si la fonction n'est pas applicable, une valeure par defaut sera renvoyer
  detect :: Int -> Coord -> Int
  detect range coord = 
    if range > 3 then invalidDist
    else
      -- on regarde si la case actuelle contient un joueur
      let isPlayer = containsPlayer coord (env model) in 
      if isPlayer then range -- si la case contient un joueur, on renvoie la range actuelle
      else -- sinon, on cherche recursivement sur les cases franchissables et renvoie le min qu'on obtient (plus petite distance a parcourir)
        let results = [tryApply range (northCoord coord), tryApply range (southCoord coord), tryApply range (ouestCoord coord), tryApply range (estCoord coord)] in
        L.minimum results
  
  invalidDist = 1000000
  -- Essaie d'appliqué l'algorithme si appliquable sur la case coord, sinon retourne invalidDist
  tryApply :: Int -> Coord -> Int 
  tryApply range coord =
    if canApply coord then detect (range + 1) coord
    else invalidDist
  -- True si la case contient un joueur ou est vide + est traversable 
  canApply :: Coord -> Bool
  canApply coord =
    (containsPlayer coord $ env model) || ((Env.isPassable coord $ env model) && cellCrossable zomb (checkCell (cx coord) (cy coord) (gamemap model))) 
  -- True si la case contient un joueur, false sinon
  containsPlayer :: Coord -> Environnement -> Bool
  containsPlayer coord env =
    if isJust $ M.lookup coord (content env)  then
      L.any (\entity -> case entity of 
                          Joueur _ _ _ _ _ -> True
                          _ -> False)
      ((content env) M.! coord)
    else False


playerIsNear _ _ = Nothing

-- verifie la précondition de playerIsNear
playerIsNearPre :: Model -> Entite -> Bool
playerIsNearPre model entity =
  removeIdPre (Ent.id entity) (env model) 

-- Donne une liste pondérée d'actions possibles pour une Entite autonome
-- l'Entite doit existé dans l'Environnement du model
provide :: Model -> Entite -> [(Int, Ordre)]
provide model zomb@(Zombie _ _ _) = 
  let l = possibleSolutions model zomb in 
  -- si le Zombie peut attaquer
  if (canAttack model zomb) /= [] then [(100, A)]
  -- si un Joueur est dans le champ de detection du Zombie
  else if isJust $ playerIsNear model zomb then [(100, fromJust $ playerIsNear model zomb)]
  -- sinon, comportement par defaut
  else aux 0 l
  where
    aux :: Int -> [Ordre] -> [(Int, Ordre)]
    aux acc [] = []
    aux acc (t:q) = 
      case t of  
        R -> (100 - acc, R):(aux 100 q) 
        otherwise -> (20, t):(aux (acc + 20) q)

provide model entity = [(100,R)]

-- Verifie la pré-condition de provide
-- True si l'Entite existe dans l'Environnement du Model, False sinon
providePre :: Model -> Entite -> Bool
providePre model entity =
  removeIdPre (Ent.id entity) (env model)

-- Choisit une action à effectué pour une Entite autonome et l'effectue
-- pré : l'Entite doit existé dans l'Environnement du Model
-- pré : la liste doit contenir au moins un Ordre
decide ::  Model -> [(Int, Ordre)] -> Entite -> Model
decide model l entity = 
  -- on tire un nombre aleatoire entre 1 et 100
  let (val, g) = randomR (1, 100) (gen model) in
  let order = takeElem l val in
  -- on applique l'ordre
  let new_model = case order of 
                    N -> move model entity (northCoord coord)
                    S -> move model entity (southCoord coord)
                    O -> move model entity (ouestCoord coord)
                    Model.E -> move model entity (estCoord coord)
                    A -> 
                      let zCanAttack = canAttack model entity in
                      attack model entity zCanAttack
                    _ -> model
                    in
    -- on recrée un model avec le nouveau générateur
    makeModel (gamemap new_model) (env new_model) g (gamelog new_model) (keyboard new_model)
  where 
    -- coordonnées actuelle de l'entité 
    coord = fst $ fromJust $ findId (Ent.id entity) (env model)
    -- récupère un Ordre dans une liste pondéré d'Ordre associés à un pourcentage (poids)
    takeElem :: [(Int, Ordre)] -> Int -> Ordre
    takeElem ((freq, e):[]) val = e
    takeElem ((freq, e):q) val 
      | val <= freq = e
      | otherwise = takeElem q (val - freq)

-- Verifié la pré-condition de decide
-- True si l'Entite existe dans le Model et que la liste contient au moins un Ordre, False sinon
decidePre :: Model -> [(Int, Ordre)] -> Entite -> Bool
decidePre _ [] _ = False
decidePre model l entity =
  removeIdPre (Ent.id entity) (env model)

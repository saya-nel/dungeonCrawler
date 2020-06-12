module GameMap where

import qualified Data.Map.Strict as M
import qualified Debug.Trace as T
import Entite
import qualified Entite as E

-- Type Coord 
data Coord = Coord {cx :: Int, cy :: Int} deriving Eq

instance Ord Coord where
    (<=) c1@(Coord {cx = x1,cy = y1}) c2@(Coord {cx = x2,cy = y2}) = (x1 <= x2 && y1 <= y2) || (x1 > x2) && (y1 < y2)

instance Show Coord where
    show (Coord {cx=x,cy=y}) = show (x,y)


data DoorDirection = NS | EO deriving Eq
data CloseableState = Opened | Closed deriving Eq
data ActionableState = Activated | Desactivated deriving Eq

-- Statiquement, sur une carte, un levier est connecté a aucun piège,
-- ce travail est fait dynamiquement a l'initialisation du jeu
-- Ensuite chaque levier contient les coordonnées du piège connecté a lui
type LinkedTo = Either (Coord,Coord) Coord  -- Un levier peut etre associé a un trap, ou a deux portails de teleportation
type OtherSide = Maybe Coord -- l'autre extermité du teleporteur

-- cellule de la GameMap
data Cell = Empty
    | Door DoorDirection CloseableState
    | Chest CloseableState (Maybe Entite)
    | Wall
    | Entrance
    | Exit
    | Portal ActionableState OtherSide
    | Trap ActionableState
    | Sink ActionableState (Maybe LinkedTo)
    deriving Eq

instance Show Cell where
    show Empty = " "
    show (Door NS Closed ) = "|"
    show (Door EO Closed ) = "-"
    show (Door NS Opened ) = "|'"
    show (Door EO Opened ) = "-'"
    show Wall = "X"
    show Entrance = "E"
    show Exit = "S"
    show (Chest Opened _) = "C'"
    show (Chest Closed _) = "C"
    show (Trap Activated) = "T"
    show (Trap Desactivated) = "T'"
    show (Sink Desactivated _) = "L"
    show (Sink Activated _) = "L'"
    show (Portal Desactivated _) = "P"
    show (Portal Activated _) = "P'"


-- La GameMap du jeux
data GameMap = GameMap {mapw :: Int,
                        maph :: Int,
                        mapCells :: (M.Map Coord Cell)} deriving Eq

instance Show GameMap where
    show = gameMapShow

gameMapShow :: GameMap -> String
gameMapShow gm@(GameMap {mapw = mw,maph = mh ,mapCells = mc}) = M.foldrWithKey fct (mempty :: String) mc where
    fct = \g@(Coord{cx = x,cy=y}) cell mnd -> if x == (mw -1) && y < (mh -1) then (show cell) <> "\n" <> mnd else (show cell) <> mnd


instance Read GameMap where
    readsPrec _ x = [(gameMapRead x,"")]


gameMapRead :: String -> GameMap
gameMapRead content = gameMapReadAux (lines content) 0 0 M.empty where
    gameMapReadAux [] x y m = GameMap{mapw = (M.size m `div`y), maph = y, mapCells = m}
    gameMapReadAux (line:lines) x y m = gameMapReadAux lines 0 (y+1) newM where
        newM = parseChars line x m where
            parseChars [] _ map = map
            parseChars (c:str) x map = parseChars str (x+1) m2 where
                cell = cellRead c
                m2 =  M.insert (Coord{cx = x,cy = y}) cell map

-- constructeur de GameMap
makeGameMap :: Int -> Int -> M.Map Coord Cell -> GameMap
makeGameMap = GameMap


-- invariant : 
-- 1. map entourée de mur, les portes avec deux murs de part et d'autre, et une seule sortie/entrée
-- 2. min 1 coffre
-- 3. dimensions minimal pour la carte a definir : (1 en largeur, 10 en longueur (pour l'affichage inventaire vie))?
-- 4. toute les coordonnées de la map sont inclus dans l'hauteur et la largeur
-- 5. le nombre de cases de teleportation doit etre divisible par 2 ET le nombre de leviers doit etre égal
--    au nombre de pièges + (nombre de case de teleportation / 2)
gameMap_inv :: GameMap -> Bool
gameMap_inv gm = mapSurrounedByWall_prop gm && 
                 doorFramedByWall_prop gm   && 
                 entranceExit_prop gm && 
                 minChests_prop gm &&
                 minDimensions_prop gm &&
                 validCoords_prop gm &&
                 sinkLinked_prop gm

--verifier que les premieres et dernière ligne horizontale de la carte sont bien remplis de mur
checkHorizontalWall :: GameMap -> Bool
checkHorizontalWall gm@(GameMap{maph=h,mapCells=m}) = chm_aux 0 0 gm [] && chm_aux 0 (h-1) gm [] where
    chm_aux x y g@(GameMap{mapw=mw,mapCells=mc}) t 
                                                 | x < mw = chm_aux (x+1) y g (((checkCell x y g) == Wall):t)
                                                 | otherwise = and t

--verifier que les premieres et dernière ligne verticale de la carte sont bien remplis de mur
checkVerticalWall :: GameMap -> Bool
checkVerticalWall gm@(GameMap{mapw=w,maph=h,mapCells=m}) = chm_aux 0 1 gm [] && chm_aux (w-1) 1 gm [] where
    chm_aux x y g@(GameMap{maph=mh,mapCells=mc}) t | y < (mh-1) = chm_aux x (y+1) g (((checkCell x y g) == Wall):t)
                                                   | otherwise = and t

minDimensions_prop :: GameMap -> Bool
minDimensions_prop gm@(GameMap{mapw=w,maph=h,mapCells=m}) = h > 3 && w >= 10

validCoords_prop :: GameMap -> Bool
validCoords_prop gm@(GameMap{mapCells=m}) = (M.size $ M.filterWithKey (\coord cell -> not $ getCell_pre (cx coord) (cy coord) gm) m) == 0

sinkLinked_prop :: GameMap -> Bool
sinkLinked_prop gm@(GameMap{mapCells=m}) = nb_portals `mod` 2 == 0 && nb_sink == nb_traps + (nb_portals `div` 2) where
    nb_portals = M.foldrWithKey portals 0 m where
        portals coord cell acc = case cell of
            Portal _ _ -> acc+1
            otherwise -> acc

    nb_traps = M.foldrWithKey traps 0 m where
        traps coord cell acc = case cell of
            Trap _ -> acc+1
            otherwise -> acc
    nb_sink = M.foldrWithKey sinks 0 m where
        sinks coord cell acc = case cell of
            Sink _ _ -> acc+1
            otherwise -> acc

mapSurrounedByWall_prop :: GameMap -> Bool
mapSurrounedByWall_prop gm = horizontal && vertical where
    horizontal = checkHorizontalWall gm
    vertical = checkVerticalWall gm

--verifie que chaque porte a un mur de chaque coté
doorFramedByWall_prop :: GameMap -> Bool
doorFramedByWall_prop gm@(GameMap {mapCells = mc}) = and $ M.foldrWithKey fct [] mc where
    fct = \g@(Coord{cx = x,cy=y}) cell mnd -> if cell == Door EO Closed 
        then (((checkCell (x-1) y gm) == Wall) && ((checkCell (x+1) y gm) == Wall)):mnd
        else
            if cell == Door NS Closed
                then (((checkCell x (y-1) gm) == Wall) && ((checkCell x (y+1) gm) == Wall)):mnd
                else
                    mnd

--verifier que y'a seulement une entrée et une sortie
entranceExit_prop :: GameMap -> Bool
entranceExit_prop gm@(GameMap {mapCells = mc}) = (M.size $ M.filter (== Entrance) m) == 1 && (M.size $ M.filter (== Exit) m) == 1 where
    m = M.filter (\cell -> cell == Entrance || cell == Exit) mc


minChests_prop :: GameMap -> Bool
minChests_prop gm@(GameMap {mapCells = mc}) = (M.foldr (\cell cnt -> if isChest cell then (cnt+1) else cnt ) 0 mc ) > 0

heroCanReachExit_prop :: GameMap -> Bool
heroCanReachExit_prop map = undefined




---------------------------------------------

-- FONCTIONS

----------------------------------------------

-- Transofrme un Char en nouvelle Cellule associée
cellRead :: Char -> Cell
cellRead '|'  = Door NS Closed
cellRead '-'  = Door EO Closed
cellRead 'X'  = Wall
cellRead 'E'  = Entrance
cellRead 'S'  = Exit
cellRead 'C' = Chest Closed Nothing
cellRead 'T' = Trap Activated
cellRead 'L' = Sink Desactivated Nothing
cellRead 'P' = Portal Desactivated Nothing
cellRead _ = Empty

-- Renvoient la coordonnées Nord, Sud, Ouest et Est à partir d'une Coordonnée donnée
northCoord :: Coord -> Coord
northCoord coord = Coord (cx coord) (cy coord - 1)

southCoord :: Coord -> Coord
southCoord coord = Coord (cx coord) (cy coord + 1)

ouestCoord :: Coord -> Coord
ouestCoord coord = Coord (cx coord - 1) (cy coord)

estCoord :: Coord -> Coord
estCoord coord = Coord (cx coord + 1) (cy coord)

-- Renvoie la cellule de coordonnées (x,y) dans la GameMap
-- pré : la GameMap doit contenir la cellule 
checkCell :: Int -> Int -> GameMap -> Cell
checkCell x y (GameMap{mapCells=m}) =  m M.! (Coord{cx=x,cy=y})

checkCellPre :: Int -> Int -> GameMap -> Bool
checkCellPre x y gameMap =
  case M.lookup (Coord x y) (mapCells gameMap) of
    Just _ -> True
    Nothing -> False

getCell_pre :: Int -> Int -> GameMap -> Bool
getCell_pre x y (GameMap{mapw=w,maph=h,mapCells=m}) = (x >= 0 && x < w) && (y >= 0 && y < h)

-- True si une cellule est traversable, False sinon
cellCrossable :: E.Entite -> Cell -> Bool
cellCrossable (E.Joueur _ _ _ _ _) (Trap _ ) = True
cellCrossable (E.Joueur _ _ _ _ _) (Door _ Opened ) = True
cellCrossable (E.Joueur _ _ _ _ _) Exit = True
cellCrossable (E.Joueur _ _ _ _ _) (Portal _ _) = True
cellCrossable (E.Zombie _ _ _) (Door _ Opened ) = True
cellCrossable (E.Zombie _ _ _) (Trap _ ) = True
cellCrossable (E.Zombie _ _ _) (Portal _ _) = True
cellCrossable _ Empty = True
cellCrossable _ Entrance = True
cellCrossable _ _ = False

-- True si une Cell est une porte, False sinon
isDoor :: Cell -> Bool
isDoor (Door _ _) = True
isDoor _ = False

-- True si une Cell est un coffre, False sinon
isChest :: Cell -> Bool
isChest (Chest _ _) = True
isChest _ = False

-- True si une Cell est un portail ou un piege, False sinon
isSink :: Cell -> Bool
isSink (Sink _ _) = True
isSink _ = False

-- Si le coffre sur la cellule est ouvert, en renvoie le contenu dans un Just
getChestContent :: Cell -> Maybe Entite
getChestContent (Chest Opened content) = content
getChestContent _ = error "Chest must be opened to be extracted\n"

-- Ouvre/ferme une porte de Coord coord en fonction de son etat actuel
-- pré : la cellule doit existé dans la GameMap
interactDoor :: Coord -> GameMap -> GameMap
interactDoor coord (GameMap{mapw=w,maph=h,mapCells=m}) = GameMap{mapw=w,maph=h,mapCells=newM} where
    newM = M.adjust f coord m where
    f (Door EO Closed ) = Door EO Opened
    f (Door EO Opened ) = Door EO Closed
    f (Door NS Closed ) = Door NS Opened
    f (Door NS Opened ) = Door NS Closed
    f cell = cell

interactDoor_pre :: Coord -> GameMap -> Bool
interactDoor_pre (Coord x y) gamemap = getCell_pre x y gamemap

-- Ouvre/ferme un coffre de Coord coord en fonction de son etat actuel
-- pré : la cellule doit existé dans la GameMap
interactChest :: Coord -> GameMap -> (Maybe Entite, GameMap)
interactChest coord g@(GameMap{mapw=w,maph=h,mapCells=m}) = (content, GameMap{mapw=w,maph=h,mapCells=newM}) where
    newM = M.adjust f coord m where
    f (Chest Closed content) = Chest Opened Nothing
    f (Chest Opened content) = Chest Closed Nothing
    f cell = cell

    content = case (M.lookup coord m) of
        Just (Chest _ loot) -> loot
        _ -> Nothing

interactChest_pre :: Coord -> GameMap -> Bool
interactChest_pre (Coord x y) gamemap = getCell_pre x y gamemap

-- Ouvre/ferme un portail ou un piege en fonction de son etat actuel
-- pré : la Coord coord doit existé dans la GameMap
interactSink :: Coord -> GameMap -> GameMap
interactSink coord gm@(GameMap _ _ m) = gm{mapCells=newM} where
    newM = M.foldrWithKey f m m where
    f crd (Sink Desactivated (Just pp@(Left (portal1,portal2)))) map = if crd == coord then 
        M.adjust propagate portal2 $ M.adjust propagate portal1 $ M.adjust (\_ -> Sink Activated (Just pp)) coord map 
        else map
    f crd (Sink Desactivated (Just t@(Right trap))) map = if crd == coord then  
        M.adjust propagate trap $ M.adjust (\_ -> Sink Activated (Just t)) coord map
        else map
    f crd (Sink Activated (Just pp@(Left (portal1,portal2)))) map = if crd == coord then 
         M.adjust propagate portal2 $ M.adjust propagate portal1 $ M.adjust (\_ -> Sink Desactivated (Just pp)) coord map 
        else map
    f crd (Sink Activated (Just t@(Right trap))) map = if crd == coord then  
        M.adjust propagate trap $ M.adjust (\_ -> Sink Desactivated (Just t)) coord map
        else map
    f crd _ map = map

interactSink_pre :: Coord -> GameMap -> Bool
interactSink_pre (Coord x y) gamemap = getCell_pre x y gamemap

-- Propager l'interaction du levier sur les objets auquels il est connecté
propagate :: Cell -> Cell
propagate (Portal Desactivated (Just portal)) = Portal Activated (Just portal)
propagate (Portal Activated (Just portal)) = Portal Desactivated (Just portal)
propagate (Trap Activated) = Trap Desactivated
propagate (Trap Desactivated) = Trap Activated
propagate cell = cell


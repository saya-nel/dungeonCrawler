{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Int 
import qualified Data.Int as I

import Data.Key
import qualified Data.Key as Key

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import System.Random as R

import Keyboard (Keyboard)
import qualified Keyboard as K

import GameMap
import qualified GameMap as GM
import Model
import qualified Model as Mdl
import Engine
import qualified Engine as Egn
import Entite
import qualified Entite as Ent
import Environnement
import qualified Entite as Ent

import qualified Data.Map.Strict as M
import qualified Debug.Trace as T

import System.Environment
import Data.List as DL

import System.Random
import qualified System.Random as Rand
import Data.Maybe as Maybe

-- lire le fichier de la carte et créer la gameMap
mapFileParse :: [String] -> IO GameMap
mapFileParse [] = error "File name missing"
mapFileParse (filename:_) = do
    content <- readFile filename
    return (fst $ head (readsPrec 0 content))

-- Fonction qui prend une taille et un nom, un chemin de fichier et peuple les texturemap et spritemap avec
loadAsset :: Int -> String -> Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadAsset tileSize name rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId name) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId name) (S.mkArea 0 0 50 50) -- tileSize tileSize mais pareil, conversion impossible
  let smap' = SM.addSprite (SpriteId name) sprite smap
  return (tmap', smap')

loadWin :: Int -> String -> Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadWin tileSize name rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId name) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId name) (S.mkArea 0 0 235 142) -- tileSize tileSize mais pareil, conversion impossible
  let smap' = SM.addSprite (SpriteId name) sprite smap
  return (tmap', smap')


generateEntites :: M.Map Coord Cell -> IO [(Coord, [Entite])]
generateEntites map = do
  let onlyEmpty = (\coord cell init -> if cell == Empty then coord:init else init)
  let safezone = M.foldrWithKey onlyEmpty [] map
  let nbEnnemies = 7   --ici on décide du nombre d'ennemis
  (_,ennemies) <- generateEnnemies nbEnnemies 2  safezone
  return $ ennemies

--On récupère tout les coffres vide initialement, et on les remplis avec soit des objets, soit rien si le nombre
--de coffre dépasse celui du nombre d'objet unique qu'on peut insérer
fillChests :: M.Map Coord Cell -> IO (M.Map Coord Cell)
fillChests map = do
  let chestsCoords = M.foldrWithKey (\key cell l -> if cell == (Chest Closed Nothing) then key:l else l) [] map
  let typeloots  = [Just Treasure,Just Key,Just (Sword 2),Nothing]
  let pickOneOf = (\l -> (l !!) <$> Rand.randomRIO (0, length l - 1))
  fillChests_aux chestsCoords typeloots pickOneOf map where
    fillChests_aux [] _ _ new_map = return new_map
    fillChests_aux coords (loot:lts) pick m = do
      coord <- pick coords
      let restCoords = delete coord coords
      let lts' = if loot == Nothing then [loot] else lts
      let new_m = M.adjust (\_ -> Chest Closed loot) coord m
      fillChests_aux restCoords lts' pick new_m

--Associer un piège, avec un levier
linkSinkToTraps :: M.Map Coord Cell -> IO (M.Map Coord Cell)
linkSinkToTraps map = do
  let traps = M.foldrWithKey (\key cell l -> if cell == (Trap Activated) then key:l else l) [] map
  let sinks = M.foldrWithKey (\key cell l -> if cell == (Sink Desactivated Nothing) then key:l else l) [] map
  let pickOneOf = (\l -> (l !!) <$> Rand.randomRIO (0, length l - 1))

  linkSinkToTraps_aux traps sinks pickOneOf map where
    linkSinkTOTraps_aux t [] _ sinkLinked = return sinkLinked
    linkSinkToTraps_aux [] s _ sinkLinked = return sinkLinked
    linkSinkToTraps_aux t s pick sinkLinked = do
      trap <- pick t
      sink <- pick s
      let restTraps = delete trap t
      let restSinks = delete sink s
      let new_m = M.adjust (\_ -> Sink Desactivated (Just (Right trap))) sink sinkLinked
      linkSinkToTraps_aux restTraps restSinks pick new_m

--Associer un couple de portails, représenté par un couple de Coord, avec un levier
linkSinkToPortals :: M.Map Coord Cell -> IO (M.Map Coord Cell)
linkSinkToPortals map = do
  let portals = M.foldrWithKey (\key cell l -> if cell == (Portal Desactivated Nothing) then key:l else l) [] map
  let sinks = M.foldrWithKey (\key cell l -> if cell == (Sink Desactivated Nothing) then key:l else l) [] map
  let pickOneOf = (\l -> (l !!) <$> Rand.randomRIO (0, length l - 1))
  linkSinkToPortals_aux portals sinks pickOneOf map where
    linkSinkToPortals_aux [] sks pick sinkLinked = return sinkLinked
    linkSinkToPortals_aux pts [] pick sinkLinked = return sinkLinked
    linkSinkToPortals_aux pts sks pick m = do
      portal1 <- pick pts
      let r = delete portal1 pts
      portal2 <- pick r
      let r' = delete portal2 r
      sink <- pick sks
      let restSinks = delete sink sks
      -- on active les portails en les liant l'un avec l'autre, et a la fin on lie un levier a ce couple de portails
      let linkp1Top2 = M.adjust (\_ -> Portal Activated (Just portal2)) portal1 m
      let linkp2Top1 = M.adjust (\_ -> Portal Activated (Just portal1)) portal2 linkp1Top2
      let sinkLink = M.adjust (\_ -> Sink Desactivated (Just (Left (portal1,portal2)))) sink linkp2Top1
      linkSinkToPortals_aux r' restSinks pick sinkLink

-- génère les ennemis
generateEnnemies :: Int -> Int -> [Coord]  -> IO ([Coord],[(Coord, [Entite])])
generateEnnemies howmany id sfz = do
  let pickOnOf = (\l -> (l !!) <$> Rand.randomRIO (0, length l - 1))
  getennemies (min howmany (length sfz)) pickOnOf sfz [] id where
        getennemies n pick safezone enms id | n == 0 = return (safezone,enms)
                        | n > 0 = do
                          coord <- pick safezone
                          --Infos sur l'ennemie
                          let life = 5
                          let attack = 1
                          let ennemie = makeZombie id (Stats life attack) False
                          --le reste des cases libres pour y inserer
                          let restSafeZone = delete coord safezone
                          getennemies (n-1) pick restSafeZone ((coord, [ennemie]) : enms) (id+1)
                        | otherwise = getennemies 0 pick safezone enms id

mkJoueur :: Entite
mkJoueur = makeJoueur 1 (Stats 10 1) False [] Unarmed

main :: IO ()
main = do
    --INITIALISATION DE L'ÉTAT DU JEU ET DU MOTEUR
    args <- getArgs
    gamemap <- mapFileParse args
    let map = mapCells gamemap
    let tileSize = 50
    let width = CInt (fromIntegral ( tileSize * mapw gamemap ) :: Int32) -- taille des sprites * nb de colonnes de la map
    let height = CInt (fromIntegral ( tileSize * (maph gamemap+1) ) :: Int32) -- taille des sprites * nb de lignes de la map + marge inventaire
    --Generation aleatoire d'ennemies
    --1 On place le joueur
    let fct = (\coord cell init -> if cell == Entrance then (coord,[mkJoueur]):init else (coord,[]):init)
    let lenv = M.foldrWithKey fct [] map
    let menv = M.fromList $ lenv
    map' <- fillChests map -- on remplis les coffres de la map avec des entités (vide, ou loot)
    map'' <- linkSinkToTraps  map' -- connecter des pièges a des leviers aleatoirement.
    map <- linkSinkToPortals map'' -- connecter des couples de portails de teleportation a des leviers
    entites <- generateEntites map -- ici on génère toute les entités utile, les ennemies etc
    let env = makeEnvironnement $ foldl (\env (coord,ent) -> M.adjust (\_ -> ent) coord env) menv entites

    --Initialisation du moteur
    let kbd = K.createKeyboard
    gen <- getStdGen
    let initModel = makeModel gamemap{mapCells = map} env gen "Debut de la partie\n" kbd
    let initEngine = makeEngine initModel

    -- init sdl2
    initializeAll
    window <- createWindow "Dungeon Crawler" $ defaultWindow { windowInitialSize = V2 width height}
    renderer <- createRenderer window (-1) defaultRenderer
    -- elements de la map
    (tmap,smap) <- loadAsset tileSize "X" renderer "./lib/assets/Wall.png" TM.createTextureMap SM.createSpriteMap
    (tmap,smap) <- loadAsset tileSize " " renderer "./lib/assets/Empty.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "E" renderer "./lib/assets/Entrance.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "S" renderer "./lib/assets/Exit.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "-" renderer "./lib/assets/DoorEOClosed.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "|" renderer "./lib/assets/DoorNSClosed.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "-'" renderer "./lib/assets/DoorEOOpened.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "|'" renderer "./lib/assets/DoorNSOpened.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "C" renderer "./lib/assets/ChestClosed.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "C'" renderer "./lib/assets/ChestOpened.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "Tr" renderer "./lib/assets/TreasureInv.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "Sw" renderer "./lib/assets/SwordInv.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "K" renderer "./lib/assets/KeyInv.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "T" renderer "./lib/assets/TrapActiv.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "T'" renderer "./lib/assets/TrapUnactiv.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "L" renderer "./lib/assets/SinkKo.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "L'" renderer "./lib/assets/SinkOk.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "P" renderer "./lib/assets/PortalUnactiv.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "P'" renderer "./lib/assets/PortalActiv.png" tmap smap
    -- entités
    (tmap,smap) <- loadAsset tileSize "J" renderer "./lib/assets/Junarmed.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "J>" renderer "./lib/assets/Jarmed.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "J>'" renderer "./lib/assets/JarmedAttacked.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "J'" renderer "./lib/assets/JunarmedAttacked.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "Z" renderer "./lib/assets/Zombie.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "Z'" renderer "./lib/assets/ZombieAttacked.png" tmap smap
    -- écrans
    (tmap,smap) <- loadWin 200 "win" renderer "./lib/assets/win.png" tmap smap
    (tmap,smap) <- loadWin 200 "loose" renderer "./lib/assets/loose.png" tmap smap
    (tmap,smap) <- loadAsset tileSize "life" renderer "./lib/assets/heart.png" tmap smap


    gameLoop (width,height) 50 60 initEngine renderer tmap smap kbd 0

-- Renvoie True si la partie est gagné, False sinon
win :: State -> Bool
win Win = True
win _ = False

-- Renvoie True si la partie est perdue, False sinon
loose :: State -> Bool
loose Loose = True
loose _ = False

-- Affichage de l'écran de fin 
finalDisplay :: (CInt, CInt) -> Renderer -> TextureMap -> SpriteMap -> State -> IO ()
finalDisplay dimensions renderer tmap smap state = do 
    let (width,height) = dimensions
    startTime <- time
    clear renderer
    if win state then S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "win") smap) (width `div` (fromIntegral 3)) (height `div` (fromIntegral 3)))
    else S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "loose") smap) (width `div` (fromIntegral 3)) (height `div` (fromIntegral 3)))
    present renderer
    endTime <- time
    let refreshTime = endTime - startTime
    let delayTime = floor (((1.0 / 60) - refreshTime) * 1000)
    threadDelay $ delayTime * 1000
    finalDisplay dimensions renderer tmap smap state

-- boucle du jeux
gameLoop :: (RealFrac a, Show a) => (CInt,CInt) -> Int -> a -> State -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Int -> IO ()
gameLoop dimensions tileSize frameRate state renderer tmap smap kbd nb_tours = do
    startTime <- time
    --evenements  + keyboard pret
    events <- pollEvents
    let kbd' = K.handleEvents events kbd
    --on efface 
    clear renderer
    let map = mapCells $ getGameMap state
    let env = Egn.getEnv state
    let cellToSprite = (\(GM.Coord x y) cell -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show cell)) smap) (fromIntegral (x*tileSize)) (fromIntegral (y*tileSize))))
    let entiteToSprite = (\(GM.Coord x y) entite -> if (length entite) > 0 then S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show (head entite))) smap) (fromIntegral (x*tileSize)) (fromIntegral (y*tileSize))) else return ())
    
    let (coord,joueur) = let j = findId 1 (makeEnvironnement env) in if Maybe.isJust j then  Maybe.fromJust $ j else ((Coord 0 0),mkJoueur)
    let inventory = Ent.inventory joueur
    let sizeInv = length inventory
    let life = (vie(stats(joueur)))
    let zipInv = DL.zipWith (\id loot -> (id,loot)) [0..sizeInv] inventory
    let displayLife = [0..life]

    --ici je map un effet de bord (l'affichage de sprite) sur la carte, et l'environnement, pour afficher
    --la carte et les entités sur l'écran, l'inventaire...
    let (width,height) = dimensions
    mapM_ (\(id,loot) -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show loot)) smap) (fromIntegral ((id*26)+(id*15))) (height - (fromIntegral tileSize)))) zipInv
    mapM_ (\id -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "life") smap) ((width - (fromIntegral tileSize)) - (fromIntegral ((id*26)+(id*15)))) (height - (fromIntegral tileSize)))) displayLife
    Key.mapWithKeyM_ cellToSprite map
    Key.mapWithKeyM_ entiteToSprite env
    
    --on dessine
    present renderer
    --gestion du framerate
    endTime <- time
    let refreshTime = endTime - startTime
    let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
    threadDelay $ delayTime * 1000 -- microseconds
    endTime <- time

    let newState = state_round state kbd' nb_tours
    if win state || loose state then finalDisplay dimensions renderer tmap smap state else return ()
    unless (K.keypressed KeycodeEscape kbd') (gameLoop dimensions tileSize frameRate newState renderer tmap smap kbd (nb_tours + 1))


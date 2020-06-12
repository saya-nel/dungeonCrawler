module GameMapSpec where

import GameMap as GM
import Test.Hspec
import Test.QuickCheck

import qualified Data.Map.Strict as M

-- La carte valide qui seras utilisée pour les prop
{-{
XXXXXXXXXXXXXXXXXXXXXXXX
X     |  S  T X        X
X  X  XXX-XX  X   P    X
X  X          X     L  X
X  XXXX       XX       X
X C   X  C     XXXXXXXXX
X    LX                X
XXXXXTX         XXXXXXXX
XE    P          |  L  X
XXX             XXXXXXXX
X                      X
X       C              X
XXXXXXXXXXXXXXXXXXXXXXXX

}-}

exGameMap = gameMapRead "XXXXXXXXXXXXXXXXXXXXXXXX\nX     |  S  T X        X\nX  X  XXX-XX  X   P    X\nX  X          X     L  X\nX  XXXX       XX       X\nX C   X  C     XXXXXXXXX\nX    LX                X\nXXXXXTX         XXXXXXXX\nXE    P          |  L  X\nXXX             XXXXXXXX\nX                      X\nX       C              X\nXXXXXXXXXXXXXXXXXXXXXXXX\n"
portals = (Coord 18 2, Coord 6 8)
trap = Coord 12 1
sink = Coord 20 3
sink' = Coord 20 8
gm' = M.adjust (\_ -> Portal Activated (Just (fst portals))) (snd portals) $ M.adjust (\_ -> Portal Activated (Just (snd portals))) (fst portals) $ M.adjust (\_ -> Sink Desactivated (Just (Left portals))) sink (mapCells exGameMap)
gm = M.adjust (\_ -> Trap Activated) trap $ M.adjust (\_ -> Sink Desactivated (Just (Right trap))) sink' gm'

--sink    = head $ M.foldrWithKey (\key cell l -> if cell == (Sink Desactivated (Just(Left portals))) then key:l else l) [] gm
expectedMap = M.adjust (\_ -> (Portal Desactivated (Just (snd portals))) ) (fst portals) $ M.adjust (\_ -> (Portal Desactivated (Just (fst portals))) ) (snd portals) $ M.adjust (\_ -> (Sink Activated (Just(Left portals))) ) sink gm


--sink' = head $ M.foldrWithKey (\key cell l -> if cell == (Sink Desactivated (Just(Right trap))) then key:l else l) [] gm
expectedMap2 = M.adjust (\_ -> (Trap Desactivated) ) trap $ M.adjust (\_ -> (Sink Activated (Just(Right trap))) ) sink' gm


northCoordSpec =
  describe "northCoord" $ do 
    it "retourne la coordonnée au nord" $ do
      northCoord (Coord 1 1) `shouldBe` Coord 1 0

southCoordSpec =
  describe "southCoord" $ do 
    it "retourne la coordonnée au sud" $ do
      southCoord (Coord 1 1) `shouldBe` Coord 1 2

ouestCoordSpec =
  describe "ouestCoord" $ do 
    it "retourne la coordonnée a l'ouest" $ do
      ouestCoord (Coord 1 1) `shouldBe` Coord 0 1

estCoordSpec =
  describe "estCoord" $ do 
    it "retourne la coordonnée a l'est" $ do
      estCoord (Coord 1 1) `shouldBe` Coord 2 1

--respecte sa pré condition et le gamemap en param respecte son invariant
prop_checkCell :: Int -> Int -> GameMap -> Property
prop_checkCell x y gamemap = property $ (gameMap_inv gamemap) && (checkCellPre x y gamemap)

checkCellSpec gamemap = do
  describe "checkCell" $ do
    it "Préserve l'invariant" $ do
      property $ prop_checkCell 9 1 gamemap
    it "Retourne la case correspondante : ici la sortie 'S'" $ do
      checkCell 9 1 gamemap `shouldBe` Exit

prop_interactDoor :: Coord -> GameMap -> Property
prop_interactDoor coord gamemap = (interactDoor_pre coord gamemap) && (gameMap_inv gamemap) ==> property $ gameMap_inv (interactDoor coord gamemap)

interactDoorSpec gamemap@(GameMap w h gmap) = do
  describe "interactDoor" $ do
    it "préserve l'invariant" $ do
      property $ prop_interactDoor (Coord 9 2) gamemap
    it "Retourne une gamemap avec la porte a (9,2) ouverte (precedemment fermée)" $ do
      let expectedMap = M.adjust (\_ -> Door EO Opened ) (Coord 9 2) gmap
      let expected = (GameMap w h expectedMap)
      interactDoor (Coord 9 2) gamemap `shouldBe`expected
    it "Retourne une gamemap avec la porte a (9,2) fermée (precedemment ouverte)" $ do
      let openedDoor = interactDoor (Coord 9 2) gamemap
      interactDoor (Coord 9 2) openedDoor `shouldBe` gamemap

prop_interactChest :: Coord -> GameMap -> Property
prop_interactChest coord gamemap = (interactChest_pre coord gamemap) && (gameMap_inv gamemap) ==> property $ gameMap_inv $ snd (interactChest coord gamemap)

interactChestSpec gamemap@(GameMap w h gmap) = do
  describe "interactDoor" $ do
    it "préserve l'invariant" $ do
      property $ prop_interactChest (Coord 9 2) gamemap
    it "Retourne une gamemap avec le coffre a (2,5) vide ouvert (precedemment fermé)" $ do
      let expectedMap = M.adjust (\_ -> Chest Opened Nothing ) (Coord 2 5) gmap
      let expected = (GameMap w h expectedMap)
      interactChest (Coord 2 5) gamemap `shouldBe`(Nothing,expected)
    it "Retourne une gamemap avec le coffre a (2,5) vide fermé (precedemment ouvert)" $ do
      let openedChest = interactDoor (Coord 2 5) gamemap
      interactChest (Coord 2 5) openedChest `shouldBe` (Nothing,gamemap)

prop_interactSink :: Coord -> GameMap -> Property
prop_interactSink coord gamemap = (interactSink_pre coord gamemap) && (gameMap_inv gamemap) ==> property $ gameMap_inv $ interactSink coord gamemap

interactChestSink gamemap@(GameMap w h gmap)= do
  describe "interactSink" $ do
    it "Retourne une gamemap avec le levier (20,3) a activer, et les deux portails de téléportation ((18,2), (6,8)) a désactivé (precedemment activé)" $ do
      let expected = (GameMap w h expectedMap)
      interactSink sink (GameMap w h gm) `shouldBe`expected
    it "Retourne une gamemap avec le levier (20,3) a  désactiver, et les deux portails de téléportation ((18,2), (6,8)) a activé (precedemment désactivé)" $ do
      let activSink = (GameMap w h expectedMap)
      --let sink    = head $ M.foldrWithKey (\key cell l -> if cell == (Sink Activated (Just(Left portals))) then key:l else l) [] expectedMap
      let expectedMap' = M.adjust (\_ -> (Portal Activated (Just (snd portals))) ) (fst portals) $ M.adjust (\_ -> (Portal Activated (Just (fst portals))) ) (snd portals) $ M.adjust (\_ -> (Sink Desactivated (Just(Left portals))) ) sink expectedMap
      let expected = (GameMap w h expectedMap')
      interactSink sink activSink `shouldBe` expected
    it "Retourne une gamemap avec le levier (20,8) a activer, et le piège (12,1) a désactiver (precedemment activé)" $ do
      let expected = (GameMap w h expectedMap2)
      interactSink sink' (GameMap w h gm) `shouldBe` expected
    it "Retourne une gamemap avec le levier (20,8) a désactiver, et le piège (12,1) a activer (precedemment désactivé)" $ do
      let activSink = (GameMap w h expectedMap2)
      --let sink    = head $ M.foldrWithKey (\key cell l -> if cell == (Sink Activated (Just(Right trap))) then key:l else l) [] expectedMap2
      let expectedMap2' = M.adjust (\_ -> (Trap Activated) ) trap $ M.adjust (\_ -> (Sink Desactivated (Just(Right trap))) ) sink' expectedMap2
      let expected = (GameMap w h expectedMap2')
      interactSink sink' activSink `shouldBe` expected

-- ceux la c'est sur tout les fichiers des dossiers qu'on envoie dans Spec.hs
mapSurrounedByWallSpec map expected = do
  describe "mapSurrounedByWall_prop" $ do
    it "La map n'est pas saine si elle est pas entourée par des murs, saine sinon" $
      GM.mapSurrounedByWall_prop map `shouldBe` expected

entranceExitSpec map expected = do
  describe "entranceExit_prop" $ do
    it "La map n'est saine si elle ne contient pas exactement 1 sortie et 1 entrée, saine sinon" $
      GM.entranceExit_prop map `shouldBe` expected

doorFramedSpec map expected = do
  describe "doorFramed_prop" $ do
    it "La map n'est pas saine si les portes ne sont pas toute encadrée de murs, saine sinon" $
      GM.doorFramedByWall_prop map `shouldBe` expected

minChestsSpec map expected= do
  describe "minChests_prop" $ do
    it "La map n'est pas saine si il n'y a pas au moins un coffre, saine sinon" $
      GM.minChests_prop map `shouldBe` expected

minDimensionsSpec map expected= do
  describe "minDimensions_prop" $ do
    it "La map n'est pas saine si sa taille n'est pas de minimum 3 en hauteur et 10 en largeur, saine sinon" $
      GM.minDimensions_prop map `shouldBe` expected

validCoordsSpec map expected= do
  describe "validCoords_prop" $ do
    it "La map n'est pas saine si il existe une coordonnée inclus dans la map, qui n'est pas inclus dans la hauteur et la largeur de la map, saine sinon" $
      GM.validCoords_prop map `shouldBe` expected

sinkLinkedSpec map expected= do
  describe "sinkLinked_prop" $ do
    it "La map n'est pas saine si le nombre de portails ne sont pas divisible par 2 ET que le nombre de leviers /= nb_sink == nb_traps + (nb_portals `div` 2), saine sinon" $
      GM.sinkLinked_prop map `shouldBe` expected

validMapSpec gamemap = do
  northCoordSpec
  southCoordSpec
  ouestCoordSpec
  estCoordSpec
  checkCellSpec exGameMap
  interactDoorSpec exGameMap
  interactChestSink exGameMap
  doorFramedSpec gamemap True
  minChestsSpec gamemap True
  entranceExitSpec gamemap True
  mapSurrounedByWallSpec gamemap True
  minDimensionsSpec gamemap True
  validCoordsSpec gamemap True
  sinkLinkedSpec gamemap True


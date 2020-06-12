module EnvironnementSpec where 

import Test.Hspec as Hs
import Test.QuickCheck as Q
import qualified Data.Map.Strict as Map


import Entite as Ent
import Environnement as Env
import GameMap as GM
 
import EntiteSpec
import qualified EntiteSpec as ES

import System.Random (Random)

-- entités
exJoueur1 = makeJoueur 1 (Stats 5 1) False [] Unarmed
exJoueur2 = makeJoueur 3 (Stats 5 1) False [] Unarmed
exZombie1 = makeZombie 2 (Stats 3 1) False
exZombie1Attacked = makeZombie 2 (Stats 2 1) True
-- environnement
exMapEnvironnement = Map.fromList 
  [(Coord 0 0, [exJoueur1, exJoueur2]), (Coord 1 0, [exZombie1]), (Coord 2 0, [])]
exEnvironnement = Environnement exMapEnvironnement


genEnvironment :: Gen Environnement
genEnvironment = do
  nb <- choose (1,10) :: Gen Integer
  mp <- genEnvironment_aux nb []
  return $ makeEnvironnement $ Map.fromList mp where
    genEnvironment_aux 0 env = return env
    genEnvironment_aux nb env = do
      x <- choose (0,50)
      y <- choose (0,50)
      let coord = Coord x y
      entity <- genEntite
      let pieceOf = (coord,[entity])
      genEnvironment_aux (nb-1) (pieceOf:env)


prop_genEnvironment_inv :: Property
prop_genEnvironment_inv = forAll genEnvironment $ environnementInv

specGenEnvironment = do
  describe "genEnvironment" $ do
    it "generates environment that satisfy, or not, their invariant" $
      property prop_genEnvironment_inv

-- verifie que makeEnvironnement respecte l'invariant
prop_makeEnvironnement :: Map.Map Coord [Entite] -> Property
prop_makeEnvironnement map =
  property $ environnementInv (makeEnvironnement map)

makeEnvironnementSpec = do
  describe "makeEnvironnement" $ do
    it "Preserve l'invariant" $ do
      property $ prop_makeEnvironnement exMapEnvironnement
    it "Retourne l'Environnement construit à partir de la map" $ do
      makeEnvironnement exMapEnvironnement `shouldBe` exEnvironnement

-- verifie que isPassable respecte l'invariant
prop_isPassable :: Environnement -> Property
prop_isPassable env =
  property $ environnementInv env

isPassableSpec = do
  describe "isPassable" $ do 
    it "Respecte l'invariant" $ do
      property $ prop_isPassable exEnvironnement
    it "Retourne True si la case est franchissable" $ do
      isPassable (Coord 2 0) exEnvironnement `shouldBe` True
    it "Retourne False si la case est infranchissable" $ do
      isPassable (Coord 0 0) exEnvironnement `shouldBe` False


-- verifie que findId respecte l'invariant 
prop_findId :: Environnement -> Property
prop_findId env = 
  property $ environnementInv env

findIdSpec = do
  describe "findId" $ do
    it "Preserve l'invariant" $ do
      property $ prop_findId exEnvironnement
    it "Retourne la position et l'entité d'id id si elle existe" $ do
      findId 3 exEnvironnement `shouldBe` Just (Coord 0 0, exJoueur2)
    it "Retourne Nothing si l'entité n'existe pas" $ do
      findId 42 exEnvironnement `shouldBe` Nothing


-- verifie que removeId respecte l'invariant et sa pré-condition
prop_removeId :: Int -> Environnement -> Property
prop_removeId id env =
  (environnementInv env) && (removeIdPre id env) ==> property $ environnementInv (removeId id env)

removeIdSpec = do
  describe "removeId" $ do
    it "Preserve l'invariant et respecte sa pré-condition" $ do
      property $ prop_removeId 3 exEnvironnement
    it "Suprime l'Entite d'id id dans l'Environnement" $ do
      removeId 3 exEnvironnement `shouldBe` (Environnement $ Map.fromList [(Coord 0 0, [exJoueur1]), (Coord 1 0, [exZombie1]), (Coord 2 0, [])])

-- verifie que moveId respecte l'invariant et sa pré-condition
prop_moveId :: Int -> Coord -> Environnement -> Property
prop_moveId id coord env =
  (environnementInv env) && (moveIdPre id coord env) ==> property $ environnementInv (moveId id coord env)

moveIdSpec = do
  describe "moveId" $ do
    it "Preserve l'invariant et respecte sa pré-condition" $ do
      property $ prop_moveId 3 (Coord 1 0) exEnvironnement
    it "Change les coordonnées de l'Entite d'id id dans l'Environnement" $ do
      moveId 3 (Coord 1 0) exEnvironnement `shouldBe` (Environnement $  Map.fromList [(Coord 0 0, [exJoueur1]), (Coord 1 0, [exJoueur2, exZombie1]), (Coord 2 0, [])])

-- verifie que updateEntity respecte l'invariant et sa pré-condition
prop_updateEntity :: Entite -> Entite -> Environnement -> Property
prop_updateEntity entity newEntity env =
  (environnementInv env) && (updateEntityPre entity newEntity env) ==> property $ environnementInv (updateEntity entity newEntity env)

updateEntitySpec = do
  describe "updateEntity" $ do
    it "Preserve l'invariant et respecte sa pré-condition" $ do
      property $ prop_updateEntity exZombie1 exZombie1Attacked exEnvironnement
    it "Met à jour l'Entite dans l'Environnement" $ do
      (updateEntity exZombie1 exZombie1Attacked exEnvironnement) `shouldBe` (Environnement $ Map.fromList [(Coord 0 0, [exJoueur1, exJoueur2]), (Coord 1 0, [exZombie1Attacked]), (Coord 2 0, [])])
 

environnementSpec = do
  makeEnvironnementSpec
  isPassableSpec 
  findIdSpec
  removeIdSpec
  moveIdSpec 
  updateEntitySpec
  specGenEnvironment

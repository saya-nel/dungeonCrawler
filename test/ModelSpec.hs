module ModelSpec where

import qualified Data.Map.Strict as M
import System.Random
import Test.Hspec
import Test.QuickCheck

import Model as M
import GameMap as GM
import Entite as Ent
import Environnement as Env
import Keyboard as K

-- Legende map :
-- J est le Joueur, Z un Zombie, E l'entrée, S la sortie, X les bordures, | et - des portes,
-- T piege, C coffre, L levier

-- La map / Environnement de base utilisé pour les tests est le suivant :

-- XXXXXXXXXXXXX
-- X     |  SX X
-- X  X  XXX-X X
-- XZ X        X
-- X  XXXX     X
-- X     X     X
-- XJZ   X     X
-- XXX         X
-- XE  X       X
-- XXXXXXXXXXXXX

-- La map / Enviroonemen utilisé pour les autres tests

--XXXXXXXXXXXXXXXXXXXXXXXX
--X     |  S  T X        X
--X  X  XXX-XX  X   P    X
--X  X          X     L  X
--X  XXXX       XX       X
--X     X  C     XXXXXXXXX
--X    LX                X
--XXXXXTX         XXXXXXXX
--XE    P          |  L  X
--XXX             XXXXXXXX
--X                      X
--X       C              X
--XXXXXXXXXXXXXXXXXXXXXXXX


exJoueur1 = makeJoueur 1 (Stats 5 1) False [] Unarmed
exZombie1 = makeZombie 2 (Stats 3 1) False
exZombie1Attacked = makeZombie 2 (Stats 2 1) True


-- verifie que makeModel respecte l'invariant
prop_makeModel :: GameMap -> Environnement -> StdGen -> String -> Keyboard -> Property
prop_makeModel gameMap env gen s kbd =
  property $ modelInv (makeModel gameMap env gen s kbd)

makeModelSpec model = do
  describe "makeModel" $ do
    it "Preserve l'invariant" $ do
      property $ prop_makeModel (gamemap model) (env model) (gen model) (gamelog model) (keyboard model)
    it "Renvoie un nouveau model" $ do
      makeModel (gamemap model) (env model) (gen model) (gamelog model) (keyboard model) `shouldBe` model

-- verifie que move respecte l'invariant et sa pré-condition
prop_move :: Model -> Entite -> Coord -> Property
prop_move model entity coord =
  (modelInv model) && (movePre model entity coord) ==> property $ modelInv (move model entity coord)

moveSpec model = do
  describe "move" $ do
    it "Preserve l'invariant" $ do
      property $ prop_move model (exJoueur1) (Coord 2 6)
    it "Retourne un nouveau Model où Joueur 1 à bouger en Coord 2 6" $ do
      -- resultat obtenu
      let res = move model (exJoueur1) (Coord 2 6)
      -- resultat attendu
      let expectedEnv = Environnement $ M.insert (Coord 1 6) [] $ M.insert (Coord 2 6) [exJoueur1, exZombie1] (content $ env model) 
      let expected = Model (gamemap model) expectedEnv (gen model) "1: a bougé sur (2,6)\n" (keyboard model)
      -- test
      res `shouldBe` expected


-- Verifie que tour preserve l'invariant et respecte sa pré-condition
prop_tour :: Model -> Entite -> Property
prop_tour model entity = 
  (modelInv model) && (tourPre model entity) ==> property $ modelInv (tour model entity)  

tourSpec model = do
  describe "tour" $ do
    it "Preserve l'invariant" $ do
      property $ prop_tour model (exZombie1)


-- Verifie que interactWithDoor preserve l'invariant et respecte sa pré-condition
prop_interactWithDoor :: Model -> Coord -> Property 
prop_interactWithDoor model coord =
  (modelInv model) && (interactWithDoorPre model coord) ==> property $ modelInv (interactWithDoor model coord)

interactWithDoorSpec model = 
  let coord = (Coord 5 1) in
  describe "interactWithDoor" $ do
    it "Preserve l'invariant" $ do
      property $ prop_interactWithDoor model coord
    it "Ouvre la porte a l'Est de (5,1) et renvoie un nouveau model avec la nouvelle carte" $ do
      let ancient_map = (gamemap model)
      let expected_map = interactDoor (Coord 6 1) ancient_map -- ouvre la porte
      interactWithDoor model coord `shouldBe` (makeModel expected_map (env model) (gen model) (gamelog model) (keyboard model))

-- Verifie que interactWithChest preserve l'invariant et respecte sa pré-condition
prop_interactWithChest :: Model -> Int -> Coord -> Property 
prop_interactWithChest model id coord =
  (modelInv model) && (interactWithChestPre model id coord) ==> property $ modelInv (interactWithChest model id coord)

interactWithChestSpec model = 
  let coord = (Coord 8 5) in 
  describe "interactWithChest" $ do
    it "Preserve l'invariant" $ do
      property $ prop_interactWithChest model 1 coord
    it "Ouvre le coffre a l'est de (8,5) et renvoie un nouveau model avec la nouvelle carte" $ do
      let ancient_map = (gamemap model)
      let (loot,expected_map) = interactChest (Coord 9 5) ancient_map --ouvre la porte
      interactWithChest model 1 coord `shouldBe` (makeModel expected_map (env model) (gen model) (gamelog model) (keyboard model))

prop_interactWithSink :: Model -> Coord -> Property 
prop_interactWithSink model coord =
  (modelInv model) && (interactWithSinkPre model coord) ==> property $ modelInv (interactWithSink model coord)

interactWithSinkSpec model = 
  let coord = (Coord 19 3) in 
  describe "interactWithSink" $ do
    it "Preserve l'invariant" $ do
      property $ prop_interactWithSink model coord
    it "Active le levier a l'est de (19,3) et les deux portails a ((18,2), (6,8)) " $ do
      let ancient_map = (gamemap model)
      let expected_map = interactSink (Coord 20 3) ancient_map
      interactWithSink model coord `shouldBe` (makeModel expected_map (env model) (gen model) (gamelog model) (keyboard model))
    it "Desactive le levier a l'est de (19,3) et active les deux portails a ((18,2), (6,8))" $ do
      let ancient_map = (gamemap model)
      let expected_map = interactSink (Coord 20 3) ancient_map
      interactWithSink model coord `shouldBe` (makeModel expected_map (env model) (gen model) (gamelog model) (keyboard model))

-- Verifie que canMoveTo respecte l'invariant et sa pré-condition
prop_canMoveTo :: Model -> Entite -> Property
prop_canMoveTo model entity = 
  property $ (modelInv model) && (canMoveToPre model entity)

canMoveToSpec model = do
  describe "canMoveTo" $ do
    it "Respecte l'invariant" $ do 
      property $ prop_canMoveTo model (exJoueur1)
    it "Retourne la liste des directions dans vers lesquels l'entite peux bouger" $ do
      canMoveTo model (exJoueur1)  `shouldBe` [N]


-- Verifie que canInteract respecte l'invariant et sa pré-condition
prop_canOpen :: Model -> Coord -> Property
prop_canOpen model coord =
  property $ (modelInv model) && (canInteractPre model coord)

canOpenSpec model = do
  describe "canInteract" $ do
    it "Respecte l'invariant" $ do
      property $ prop_canOpen model (Coord 5 1)
    it "Il y'a une porte a droite de (5,1)" $ do
      canInteract model (Coord 5 1) `shouldBe` [ID Est]


treasureSpec model = do
  describe "treasure_prop" $ do
    it "Il doit y avoir un et un seul Treasure contenu dans un coffre sur la map a l'initialisation du jeu" $
      treasure_prop model `shouldBe` True

-- verifie que canAttack respecte l'invariant et sa pré-condition
prop_canAttack :: Model -> Entite -> Property
prop_canAttack model entity =
  property $ (modelInv model) && (canAttackPre model entity)

canAttackSpec model = do
  describe "canAttack" $ do
    it "Respecte l'invariant" $ do
      property $ prop_canAttack model exJoueur1
    it "Renvoie la liste des entités attaquables" $ do
      canAttack model exJoueur1 `shouldBe` [exZombie1]


-- verifie que attack preserve l'invariant et respecte sa pré-condition
prop_attack :: Model -> Entite -> [Entite] -> Property
prop_attack model entity entities =
  (modelInv model) && (attackPre model entity entities) ==> property $ modelInv (attack model entity entities)

attackSpec model = do
  describe "attack" $ do 
    it "Preserve l'invariant" $ do
      property $ prop_attack model exJoueur1 [exZombie1]
    it "Attack les entites de la liste" $ do
      let fct = (\coord cell init -> if coord == Coord 1 6 then (coord, [exJoueur1]):init else if coord == Coord 2 6 then (coord, [exZombie1Attacked]):init else (coord,[]):init)
      let expected_env = Environnement $ M.fromList $ M.foldrWithKey fct [] (mapCells (gamemap model)) 
      let expected_model = Model (gamemap model) expected_env (gen model) (gamelog model) (keyboard model) 
      (attack model exJoueur1 [exZombie1]) `shouldBe` expected_model


-- verifie que cleanAttacked preserve l'invariant et respecte sa pré-condition
prop_cleanAttacked :: Model -> Entite -> Property
prop_cleanAttacked model entity =
  (modelInv model) && (cleanAttackedPre model entity) ==> property $ modelInv (cleanAttacked model entity)

cleanAttackedSpec model = do
  describe "cleanAttacked" $ do
    it "Preserve l'invariant" $ do
      let new_model = attack model exJoueur1 [exZombie1] 
      property $ prop_cleanAttacked new_model exZombie1Attacked
    it "Retire l'état attacked" $ do
      -- model attendu (zombie a été attaqué, a perdu un pdv, et a remis sont etat attacked a false)
      let fct = (\coord cell init -> if coord == Coord 1 6 then (coord, [exJoueur1]):init else if coord == Coord 2 6 then (coord, [exZombie1Attacked{attacked=False}]):init else (coord,[]):init)
      let expected_env = Environnement $ M.fromList $ M.foldrWithKey fct [] (mapCells (gamemap model)) 
      let expected_model = Model (gamemap model) expected_env (gen model) (gamelog model) (keyboard model) 
      (cleanAttacked (attack model exJoueur1 [exZombie1]) exZombie1Attacked) `shouldBe` expected_model


-- Verifie que possibleSolutions respecte l'invariant et sa pré-condition
prop_possibleSolutions :: Model -> Entite -> Property 
prop_possibleSolutions model entity =
  property $ (modelInv model) && (possibleSolutionsPre model entity)

possibleSolutionsSpec model = do
  describe "possibleSolutions" $ do
    it "Respecte l'invariant" $ do
      property $ prop_possibleSolutions model (exJoueur1)
    it "Retourne la liste des Ordre possible pour l'entite" $ do
      possibleSolutions model (exJoueur1) `shouldBe` [A,N,R]


-- Verifie que provide respecte l'invariant et sa pré-condition
prop_provide :: Model -> Entite -> Property
prop_provide model entity =
  property $ (modelInv model) && (providePre model entity)

provideSpec model = do
  describe "provide" $ do
    -- on bouge le zombie une case en haut pour pas qu'il veuille uniquement aller vers le joueur
    let new_model = move model exZombie1 (Coord 1 2)
    it "Respecte l'invariant" $ do
      property $ prop_provide model (exZombie1)
    it "Retoune la liste pondérée des actions possibles pour une entite" $ do
      provide new_model (exZombie1) `shouldBe` [(20,N),(20,S),(20,M.E),(40,R)]


-- Verifie que decide preserve l'invariant et sa pré-condition
prop_decide :: Model -> [(Int, Ordre)] -> Entite -> Property
prop_decide model l entity =
  (modelInv model) && (decidePre model l entity) ==> property $ modelInv (decide model l entity)

decideSpec model = do
  describe "decide" $ do
    it "Preserve l'invariant" $ do
      property $ prop_decide model [(20,N), (20,M.E), (60,R)] (exZombie1)

modelSpec map = do
  -- carte
  let real_map = M.adjust (\_ -> Chest Closed (Just Treasure)) (Coord 9 5) (mapCells map)
  let portals = (Coord 18 2, Coord 6 8)
  let trap = Coord 12 1
  let sink = Coord 20 3
  let sink' = Coord 20 8
  let gm' = M.adjust (\_ -> Portal Activated (Just (fst portals))) (snd portals) $ M.adjust (\_ -> Portal Activated (Just (snd portals))) (fst portals) $ M.adjust (\_ -> Sink Desactivated (Just (Left portals))) sink real_map
  let real_map = M.adjust (\_ -> Trap Activated) trap $ M.adjust (\_ -> Sink Desactivated (Just (Right trap))) sink' gm'
  -- environnement
  let fct = (\coord cell init -> if coord == Coord 1 6 then (coord, [exJoueur1]):init else if coord == Coord 2 6 then (coord, [exZombie1]):init else (coord,[]):init)
  let exMapEnvironnement = M.fromList $ M.foldrWithKey fct [] real_map
  let exEnvironnement = Environnement exMapEnvironnement
  let exModel = Model (map{mapCells=real_map}) exEnvironnement (mkStdGen 1000) "" createKeyboard

  makeModelSpec exModel
  moveSpec exModel
  tourSpec exModel
  interactWithDoorSpec exModel
  interactWithSinkSpec exModel
  canMoveToSpec exModel
  canOpenSpec exModel
  possibleSolutionsSpec exModel
  provideSpec exModel
  decideSpec exModel
  treasureSpec exModel
  canAttackSpec exModel
  attackSpec exModel
  cleanAttackedSpec exModel

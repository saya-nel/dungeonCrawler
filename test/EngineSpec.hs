module EngineSpec where

import qualified Data.Map.Strict as M
import System.Random
import Test.Hspec
import Test.QuickCheck

import Model as M
import GameMap as GM
import Entite as Ent
import Environnement as Env
import Keyboard as K
import Engine as Eng

prop_makeEngine :: Model -> Property
prop_makeEngine model =
  property $ engineInv $ makeEngine model

makeEngineSpec model = do
  describe "makeModel" $ do
    it "Preserve l'invariant" $ do
      property $ prop_makeEngine model

prop_setEntRound :: State -> Property
prop_setEntRound state = (setEntRound_pre (env_round state)) && (engineInv state) ==> property $ engineInv $ state{ent_round=setEntRound $ env_round state}

setEntRoundSpec state = do
    describe "setEntRound" $ do
        it "Preserve l'invariant" $ do
            property $ prop_setEntRound state
        it "Renvoie la map des entités ent_round" $ do
            setEntRound (env_round state) `shouldBe` M.fromList [(1,Joueur 1 (Stats 10 10) False [] Unarmed),(2,Zombie 2 (Stats 10 10) False)]

prop_newState :: Int -> String -> Model -> Property
prop_newState round log model = (newState_pre round log model) ==> property $ engineInv $ newState round log model

newStateSpec round log model = do
    describe "newState" $ do
        it "Créer un état qui respecte son invariant" $ do
            property $ prop_newState round log model

prop_state_round :: State -> Keyboard -> Int -> Property
prop_state_round state kbd nb_round = (engineInv state) ==> property $ engineInv $ state_round state kbd nb_round

state_roundSpec state kbd nb_round = do
    describe "state_round" $ do
        it "Preserve l'invariant" $ do
            property $ prop_state_round state kbd nb_round


engineSpec map = do
  let real_map = M.adjust (\_ -> Chest Closed (Just Treasure)) (Coord 10 3) (mapCells map)
  let exJoueur = makeJoueur 1 (Stats 10 10) False [] Unarmed
  let exZombie = makeZombie 2 (Stats 10 10) False
  let fct = (\coord cell init -> if coord == Coord 1 6 then (coord, [exJoueur]):init else if coord == Coord 2 6 then (coord, [exZombie]):init else (coord,[]):init)
  let exMapEnvironnement = M.fromList $ M.foldrWithKey fct [] real_map
  let exEnvironnement = Environnement exMapEnvironnement
  let exModel = Model (map{mapCells=real_map}) exEnvironnement (mkStdGen 1000) "debut de la partie\n" createKeyboard
  let exEngine = makeEngine exModel

  makeEngineSpec exModel
  setEntRoundSpec exEngine
  newStateSpec ((Eng.round exEngine)+1) (log_round exEngine) exModel
  state_roundSpec exEngine (keyboard exModel) 1 --tour du joueur uniquement
  state_roundSpec exEngine (keyboard exModel) 30 --tour des ennemies + joueur
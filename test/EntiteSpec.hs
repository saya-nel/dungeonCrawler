module EntiteSpec where 

import Test.Hspec as Hs
import Test.QuickCheck as Q

import Entite as Ent

type Stuff = Maybe Entite

genStats :: Gen Stats
genStats = do
  vie <- choose (5,50)
  atk <- choose (1,5)
  return $ Stats vie atk

getState :: Gen (PlayerState,[Entite])
getState = do
  buff <- choose (1,5)
  choice <- arbitrary
  case choice of
    True -> return (Unarmed,[])
    False -> return $ (Armed buff,[Sword buff])

genEntite :: Gen Entite
genEntite = do
  stats <- genStats
  attacked <- arbitrary
  (state,inv) <- getState
  id <- choose (1,100000)
  who <- arbitrary
  case who of
    True -> return $ Joueur id stats attacked inv state
    False -> return $ Zombie id stats attacked



-- verifie que makeZombie respecte l'invariant
prop_makeZombie :: Int -> Stats -> Bool -> Property
prop_makeZombie id stats attacked = 
  property $ entiteInv (makeZombie id stats attacked)

makeZombieSpec = do
  describe "makeZombie" $ do
    it "Preserve l'invariant" $ do
      property $ prop_makeZombie 1 (Stats 10 10) False
    it "Retourne un Zombie" $ do
      makeZombie 1 (Stats 10 10) False `shouldBe` Zombie 1 (Stats 10 10) False

-- verifie que makeJoueur respecte l'invariant
prop_makeJoueur :: Int -> Stats -> Bool -> [Entite] -> PlayerState -> Property
prop_makeJoueur id vie attacked inventory state =
  property $ entiteInv $ makeJoueur id vie attacked inventory state

makeJoueurSpec = do
  describe "makeJoueur" $ do
    it "Preserve l'invariant" $ do
      property $ prop_makeJoueur 1 (Stats 10 10) False [] Unarmed
    it "Retourne un Joueur" $ do
      makeJoueur 1 (Stats 10 10) False [] Unarmed `shouldBe` Joueur 1 (Stats 10 10) False [] Unarmed
  
coherencyPlayerStateSpec joueur = do
  describe "coherencyPlayerState" $ do
    it "Le joueur a une épée dans son inventaire, son état est armée" $ do
      coherencyPlayerState_prop (expandInventory (Just (Sword 10)) joueur) `shouldBe` True
    it "Le joueur a pas d'épée dans son inventaire, son état est non armée" $ do
      coherencyPlayerState_prop joueur `shouldBe` True

prop_expandInventory :: Maybe Entite -> Entite -> Property
prop_expandInventory loot joueur = 
  (entiteInv joueur) ==> property $ entiteInv (expandInventory loot joueur)  

expandInventorySpec loot joueur = do
  describe "expandInventory" $ do
    it "Preserve l'invariant" $ do
      property $ prop_expandInventory loot joueur
    it "Le joueur ramasse une épée, et devient armée, l'épée est dans son inventaire" $ do
      expandInventory loot joueur `shouldBe` (Joueur 1 (Stats 10 10) False [Sword 50] (Armed 50))

entiteSpec = do
  let exJoueur = makeJoueur 1 (Stats 10 10) False [] Unarmed
  let exLoot = Just (Sword 50)
  makeZombieSpec
  makeJoueurSpec
  coherencyPlayerStateSpec exJoueur
  expandInventorySpec exLoot exJoueur
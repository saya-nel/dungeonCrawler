module Entite where 
import qualified Debug.Trace as T
type BonusAttack = Int

-- Etat du Joueur, peut etre Armé ou désarmé
data PlayerState =
  Unarmed
  | Armed BonusAttack deriving Eq

-- Stats des Entite vivantes
data Stats = Stats {vie :: Int, atk :: Int} deriving Eq

-- Entite du jeux
data Entite = 
  Zombie {id :: Int, stats :: Stats, attacked :: Bool} |
  Joueur {id :: Int, stats :: Stats, attacked :: Bool, inventory :: [Entite], state :: PlayerState} |
  Treasure |
  Sword BonusAttack |
  Key
  deriving (Eq)

-- Sert aussi pour les affichages graphiques
instance Show Entite where
    show (Joueur _ _ True _ Unarmed ) = "J'"
    show (Joueur _ _ _ _ Unarmed) = "J"
    show (Joueur _ _ True _ (Armed _)) = "J>'"
    show (Joueur _ _ _ _ (Armed _)) = "J>"
    show (Zombie _ _ attacked) = if attacked then "Z'" else "Z"
    show Treasure = "Tr"
    show (Sword bonus) = "Sw"
    show Key = "K"

-- smart constructors

-- constructeur Zombie
makeZombie :: Int -> Stats -> Bool -> Entite 
makeZombie id (Stats vie atk) attacked 
  | vie > 0 && atk > 0 = Zombie id (Stats vie atk) attacked
  | otherwise = error "Wrong Zombie"

-- constructeur Joueur
makeJoueur :: Int -> Stats -> Bool -> [Entite] -> PlayerState -> Entite 
makeJoueur id (Stats vie atk) attacked inventory state
  | vie > 0 && atk > 0 = Joueur id (Stats vie atk) attacked inventory state
  | otherwise = error "Wrong Joueur"

-- invariant :
-- 1 les vies doivent etre >= 0
-- 2 si le joueur est en état unarmed, l'épée ne doit pas etre contenue dans l'inventaire, 
--   si il est en état armed, l'épée doit etre contenu dans l'inventaire
-- 3 l'inventaire ne peut contenir que des entités immobiles (pas de Zombie ou de Joueur..)
entiteInv :: Entite -> Bool
entiteInv entity@(Joueur _ _ _ _ _) = (vie (stats entity)) >= 0 && coherencyPlayerState_prop entity && coherencyInventory entity
entiteInv entity@(Zombie _ _ _) = (vie (stats entity)) >= 0
entiteInv _ = True

-- 3eme proprietée de l'invariant décrite en haut
coherencyInventory :: Entite -> Bool
coherencyInventory (Joueur _ _ _ inv _) = and $ coherencyInventory_aux inv [] where
  coherencyInventory_aux [] isConsistent = isConsistent
  coherencyInventory_aux (x:xs) isConsistent = case x of
    (Joueur _ _ _ _ _) -> False : isConsistent
    (Zombie _ _ _) -> False : isConsistent
    otherwise -> True : isConsistent

-- 2eme proprietée de l'invariant décrite en haut
coherencyPlayerState_prop :: Entite -> Bool
coherencyPlayerState_prop (Joueur _ _ _ inv Unarmed) = not $ swordCoherency inv 0
coherencyPlayerState_prop (Joueur _ _ _ inv (Armed buff)) = swordCoherency inv buff

-- fonction auxilliaire coherencyPlayerState qui verifie si l'épée est dans l'inventaire, et si le buff de l'épée est égale a celui
-- du joueur
swordCoherency :: [Entite] -> Int -> Bool
swordCoherency [] buff = False
swordCoherency (x:xs) buff = case x of
  Sword bonus -> buff == bonus
  otherwise -> False

-- étend l'inventaire du personnage avec un objet qu'il a ramassé
expandInventory :: Maybe Entite -> Entite -> Entite
expandInventory (Just loot@(Sword buff)) joueur = joueur{inventory = (inventory joueur) ++[loot], state=(Armed buff)}
expandInventory (Just loot@(Treasure)) joueur = joueur{inventory = (inventory joueur) ++[loot]}
expandInventory (Just loot@(Key)) joueur = joueur{inventory = (inventory joueur) ++[loot]}
expandInventory Nothing joueur = joueur
expandInventory _ ent = ent

{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Semigroup
import Text.Printf

data Food = Food
  { _food_fat :: Double
  , _food_protein :: Double
  , _food_carb :: Double
  , _food_label :: [String]  -- TODO: Use multiplier (Int, String)
  }
  deriving (Eq, Show, Ord)

instance Semigroup Food where
  (<>) = addFood

instance Monoid Food where
  mempty = Food 0 0 0 mempty

-- TODO: Constraint mulFood on single food item only, and add the unit to description.
mulFood :: Food -> Double -> Food
mulFood (Food f p c s) n = Food (f*n) (p*n) (c*n) s

addFood :: Food -> Food -> Food
addFood (Food f1 p1 c1 s1) (Food f2 p2 c2 s2) = Food (f1 + f2) (p1 + p2) (c1 + c2) (s1 <> s2)

showFood :: Food -> IO ()
showFood food@(Food f p c s) = do
  printf "%s => %s\n\t| ratio: %.2f | cal: %.0f\n" (show s) (show food) (f / p) (f * 9.3 + p * 4.1 + c * 4.1)

data Dairy
  = Butter
  | CheddarCheese

data Egg
  = LargeEgg

data Meat
  = GroundBeefLaMauve

class IsFood a where
  getFood :: a -> Food
  getUnit :: a -> String

instance IsFood a => IsFood [a] where
  getFood = mconcat . map getFood
  getUnit = mconcat . map getUnit

butter = Food 92 1 0.1 ["Butter"] -- 1 stick
cheddarCheese = Food 33 25 1.3 ["Cheddar Cheese"] -- Per 100g
goatCheeseServing = mulFood cheddarCheese 0.43  -- 1/3rd of Agropur cheese
largeEgg = Food 4.8 6 0.4 ["Large egg"]
groundBeef = Food 20 17 0 ["Ground beef"]
tildaCookedRice = Food 4.8 7.2 70.4 ["Tilda rice"]
cashewsRoyalNut50g = Food 27 10 12 ["Cashews, Royal Nut"]
-- lambRibs = Food 34 15 0 ["Lamb belly"]
-- duckFat = Food 100 0 0 ["Duck fat"]
-- suet = Food 100 0 0 ["Suet"]
-- liver = Food 4.4 26 3.8 ["Liver"]

instance IsFood Dairy where
  getFood = \case
    Butter -> butter
    CheddarCheese -> undefined
  getUnit = \case
    Butter -> "1 stick"
    CheddarCheese -> undefined

instance IsFood Egg where
  getFood = \case
    LargeEgg -> Food 4.8 6 0.4 ["Large egg"]
  getUnit = \case
    LargeEgg -> "1 large egg"

lmGroundBeef = mulFood groundBeef 4.31
butterServing = mulFood butter $ fromRational (1/8)

riceCashewOmadMeal = mconcat
  [ lmGroundBeef <> butterServing
  , mulFood largeEgg 3 <> butterServing
  , mulFood cheddarCheese 0.25
  , tildaCookedRice
  , cashewsRoyalNut50g
  ]

carnivoreOmadMeal1 = mconcat
  [ lmGroundBeef <> butterServing
  , mulFood largeEgg 6 <> butterServing
  , mulFood cheddarCheese 0.25
  , goatCheeseServing
  ]

carnivoreOmadMeal2 = mconcat
  [ carnivoreOmadMeal1
  , mulFood largeEgg 3
  ]

main = do
  showFood riceCashewOmadMeal
  showFood carnivoreOmadMeal1
  showFood carnivoreOmadMeal2

----------------------------------------------------------------------
--    Stater File for HW6  (CIS 252: Spring 2017)
----------------------------------------------------------------------

-- possible pizza toppings
data Topping = Pepperoni | Onions | Ham | Mushrooms | Chicken
               deriving (Show)

-- sauces for pizzas and breadsticks
data Sauce = Tomato | Garlic | Pesto 
             deriving (Show)

-- sizes for pizzas and salads
data Size = Small | Large
            deriving (Show)

-- possible dressings for salads
data Dressing = Ranch | Greek | Caesar | None
                deriving (Show)

-- items on the menu for purchase
data MenuItem = Breadsticks Sauce
              | Salad Size Dressing
              | Pizza Size Sauce [Topping]
                deriving (Show)


----------------------------------------------------------------------
--  Do not make alterations to the code above this line.  All of your
--  code should appear below this comment.
----------------------------------------------------------------------

-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 5


-- Exercise 1a - order of breadsticks with pesto sauce
breadsticks1 :: MenuItem
breadsticks1 = Breadsticks Pesto

-- Exercise 1b - large salad with Caesar dressing
salad1 :: MenuItem
salad1 = Salad Large Caesar

-- Exercise 1c - small pizza with garlic sauce and no toppings
pizza1 :: MenuItem 
pizza1 = Pizza Small Garlic []

-- Exercise 1d - large pizza with tomato sauce, onions, a double-order of
--               pepperoni, and mushrooms
pizza2 :: MenuItem 
pizza2 = Pizza Large Tomato [Onions, Pepperoni, Pepperoni, Mushrooms]

-- Exercise 1e - Order of: large salad with no dressing; breadsticks with
--               garlic sauce; a large plain pizza with tomato sauce
order1 :: [MenuItem]
order1 = [Salad Large None, Breadsticks Garlic, Pizza Large Tomato []]

-- Exercise 1f - Order of: small pizza with tomato sauce, pepperoni,
--    mushrooms, ham and onions; small salad with ranch dressing; large salad
--    with Greek dressing; large pizza with pesto sauce, ham, and chicken;
--    breadsticks with garlic sauce
order2 :: [MenuItem]
order2 = [Pizza Small Tomato [Pepperoni, Mushrooms, Ham, Onions],
          Salad Small Ranch, Salad Large Greek,
          Pizza Large Pesto [Ham, Chicken],
          Breadsticks Garlic]


-- Exercise 2 - Calculates the price of a given topping for a given pizza size
toppingCost :: Size -> Topping -> Float
toppingCost Small Chicken = 2.50
toppingCost Small _       = 1.50
toppingCost Large Chicken = 3.25
toppingCost Large _       = 2.25


-- Exercise 3 - Calculates the price of a given sauce for a given pizza size
sauceCost :: Size -> Sauce -> Float
sauceCost Small Pesto = 13.25  -- Additional $1.75
sauceCost Small _     = 11.50
sauceCost Large Pesto = 16.50  -- Additional $2.50
sauceCost Large _     = 14.00


-- Exercise 4 - Changes order to switch large orders with smalls and remove
--              all salad dressing
cutCalories :: MenuItem -> MenuItem
cutCalories (Salad _ _)    = Salad Small None
cutCalories (Pizza _ s ts) = Pizza Small s ts
cutCalories item           = item


-- Exercise 5 - Determines whether Martha will eat a given order
willEat :: MenuItem -> Bool
willEat (Breadsticks Pesto) = False
willEat (Pizza _ Pesto _)   = False
willEat (Pizza _ _ ts)      = null [Mushrooms | Mushrooms <- ts]
willEat _                   = True


-- Exercise 6 - Calculates the price of a menu item
price :: MenuItem -> Float
price (Breadsticks _)  = 4.25
price (Salad Small _)  = 6.75
price (Salad Large _)  = 9.50
price (Pizza sz sa ts) = (sauceCost sz sa) + sum [toppingCost sz t | t <- ts]


-- Exercise 7 - Calculates the number of pizza toppings in an entire order
numToppings :: [MenuItem] -> Int
numToppings is = sum [length ts | (Pizza _ _ ts) <- is]


-- Exercise 8 - Calculates promotional price where anyone buying multiple menu
--              items will get cheapest one free
promotion :: [MenuItem] -> Float
promotion []   = 0.00
promotion [i]  = price i
promotion is   = (sum ps) - (minimum ps)
    where ps = [price i | i <- is]

-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 2

-- Exercise 1 - Compares number of characters equal to each other
compareChars :: Char -> Char -> Char -> String
compareChars a b c
    | a == b && b == c            = "All equal"
    | a == b || a == c || b == c  = "Two match"
    | otherwise                   = "All distinct"

-- Exercise 2 - Combines digits or -1 as an error if given a non-digit input
combine :: Int -> Int -> Int -> Int
combine x y z
    | isDigit x && isDigit y && isDigit z  = z + y*10 + x*100
    | otherwise                            = -1
    where isDigit d = d >= 0 && d <= 9

-- Exercise 3 - Splits a floating point into its whole-number and fractional parts
splitFloat :: Float -> (Integer, Float)
splitFloat num
    | remainder == 0  = (whole, 0.0)
    | num >= 0        = (whole, remainder)
    | otherwise       = (1 + whole, 1 - remainder)
    where whole     = floor num
          remainder = num - fromInteger whole

-- Exercise 4a - Calculates the cost for the standard-pricing plan
stdCost :: Int -> Int -> Bool -> Int
stdCost dev gb new
    | dev < 1 || gb < 0  =  -1
    | new                = plan - 25
    | otherwise          = plan
    where plan     = 50 + 10 * (dev - 1) + overage
          overage  = max 0 (15 * (gb  - 4))

-- Exercise 4b - Calculates the cost for the power-user plan
powerCost :: Int -> Int -> Bool -> Int
powerCost dev gb new
    | dev < 1 || gb < 0  =  -1
    | new                = plan - 25
    | otherwise          = plan
    where plan     = 80 + 8 * (dev - 1) + overage
          overage  = max 0 (20 * (gb - 10 - 2 * dev))

-- Exercise 4c - Determines cheapest plan between the standard-pricing and
--               power-user plans, or if they're the same cost
bestPlan :: Int -> Int -> String
bestPlan dev gb
    | std == power  = "Same cost"
    | std > power   = "Power User"
    | otherwise     = "Standard"
    where std    = stdCost dev gb False
          power  = powerCost dev gb False

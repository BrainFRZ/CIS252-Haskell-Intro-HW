-- Name:     Terry Weiss
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105

-- Problem 6: test if m is strictly between y and z
between :: Int -> Int -> Int -> Bool
between m y z = (y < m) && (m < z)

-- Problem 7: exclusive or
xor :: Bool -> Bool -> Bool
xor e1 e2 = e1 /= e2

-- Problem 8a: Delisle-Fahrenheit Converter
convertDtoF :: Float -> Float
convertDtoF temp = 212.0 - 1.2 * temp

-- Problem 8b: Fahrenheit-Delisle Converter
convertFtoD :: Float -> Float
convertFtoD temp = (212 - temp) / 1.2
{-main :: IO String
main = do line <- getLine
          return line-}

{-main :: IO Integer
main = do i <- (\l -> return (read l::Integer))
          return i
-}

getInteger :: IO Integer
getInteger = do line <- getLine
                return (read line :: Integer)

main :: IO Integer
main = do i <- getInteger
          return i

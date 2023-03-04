module Api0
  ( get0
  , check0
  ) where

get0 :: [String] -> IO String
get0 []      = pure "OK"
get0 [op, _] = case op of
  "title"  -> pure "Haskell In Depth"
  "year"   -> pure "2021"
  "rating" -> pure "Great"
  _        -> ioError (userError "Not implemented")
get0 _ = ioError (userError "Malformed request")

check0 :: IO ()
check0 = do
  b <- get0 []
  y <- get0 ["year", "7548"]
  putStrLn (if b == "OK" && y == "2021" then "OK" else "Wrong answer!")

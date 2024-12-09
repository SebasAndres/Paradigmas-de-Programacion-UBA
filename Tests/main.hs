trim :: String -> String
trim = recr (\x xs rec -> if x == ' ' then rec else x : xs) []
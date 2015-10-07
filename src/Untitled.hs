module Untitled where
    
fact i = if i <= 1 then 1 else i * fact (i - 1)

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "enter the number, which will assume the factorial"
    i <- getLine 
    putStrLn  i 
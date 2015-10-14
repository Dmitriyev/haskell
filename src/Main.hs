module Main where

fact i = if i <= 1 then 1 else i * fact (i - 1)
strfact sx =
    let param = read sx in
    show (fact param)

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "enter the number, which will assume the factorial"
    i <- getLine 
    putStrLn (strfact i)
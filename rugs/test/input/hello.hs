module Main where

main :: IO ()
main = do
        a <- do
                print "inside" 
                print "a do"
        print (1+2)
        putStrLn "hello"


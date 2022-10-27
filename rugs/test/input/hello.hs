module Main where

main :: IO ()
main = do
        a <- do
                print "inside"
                        "job"
        print (1+2)
        putStrLn "hello"


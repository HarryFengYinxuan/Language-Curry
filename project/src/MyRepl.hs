module MyRepl where

import Exec 
import Ast
import Parser
import ParserMonad
import System.Environment

replLetPar :: Parser String 
replLetPar = do 
    token $ literal "let" <|> return ""
    a <- varStr 
    token $ literal "="
    b <- token parser
    return ("let " ++ (a) ++ " = " ++ (show b) ++ " in ")

importPar :: Parser String 
importPar = do 
    token $ literal "import" -- <|> return ""
    a <- varStr 
    return (a ++ ".curry")

replPar :: Parser (Either String String)
replPar = do 
    s <- replLetPar <||> importPar
    case s of 
        Left s -> return (Left s)
        Right s -> return (Right s)


-- main :: String -> IO ()
-- main p = do
--     putStr ">>"
--     l <- getLine
--     case l of 
--         "" -> main p
--         "q" -> putStrLn "Terminating.."
--         _ -> case (parse replLetPar l) of 
--             Just (s, "") -> let newP = p ++ s in
--                 do 
--                     -- putStr "new p: "
--                     -- putStrLn newP
--                     main newP
--                     -- case (exec (newP++l)) of
--                     --     (Good v s) -> do 
--                     --         printList s
--                     --         main newP
--                     --     _ -> putStrLn "bad"
--             Just (s, _) -> putStrLn "repl let bad"
--             _ -> let newP = p in do 
--                     -- putStrLn (newP++l)
--                     case (exec (newP++l)) of
--                         (Good v s) -> do 
--                             printList s
--                             main newP
--                         _ -> putStrLn "bad"

main :: String -> IO ()
main p = do
    putStr ">>"
    l <- getLine
    case l of 
        "" -> main p
        "q" -> putStrLn "Terminating.."
        _ -> case (parse replPar l) of 
            Just (Left s, "") -> let newP = p ++ s in do 
                    -- putStr "new p: "
                    -- putStrLn newP
                    main newP
                    -- case (exec (newP++l)) of
                    --     (Good v s) -> do 
                    --         printList s
                    --         main newP
                    --     _ -> putStrLn "bad"
            Just (Left s, _) -> putStrLn "repl let bad"
            Just (Right f, "") -> do 
                d <- test f 
                case (d) of 
                    Just d -> main (p++d) 
                    Nothing -> main (p)
            _ -> do 
                case (exec (p++l)) of
                    (Good v s) -> do 
                        printList s
                        main p
                    _ -> do
                        putStrLn "Bad command"
                        main p
                
            
start :: IO ()
start = main ""
    

printList :: [String] -> IO ()
printList l = sequence_ (map putStrLn l)

-- importCur :: String -> IO String 
-- importCur f = do 
--     s <- readFile f 


allLetPar :: Parser String 
allLetPar = do 
    s <- replLetPar <|> return ""
    case s of 
        "" -> do return ""
        _ -> do 
            rest <- allLetPar
            return (s ++ rest)

test :: String -> IO (Maybe String)
test f = do 
    s <- readFile f
    -- putStrLn s 
    case (parse allLetPar s) of 
        (Just (s, "")) -> do
            -- putStrLn s 
            return $ Just s
        (Just (s, a)) -> do
            putStrLn ("Error with import: "++a)
            return $ Nothing
        Nothing -> do
            putStrLn "The file doesn't have definitions"
            return $ Nothing
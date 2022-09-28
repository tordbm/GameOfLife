-- Tord Berget Monclair, gruppe 2
import Data.List ( (\\), nub )
import Data.Char (isDigit)
import Control.Concurrent ( threadDelay ) 
import System.Exit ( exitSuccess )
import System.IO ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad ( when )

{-Oppgave 2.4 hopper jeg over da jeg ikke vet om noen måte å lage en glider uten å prøve å feile i lang tid, noe jeg ikke har.
Ellers funker alt som det skal, ut i fra det jeg har testet. Ikke alle steder jeg har lagt inn feilmeldinger til brukeren, 
men programmet fikser alle problemer jeg kan tenke meg selv i bakgrunnen så feil input blir tatt hånd om. 
Burde kanskje lagt til noen flere meldinger, men hadde ikke tid.

Har valgt å bare definere en type Cell. Kunne/tenkte å lage en type State som et tuppel med all info,
men følte ikke det var verdt det i forhold til tiden det ville tatt å konvertere det jeg har. Ville nok sett litt ryddigere ut
-}
type Cell = (Int, Int)

main = do
    putStrLn "\nWelcome to my version of \"The Game of Life\":)"
    c <- getLine
    let input = words c
    case input of
        ("c":x:_) -> do
            let size = read x
            when (read x > 99 || read x <= 0) $
             putStrLn $ "Invalid size "++x++". Size must be between 1 and 99"
            board size
            run size (1,1) (1,1) []
        ("r":x:_) -> do
            theFile <- openFile x ReadMode
            content <- hGetContents theFile
            let contents = words $ filter (not . (`elem` "(,)")) content
                size = read (head contents) :: Int
                (s1, s2) = getSurviver $ tail contents
                (b1, b2) = getBorn $ tail contents
                livingCells = getCells cellsToString size
                cellsToString = evenLength $ drop 7 contents
            board size
            placeAll cellsToString size
            hClose theFile
            run size (s1, s2) (b1, b2) livingCells
        ("quit":_) -> exitSuccess
        _ -> do
            putStrLn "Cannot initiate board as no valid command was given. Please enter a valid command."
            main

board :: Int -> IO ()
board size = putStr $ createBoard size

createBoard :: Int -> String
createBoard size = let
    firstRow = concat [show i ++ spaces2 i | i <- [1..size]]
    rad = concat $ replicate size "  ." in
        "    " ++ firstRow ++  "\n" ++
        concat [show i ++ spaces i ++ rad ++ "\n"| i <- [1..size]]
    where 
        spaces i  | i >= 10 = ""
                  | otherwise = " "
        spaces2 i | i >= 9 = " "
                  | otherwise = "  "

getSurviver, getBorn ::[String] -> (Int, Int)
getSurviver (x:y:z:xs) | x == "s" = (read y, read z)
                       | otherwise = getSurviver xs

getBorn (x:y:z:xs) | x == "b" = (read y, read z)
                   | otherwise = getBorn xs

run :: Int -> (Int, Int) -> (Int, Int) -> [Cell] -> IO ()
run size (s1,s2) (b1,b2) livingCells = do
    line <- getLine
    goXup 1
    deleteLineAtCursor
    let lineWords = words line
    if null lineWords then do
        --Vet at generations vises feil ved kjøring ved enter, men føler ikke det er verdt å fikse grunnet tid. Live mode funker
        runGame size 1 (s1,s2) (b1,b2) livingCells 1 0
    else if not $ all isDigit $ concat $ tail lineWords then do
        goXdown 1
        deleteLineAtCursor
        putStrLn "All input after command must be of type Int"
        goXup 2
        run size (s1,s2) (b1,b2) livingCells 
    else do
    case lineWords of 
        ("quit":_) -> putStrLn "Thanks for playing! The game is over" >> exitSuccess
        ("n":xs) -> do 
            let newLivingCells = nub (getCells (evenLength xs) size ++ livingCells)
            placeAll (getString newLivingCells size) size
            run size (s1,s2) (b1,b2) newLivingCells
        ("e":xs) -> do 
            let newLivingCells =  livingCells \\ deadCells
                deadCells = getCells (evenLength xs) size
            deleteAll (getString deadCells size) size
            run size (s1,s2) (b1,b2) newLivingCells
        ("w":_) -> do
            goXdown 1
            deleteLineAtCursor
            putStrLn $ "The living cells are: "++ show livingCells
            goXup 2
            run size (s1,s2) (b1,b2) livingCells 
        ("s":m:n:_) ->
            run size (read m, read n) (b1, b2) livingCells
        ("b":m:n:_) ->
            run size (s1,s2) (read m, read n) livingCells 
        ("?":_) -> do
            goXdown 1
            deleteLineAtCursor
            putStrLn $ "The current rules of the game are: to Survive: "++show s1++", "++show s2++" and to be Born: "++show b1++", "++show b2
            goXup 2
            run size (s1,s2) (b1,b2) livingCells
        ("l":g:_) -> do
            let gen = read g
            runGame size 1 (s1,s2) (b1,b2) livingCells gen 1000000
        _ ->  do
            goXdown 1
            deleteLineAtCursor
            putStrLn "No valid input given"
            goXup 2
            run size (s1,s2) (b1,b2) livingCells   
    
runGame :: Int -> Int ->(Int, Int) -> (Int, Int) -> [Cell] -> Int -> Int -> IO ()
runGame size _ (s1,s2) (b1,b2) livingCells 0 _ = run size (s1,s2) (b1,b2) livingCells
runGame size c (s1,s2) (b1,b2) livingCells gen delay = do
    deleteLineAtCursor
    let constLivingCells = livingCells
        constEmptyCells = emptyCells
        liveNeighbours = findAllLiveNeighbours livingCells constLivingCells size
        emptyCells = concat (findAllEmptyNeighbours livingCells constLivingCells size)
        deadCells = wontSurvive liveNeighbours livingCells (s1,s2)
        birthCells = giveBirth emptyCells constEmptyCells (b1, b2)
        newLivingCells = nub ((livingCells \\ deadCells) ++ birthCells)
        liveToString = getString birthCells size
        deadToString = getString deadCells size
    stableCheck size c (s1, s2) (b1, b2) constLivingCells livingCells newLivingCells
    placeAll liveToString size
    deleteAll deadToString size
    threadDelay delay
    runGame size (c+1) (s1,s2) (b1,b2) newLivingCells (gen-1) delay

getCells :: [String] -> Int -> [Cell]
getCells [] _ = []
getCells (x:y:xs) size | read x > size || read y > size = getCells xs size
                       | read x <= 0 || read y <= 0 = getCells xs size
                       | otherwise = (read x, read y) : getCells xs size
getString :: [Cell] -> Int -> [String]
getString cells size = words $ concat [show x ++ " " ++ show y ++ " "| (x,y) <- cells, x <= size && x > 0, y <= size && y > 0]

stableCheck :: Int -> Int -> (Int, Int) -> (Int, Int) -> [Cell] -> [Cell] -> [Cell] -> IO ()
stableCheck size gen (s1, s2) (b1, b2) livingCells xs ys = 
                    when (xs == ys) $  
                        do 
                        goXdown 1
                        deleteLineAtCursor
                        putStrLn $ "Stable Configuration Reached in Generation "++ show gen++"!" 
                        goXup 2
                        run size (s1,s2) (b1,b2) livingCells

wontSurvive :: [[Cell]] -> [Cell] -> (Int, Int) -> [Cell]
wontSurvive [] _ _ = []
wontSurvive _ [] _ = []
wontSurvive (x:neighbours) (z:livingCells) (s1, s2)| length x >= s1 && length x <= s2 = wontSurvive neighbours livingCells (s1, s2)
                                                   | otherwise = z : wontSurvive neighbours livingCells (s1, s2)
giveBirth ::  [Cell] -> [Cell] -> (Int, Int) -> [Cell]
giveBirth [] _ _ = []
giveBirth (x:emptyCells) empty (b1, b2) | count x empty >= b1 && count x empty <= b2 = x : giveBirth emptyCells empty (b1, b2)
                                        | otherwise = giveBirth emptyCells empty (b1, b2)

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

isAlive :: Foldable t => Cell -> t Cell -> Bool
isAlive cell state = cell `elem` state
                                                   
findAllLiveNeighbours, findAllEmptyNeighbours :: [Cell] -> [Cell] -> Int ->[[Cell]]
findAllLiveNeighbours [] _ _ = []
findAllLiveNeighbours (cell:livingCells) live size = findLiveNeighbours cell live size : findAllLiveNeighbours livingCells live size

findAllEmptyNeighbours [] _ _ = []
findAllEmptyNeighbours (cell:livingCells) live size = findEmptyNeighbours cell live size : findAllEmptyNeighbours livingCells live size

findLiveNeighbours, findEmptyNeighbours :: Foldable t => Cell -> t Cell -> Int -> [Cell]
findLiveNeighbours (x,y) live size = [(i,j) | i <- [x-1..x+1], j <- [y-1..y+1], (i,j) /= (x,y), isAlive (i,j) live, i > 0 && i <= size, j > 0 && j <= size]

findEmptyNeighbours (x,y) live size = [(i,j) | i <- [x-1..x+1], j <- [y-1..y+1], (i,j) /= (x,y), not $ isAlive (i,j) live, i > 0 && i <= size, j > 0 && j <= size]

evenLength :: [a] -> [a]
evenLength xs = if odd $ length xs then init xs else xs

placeAll, deleteAll :: [String] -> Int -> IO ()
placeAll  [] _ = return ()
placeAll (x:y:xs) size = addO size (read x) (read y) >> placeAll xs size

deleteAll [] _ = return ()
deleteAll (x:y:xs) size = delO size (read x) (read y) >> deleteAll xs size

doActionAt :: String -> Int -> Int -> Int -> IO()
doActionAt act size x y = do
        let mx = size - x + 1
            my = y * 3 + 2
        goXup mx
        goYright my
        putStr act 
        goXdown mx
        goYleft my

goXup, goXdown, goYleft, goYright :: Show a => a -> IO ()
goXup x = putStr $ "\ESC[" ++ show x ++ "A"
goXdown x = putStr $ "\ESC[" ++ show x ++ "B"
goYright y = putStr $ "\ESC[" ++ show y ++ "C"
goYleft y = putStr $ "\ESC[" ++ show y ++ "D"

deleteLineAtCursor, clr :: IO ()
deleteLineAtCursor = putStr "\ESC[2K"
clr = putStr "\ESC[2J"

addO, delO :: Int -> Int -> Int -> IO ()
addO = doActionAt "\BSO"
delO = doActionAt "\BS."
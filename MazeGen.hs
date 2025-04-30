module MazeGen (
    Cell,
    Maze,
    gridMaze,
    generateMazeIO,
    showMaze,
    getWalls,
    cellIsTrue,
    testMaze3x3,
    testMaze6x6,
    testMaze10x10,
    setCellState,
    showSolutionMaze
)
where




import Data.Char
import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

--Estructura Cell 
--Paredes  l r u d - visitated
data Cell = C Bool Bool Bool Bool Bool
    deriving Show
    
type Maze = [[Cell]]



-- Muestra un laberinto
showMaze :: Maze -> String
showMaze m = showMaze' m (length m)

showMaze' :: Maze -> Int -> String
showMaze' [] n = '¯' : (concat ((replicate n "¯¯¯¯¯")))
showMaze' (r:rs) n = ('|' : (tail (showUpLine r))) ++ "|\n" ++ (showLeft r) ++ "|\n" ++ (showMaze' rs n)

-- Muestra las paredes de arriba del laberinto
showUpLine :: [Cell] -> String
showUpLine [] = []
showUpLine (x:xs) = (c x) ++ showUpLine(xs)
    where
        c :: Cell -> String
        c (C _ _ True _ _) = "-----"
        c _ = "     "

-- Muestra las paredes izquierdas del laberinto
showLeft :: [Cell] -> String
showLeft [] = []
showLeft (x:xs) = (c x) ++ showLeft(xs)
    where
        c :: Cell -> String
        c (C True _ _ _ _) = "|    "
        c _ = "     "

-- Muestra un laberinto con su solucion
--                laberinto -- Camino
showSolutionMaze :: Maze -> [(Int, Int)] -> String
showSolutionMaze maze path = showSolutionMaze' maze (length maze) path (-1)

showSolutionMaze' :: Maze -> Int -> [(Int, Int)] -> Int -> String
showSolutionMaze' [] n _ _ = '¯' : (concat ((replicate n "¯¯¯¯¯")))
showSolutionMaze' (r:rs) n path nrow = ('|' : (tail (showUpLine r))) ++ "|\n" ++ (showLeftSolution r path (nrow + 1) 0) ++ "|\n" ++ (showSolutionMaze' rs n path (nrow+1))

showLeftSolution :: [Cell] -> [(Int, Int)] -> Int -> Int -> String
showLeftSolution [] _ _ _ = []
showLeftSolution (c:cs) path row column = cell ++ (showLeftSolution cs path row (column+1))
    where
        cell = (w c) :' ': ch : "  " 
        ch :: Char
        ch
            | elem (row, column) path = '*'
            | otherwise = ' '

        w :: Cell -> Char
        w (C True _ _ _ _) = '|'
        w _ = ' '


-- Devueleve un laberinto con todas las paredes (Devuelve una grilla)
gridMaze :: Int -> Maze
gridMaze n = replicate n (replicate n (C True True True True False))

-- Devuelve un laberinto
--               tamaño
generateMazeIO :: Int -> IO Maze
generateMazeIO n = do
    x <- randomRIO (0, n-1)
    y <- randomRIO (0, n-1)
    key <- newStdGen
   
    return $ generateMaze' (gridMaze n) (y, x) n key


--        Laberinto --  Actual  -- Tamaño -- LLave aleatoria  
generateMaze' :: Maze -> (Int, Int) -> Int -> StdGen -> Maze
generateMaze' maze pAct limit key 
    | null neighboors = maze'
    | otherwise       = foldr (\pNei m -> generateMaze' (delWall m pAct pNei) pNei limit nkey) maze' neighboors
        where
            maze' = (setCellState maze pAct True)
            (neighboors, nkey) = shuffleList (getUnViNeigh maze' pAct limit) key
    

                    
-- Elimina la pared entre dos celdas                 
--         maze -- celda 1 -- celda 2 
delWall :: Maze -> (Int, Int) -> (Int, Int) -> Maze
delWall m (ya, xa) (yn, xn)

    | cellIsTrue m (yn, xn) = m
    --Down
    | yn > ya = (delWall' (delWall' m (ya, xa) 'D' ) (yn, xn) 'U' )

    -- Up
    | yn < ya = (delWall' (delWall' m (ya, xa) 'U' ) (yn, xn) 'D' )

    --Right
    | xn > xa = (delWall' (delWall' m (ya, xa) 'R' ) (yn, xn) 'L' )

    --Left
    | otherwise = (delWall' (delWall' m (ya, xa) 'L' ) (yn, xn) 'R' )
    -- xn < xa


delWall' :: Maze -> (Int, Int) -> Char -> Maze
delWall' m (y, x) char = ri ++ ((ci ++ (cell': (tail cf))) : tail rf)
    where
        (ri, rf) = splitAt y m
        (ci, cf) = splitAt x (head rf)
        cell' = rmWall (head cf) char
        

        rmWall :: Cell -> Char -> Cell
        rmWall (C l r u d s) c
            | c == 'U' = (C l r False d s)
            | c == 'D' = (C l r u False s)
            | c == 'R' = (C l False u d s)
            | c == 'L' = (C False r u d s)


-- Mezcla los elementos de una lista y devuelve la lista mezclada junto con una nueva llave
shuffleList :: [a] -> StdGen -> ([a],StdGen)
shuffleList xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
                (a,s') <- liftM (randomR lohi) (readSTRef g)
                writeSTRef g s'
                return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
    where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

    --Fuente: https://wiki.haskell.org/Random_shuffle
    

-- Regresa los vecinos no visitados de una celda
--              Maze -- Posicion -- Limite
getUnViNeigh :: Maze -> (Int, Int) -> Int -> [(Int, Int)]
getUnViNeigh maze (y,x) lim = filter notVisited lista
    where
        lista = [ (y, x + 1) | x + 1 < lim ] ++ [ (y, x - 1) | x - 1 >= 0 ] ++ [ (y + 1, x) | y + 1 < lim ] ++ [ (y - 1, x) | y - 1 >= 0 ]
        notVisited :: (Int,Int) -> Bool
        notVisited (y, x) =  not (cellIsTrue maze (y,x)) 


--------------------------------------------------------------------------------------------------------------------------------------------------
-- Regresa el valor si la celda es True
cellIsTrue :: Maze -> (Int, Int) -> Bool
cellIsTrue m  (y, x) = v
    where
        (C _ _ _ _ v) = (m !! y !! x)

-- Regresa una tupla de booleanos simbolizando las paredes del laberinto
getWalls :: Cell -> (Bool, Bool, Bool, Bool)
getWalls (C l r u d _) = (l, r, u, d)

-- Establece un valor a una celda dentro un laberinto
setCellState :: Maze -> (Int, Int) -> Bool -> Maze
setCellState m (y, x) valBool = ri ++ ((ci ++ (cell': (tail cf))) : tail rf)
    where
        (ri, rf) = splitAt y m
        (ci, cf) = splitAt x (head rf)
        cell' =  (\(C l r u d _) -> (C l r u d valBool)) (head cf) 


t :: Bool
t = True

f :: Bool
f = False

testMaze6x6 ::Maze
testMaze6x6 = [
    [(C t t t f t), (C t f t f t), (C f t t f t), (C t f t f t), (C f t t f t), (C t t t f t)],
    [(C t f f f t), (C f t f t t), (C t t f f t), (C t t f t f), (C t f f f t), (C f t f f t)],
    [(C t f f f t), (C f f t t t), (C f f f t t), (C f f t f t), (C f f f t t), (C f t f f t)],
    [(C t f f f t), (C f t t t t), (C t t t f t), (C t f f t t), (C f t t t t), (C t t f f t)],
    [(C t f f f t), (C f f t f t), (C f t f f t), (C t f t f t), (C f f t t t), (C f t f f t)],
    [(C t t f t t), (C t t f t t), (C t f f t t), (C f f f t t), (C f f t t t), (C f t f t t)]
    ]

testMaze3x3 ::Maze
testMaze3x3 = [
    [(C t f t f t), (C f f t t t), (C f t t f t)],
    [(C t t f f t), (C t f t f t), (C f t f t t)],
    [(C t f f t t), (C f f f t t), (C f t t t t)]
    ]

testMaze10x10 ::Maze
testMaze10x10 = [
    [(C t f t f t), (C f f t t t), (C f f t f t), (C f t t f t), (C t f t f t), (C f f t f t), (C f f t f t), (C f t t f t), (C t f t t t), (C f t t f t)],
    [(C t t f f t), (C t f t f t), (C f f f t t), (C f t f f t), (C t t f f t), (C t f f t t), (C f t f t t), (C t f f f t), (C f f t t t), (C f t f f t)],
    [(C t f f f t), (C f t f f t), (C t f t t t), (C f f f f t), (C f f f t t), (C f f t f t), (C f f t f t), (C f t f f t), (C t f t f t), (C f t f f t)],
    [(C t f f t t), (C f f f f t), (C f f t t t), (C f f f f t), (C f t t f t), (C t t f f t), (C t f f t t), (C f f f t t), (C f f f f t), (C f t f f t)],
    [(C t t t f t), (C t f f f t), (C f f t f t), (C f f f t t), (C f t f t t), (C t f f f t), (C f f t t t), (C f t t f t), (C t f f f t), (C f t f f t)],
    [(C t t f f t), (C t f f t t), (C f f f f t), (C f f t f t), (C f t t f t), (C t t f f t), (C t t t f t), (C t f f t t), (C f f f f t), (C f t f f t)],
    [(C t f f f t), (C f t t f t), (C t t f f t), (C t f f t t), (C f f f f t), (C f t f f t), (C t f f f t), (C f t t f t), (C t t f t t), (C t t f f t)],
    [(C t f f t t), (C f f f f t), (C f f f f t), (C f t t f t), (C t t f t t), (C t f f f t), (C f t f f t), (C t f f t t), (C f f t t t), (C f t f t t)],
    [(C t t t f t), (C t f f f t), (C f f f f t), (C f t f t t), (C t f t f t), (C f f f f t), (C f t f t t), (C t f t f t), (C f f t f t), (C f t t f t)],
    [(C t f f t t), (C f t f t t), (C t t f t t), (C t f t t t), (C f f f t t), (C f f f t t), (C f f t t t), (C f f f t t), (C f t f t t), (C t t f t t)]
    ]
import System.Random
import AStar
import MazeGen

-- Regresa un laberinto
--      Opcion -- tamaño
getMaze :: Int -> Int -> IO Maze
getMaze n s 
    | n == 1 =  do return (testMaze3x3)
    | n == 2 = do return (testMaze6x6)
    | n == 3 = do return (testMaze10x10)
    | otherwise = generateMazeIO s 
      

-- Recibe una opcion y devuelve el tamaño del laberinto
getSize :: Int -> IO Int
getSize 4 = do
    putStrLn "Ingrese un tamaño:"
    size <- getLine
    return (read size)
getSize 1 = return 3
getSize 2 = return 6
getSize 3 = return 10

-- Develve una tupla de coordenadas
--              limite
getCoordenadas :: Int -> IO (Int, Int)
getCoordenadas tam = do
    putStrLn "Y: "
    yStr <- getLine
    putStrLn "X: "
    xStr <- getLine

    let x = read xStr :: Int
    let y = read yStr :: Int

    if isValidCoord tam x && isValidCoord tam y
        then return (y, x)
        else do
            putStrLn "Las coordenadas son invalidas, ingrese coordenadas correctas"
            getCoordenadas tam

-- Verifica si el valor ingresado es mayor a 0 y esta dentro el limite
isValidCoord :: Int -> Int -> Bool
isValidCoord tam coord = coord >= 0 && coord < tam



menu :: IO ()
menu = do

    putStrLn "RESOLVEDOR DE LABERINTOS CON A*"
    putStrLn "-------------------------------"
    putStrLn "Seleccione una opción:\n\n\t1. Laberinto Prueba 3x3\n\t2. Laberinto Prueba 6x6\n\t3. Laberinto Prueba 10x10\n\t4. Generar Laberinto"
    
    op <- getLine
    let opcion = read op :: Int
    if opcion <= 4 && opcion >= 1
        then
            do
                size <- getSize opcion
                maze <- (getMaze opcion size)
                let mazeStr = (showMaze maze)

                -- Mostrar Maze
                putStrLn (mazeStr)

                -- Obtener coordenadas
                putStrLn "Ingrese Coordenadas del INICIO"
                inicio <- getCoordenadas size
                putStrLn "Ingrese Coordenadas del FINAL"
                final <- getCoordenadas size
                
                -- Mostrar solucion
                let (solucion, nodos) = (aStar maze inicio final)
                putStrLn (show solucion)
                let mazeSolution = (showSolutionMaze  maze solucion)
                putStrLn (mazeSolution)
                putStrLn "-"

        else
            do
                putStrLn "\n##### Opción incorrecta #####\n\n"
                menu
        
    



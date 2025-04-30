module AStar
(
    aStar
)
where


import MazeGen
import NPQueue



-- Recibe un laberinto una posicion de inicio y una posicion Final y devuelve los pasos necesaros para llegar a la meta
--         Maze      INICIO        FIN          PASOS
aStar :: Maze -> (Int, Int) -> (Int, Int) -> ([(Int, Int)], [Nodo])
aStar m i f = aStar' m  f (enQueue (convertNodo i f 0 i) emptyQueue) [] 



-- LOGICA ALGORITMO A* 

-- Pop de la cola
-- Si elemento es la meta:
--      Mostrar camino a la meta
-- Sino:
--      Añadir nodo a  lista visitados
--      Marcar Casilla como visitada
--      Buscar vecinos accesibles no visitados
--      Crear nodos vecinos y añadirlos cola
--      Recursividad con nuevos valores


-- Funcion Auxiliar A*
-- Devuleve el camino al final y la lista de nodos visitados

--         Maze        FIN         Cola      Visitados     
aStar' :: Maze -> (Int, Int) -> PQueue -> [Nodo] -> ([(Int, Int)], [Nodo])
aStar' m  fin pq visitados 
    | posTop == fin = (reverse(mostrarCamino (visitados') posTop), reverse visitados') --Mostrar Pasos
    | otherwise     = aStar' m' fin pq'' visitados' 
    where
        (top, pq') = deQueue pq
        visitados' = top:visitados

        posTop = getPosNodo top

        m' = setCellState m posTop False
        vecinos = getVecinos m posTop
        
        g = getGNodo top
        nodos = createNodos vecinos fin (g+1) posTop
        pq'' = enQueueList nodos pq'



-- Regresa la lista de vecinos accesibles no visitados de una posicion
--            Maze    posicion        
getVecinos :: Maze -> (Int, Int) -> [(Int, Int)]
getVecinos maze (y, x)  = filter (\pos -> cellIsTrue maze pos) ([(y-1, x) | not u] ++  [(y+1, x) | not d] ++ [(y, x-1) | not l] ++ [(y, x+1) | not r])
    where
        (l, r, u, d) = (getWalls (maze !! y !! x))

-- Regresa una lista de Nodos 
--           Posicion Actual -- Posicion Final -- Valor G -- Predecesor
createNodos :: [(Int, Int)] -> (Int, Int) -> Int -> (Int, Int) -> [Nodo]
createNodos [] _ _ _ = []
createNodos (x:xs) f g pred = (convertNodo x f g pred) : (createNodos xs f g pred)


--Recibe una lista de nodos y devuelve el camino desde la meta al inicio
--         Lista Nodos -- Posicion Buscada       
mostrarCamino :: [Nodo] -> (Int, Int) -> [(Int, Int)]
mostrarCamino (n:ns) bus
    | act == pred = [act]
    | act == bus  = act : (mostrarCamino ns pred)
    | otherwise   = (mostrarCamino ns bus)
    where
        (act, pred) = getPosPredNodo n









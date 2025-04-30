--Cola de Prioridad para Nodos

module NPQueue 
(
    PQueue,
    Nodo,
    convertNodo,
    emptyQueue,
    enQueue,
    deQueue,
    enQueueList,
    manDistance,
    getPosNodo,
    getPosPredNodo,
    getGNodo
)
where
--Hacer Eq y Ord Nodo

-- Tipo de Dato Nodo
-- Contiene -> Posicion Actual (x,y) -- Calculo Heuristico (H) -- Calculo Total (F)  -- Nodo Predecesor

data Nodo = N (Int, Int) Int Int (Int, Int)
    deriving (Show)

-- Convierte datos en un nuevo nodo
--       Posicion Actual -- Posicion Meta -- G -- Predecesor

instance Ord Nodo where
    (N _ h1 f1 _) > (N _ h2 f2 _) = f1 > f2 || (f1 == f2 && h1 > h2)
    (N _ h1 f1 _) < (N _ h2 f2 _) = f1 < f2 || (f1 == f2 && h1 < h2)
    (N _ h1 f1 _) >= (N _ h2 f2 _) = f1 >= f2 || (f1 == f2 && h1 >= h2)
    (N _ h1 f1 _) <= (N _ h2 f2 _) = f1 <= f2 || (f1 == f2 && h1 <= h2)
    compare (N _ h1 f1 _) (N _ h2 f2 _)
        | f1 /= f2 = compare f1 f2
        | otherwise = compare h1 h2

instance Eq Nodo where
    (N _ h1 f1 _) == (N _ h2 f2 _) = h1 == h2 && f1 == f2


--        Posicion Actual - Posicion destino - funcion g - Predecesor
convertNodo :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int) -> Nodo
convertNodo posi posf g pred =  N posi h (g+h) pred 
    where
        h = (manDistance posi posf)

-- Regresa el valor G de un nodo
getGNodo :: Nodo -> Int
getGNodo (N _ h f _) = (f - h)

-- Regresa la posicion de un Nodo
getPosNodo :: Nodo -> (Int, Int)
getPosNodo (N pos _ _ _) = pos

--Regresa la posicion actual de un nodo y la de su predecesor
getPosPredNodo :: Nodo -> ((Int,Int), (Int,Int))
getPosPredNodo (N pos _ _ pred) = (pos, pred)

--Calcula la distancia de Manhattan entre dos puntos
manDistance :: (Int, Int) -> (Int, Int) -> Int
manDistance (y1, x1) (y2, x2) = abs(y1-y2) + abs(x1-x2)

-- Cola de prioridad de Nodos
data PQueue = Q [Nodo]
    deriving (Show, Eq)

-- Cola Vacia
emptyQueue :: PQueue
emptyQueue = Q []

-- Encolar
enQueue :: Nodo -> PQueue -> PQueue
enQueue newNodo (Q []) = (Q [newNodo])
enQueue newNodo (Q ln) = (Q (enQueue' newNodo ln))
   
enQueue' :: Nodo -> [Nodo] -> [Nodo]
enQueue' new [] = [new]
enQueue' new ln@(n:ns)
    | new < n = (new:ln) 
    | otherwise = n : (enQueue' new ns)

-- Descencolar
deQueue :: PQueue -> (Nodo, PQueue)
deQueue (Q []) = error "Empty Queue"
deQueue (Q (x:xs)) = (x, (Q xs))

-- Recibe una lista de nodos y los encola
enQueueList :: [Nodo] -> PQueue -> PQueue
enQueueList [] p = p
enQueueList (x:xs) p = enQueueList xs (enQueue x p) 



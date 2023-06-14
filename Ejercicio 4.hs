import Data.List
import Data.Ord
import System.Environment (getArgs)
import System.IO (readFile, writeFile)


-- tipo de dato de arbol de Huffman
data ArbolHuffman = Hoja Char Int
                    | Nodo Int ArbolHuffman ArbolHuffman
  deriving (Show, Read)
  
-- Funcion para plegar arbol de Huffman
plegarArbol :: (Char -> Int -> b) -> (Int -> b -> b -> b) -> ArbolHuffman -> b
plegarArbol transHoja transNodo = plegar
    where
        plegar (Hoja c freq) = transHoja c freq
        plegar (Nodo freq izq der) = transNodo freq (plegar izq) (plegar der)

-- Funcion que devuelve la frecuencia de un tipo ArbolHuffman
frecuencia :: ArbolHuffman -> Int
frecuencia = plegarArbol transHoja transNodo
    where
        transHoja = (\_ freq -> freq)
        transNodo = (\freq _ _ -> freq)

-- Funcion que une dos arboles de huffman
combinar :: ArbolHuffman -> ArbolHuffman -> ArbolHuffman
combinar t1 t2 = Nodo (frecuencia t1 + frecuencia t2) t1 t2

-- Funcion que crea diccionario de frecuencias
frecuencias :: String -> [(Char, Int)]
frecuencias txt = map (\x -> (head x, length x)) . group . sort $ txt

-- Funcion para crear hojas a partir del diccionario
crearHojas :: [(Char, Int)] -> [ArbolHuffman]
crearHojas frecs = map (\(c, f) -> Hoja c f) frecs

-- Funcion para ordenar hojas de menor frecuencia a mayor frecuencia
ordenarHojas :: [ArbolHuffman] -> [ArbolHuffman]
ordenarHojas nodos = sortBy (comparing frecuencia) nodos

-- Fucnion para crear lista de hojas ordenadas dado un string
construirListaHuffman :: String -> [ArbolHuffman]
construirListaHuffman texto =
    let frecs = frecuencias texto
        hojas = crearHojas frecs
        hojasOrdenadas = ordenarHojas hojas
    in hojasOrdenadas

-- Función para construir el arbol Huffman a partir de las hojas ordenadas
construirArbolHuffman :: [ArbolHuffman] -> ArbolHuffman
construirArbolHuffman [nodo] = nodo
construirArbolHuffman nodos =
    let sorted = ordenarHojas nodos
        nodo1 = head sorted
        nodo2 = head (tail sorted)
        candidatos = drop 2 sorted
        nuevoNodo = combinar nodo1 nodo2 
        nuevoCandidatos =  [nuevoNodo] ++ candidatos   
    in construirArbolHuffman nuevoCandidatos

-- generar codificacion Huffman de un arbol dado
codificacionHuffman :: ArbolHuffman -> [(Char, String)]
codificacionHuffman arbol = plegarArbol hoja nodo arbol []
    where
        hoja c _ camino = [(c, reverse camino)]
        nodo _ izq der camino = izq ('0':camino) ++ der ('1':camino)

-- codifica un string dado un diccionario de un arbol
codificar :: String -> [(Char, String)] -> String
codificar texto codificacion = 
    let
        codificarCaracter c = case lookup c codificacion of
                                Just codigo -> codigo
                                Nothing     -> error "Carácter no encontrado en la codificación"
    in concatMap codificarCaracter texto

-- decodifica un string dado un diccionario de un arbol
decodificar :: String -> [(Char, String)] -> String
decodificar texto codificacion = decodificarAux texto ""
    where
        codificacionInvertida = map (\(a, b) -> (b, a)) codificacion
        decodificarAux [] _ = []
        decodificarAux s codigoActual =
            let codigosPosibles = filter (\codigo -> codigo `isPrefixOf` s) (map fst codificacionInvertida)
            in if null codigosPosibles
               then decodificarAux (tail s) (codigoActual ++ [head s])
               else let codigoMax = maximumBy (comparing length) codigosPosibles
                        Just caracter = lookup codigoMax codificacionInvertida
                    in caracter : decodificarAux (drop (length codigoMax) s) ""



main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-c":_) -> codificando
        ("-d":_) -> decodificando

codificando :: IO ()
codificando = do
    contenido <- readFile "./sample.txt"

    let hojasOrdenadas = construirListaHuffman contenido
    let arbol = construirArbolHuffman hojasOrdenadas
    let codificacion = codificacionHuffman arbol
    let codigo = codificar contenido codificacion
    let contenidoW = show(arbol) ++ "\n" ++ codigo

    writeFile "./sample.hz" contenidoW

decodificando :: IO ()
decodificando = do
    contenido <- readFile "./sample.hz"
    let contenidoAux = lines contenido
    let strArbol = contenidoAux!!0
    let arbol = read strArbol :: ArbolHuffman
    let codificacion = codificacionHuffman arbol
    let codigo = contenidoAux!!1
    let mensaje = decodificar codigo codificacion

    writeFile "./sample.txt" mensaje
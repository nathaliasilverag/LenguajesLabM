module ArbolDeHuffman where

import Data.List as List (sortBy, nub, foldl')
import qualified Data.Binary as Binary
import qualified Data.Map as Map

type Code            = String
type CharFrequency   = (Char, Int)
type CharFrequencies = [CharFrequency]

-- Tipo de data a utilizar para construir el arbol de codificacion de Huffman 
data ArbolDeHuffman = EmptyTree | Node { root  :: CharFrequency,
                                      left  :: ArbolDeHuffman,
                                      right :: ArbolDeHuffman
                                    } deriving (Show, Eq)

-- Instancia para poder clasificar los arboles de la codificacion
-- segun las frecuencias de caracteres 

instance Ord ArbolDeHuffman where
  compare :: ArbolDeHuffman -> ArbolDeHuffman -> Ordering
  compare (Node (_,x) _ _) (Node (_,y) _ _) = compare x y


-- Instancia que binaria auxiliar para poder codificar y cambiar 
-- los char recibidos por codigo binario
instance Binary.Binary ArbolDeHuffman where
  put :: ArbolDeHuffman -> Binary.Put
  put EmptyTree = do Binary.put (0 :: Binary.Word8)
  put (Node root left right) = do Binary.put (1 :: Binary.Word8)
                                  Binary.put root
                                  Binary.put left
                                  Binary.put right
  get :: Binary.Get ArbolDeHuffman
  get = do t <- Binary.get :: Binary.Get Binary.Word8
           case t of
             0 -> do return EmptyTree
             1 -> do root <- Binary.get
                     left <- Binary.get
                     right <- Binary.get
                     return (Node root left right)

-- Funcion para crear un nuevo arbol de huffman singleton
creaHoja ::  CharFrequency -> ArbolDeHuffman
creaHoja x = (Node x EmptyTree EmptyTree)

hoja :: ArbolDeHuffman -> Bool
hoja (Node _ EmptyTree EmptyTree) = True
hoja                           _  = False

-- obtenemos el caracter asoaciado a la raiz del arbol
caracter :: ArbolDeHuffman -> Char
caracter = fst . root

-- Frencuencia asociada a ala raiz del arbol 
frequencia :: ArbolDeHuffman -> Int
frequencia = snd . root

-- Obtiene la frencuencia de los caracteres del archivo 
-- Diccionario implementado para codificar
-- Ejemplo: contentsFrequencia "abracadabra" -> [('a',5),('b',2),('r',2),('c',1),('d',1)]
frencuenciaContenida :: String -> [CharFrequency]
frencuenciaContenida contents = nub $ map (\x -> (x, (count x contents))) contents
  where count x               = foldl' (\acc y -> if y == x then acc + 1 else 0) 0 

-- Cada par (Char, Frequency) se convierte en una hoja del arbol de huffman
creaHojas :: CharFrequencies -> [ArbolDeHuffman]
creaHojas = map creaHoja

-- Combina dos arboles, de manera que se sumen las frecuencias.
unirArboles :: ArbolDeHuffman -> ArbolDeHuffman -> ArbolDeHuffman
unirArboles leftTree rightTree = Node (' ', frequenciesSum) leftTree rightTree
  where frequenciesSum         = frequencia leftTree + frequencia rightTree

-- Funcion creada para construir el arbol de codificacion de huffman 
-- segun los caracteres contenido en el archivo y sus respectivas frecuencias
construirHuffman :: CharFrequencies -> ArbolDeHuffman
construirHuffman        = construir . creaHojas
  where construir []          = EmptyTree
        construir [arbol]     = arbol
        construir arbol       = construir actualizado
          where ordenado      = sortBy compare arbol
                smallestTrees = take 2 ordenado 
                rightTree     = head smallestTrees
                leftTree      = head $ tail smallestTrees
                unido         = unirArboles rightTree leftTree
                eliminado     = drop 2 ordenado
                actualizado   = unido : eliminado-- Genera codigos de codificacion binaria unicos, por cada caracter contenido
-- en el archivo.
codigosBinario :: ArbolDeHuffman -> [(Char, Code)]
codigosBinario = codigosGenerado ""
  where codigosGenerado path arbol 
          | hoja arbol   = [(caracter arbol, path)]
          | otherwise    = codigosGenerado (path ++ "0") (left arbol)
                          ++ codigosGenerado (path ++ "1") (right arbol)

-- Transforma el contenido del archivo en una secuencia codificada 
-- de 1`s y 0`s(Algorito de Huffman)
-- Ejemplo : codifica "abracadabra" -> ("01111001100011010111100", <arbol>),
-- |          y <arbol> es un Arbol de Huffman valido.
codifica :: String   -> (Code, ArbolDeHuffman)
codifica content     = (codificado content, arbolHuffman)
  where arbolHuffman = construirHuffman $ frencuenciaContenida content
        codificado   = concat . codigoBinario
          where getValue (Just x) = x
                codes             = Map.fromList $ codigosBinario $ arbolHuffman
                codigoBinario     = Prelude.map (\x -> getValue (Map.lookup x codes))

-- Decodifica el arbol de Huffman para obtener nuevamente el contenido inicial 
decodifica :: (Code, ArbolDeHuffman) -> String
decodifica (codigoBinario, arbolHuffman)                    = decodificarArbol codigoBinario arbolHuffman
  where decodificarArbol "" EmptyTree                      = ""
        decodificarArbol "" arbol                          = [caracter arbol]
        decodificarArbol code (Node x EmptyTree EmptyTree) = fst x : decodificarArbol code arbolHuffman
        decodificarArbol (x:xs) arbol                      = if x == '0'
                                                       then decodificarArbol xs (left arbol)
                                                       else decodificarArbol xs (right arbol)
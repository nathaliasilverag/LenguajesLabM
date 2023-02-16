-- Nathalia Silvera 12-10921
-- Ejercicio 4 Tarea 1
module Main where

import ArbolDeHuffman ( ArbolDeHuffman, Code, codifica, decodifica )
import qualified Data.Binary as Binary


main :: IO ()
main = do
  putStrLn "Ingrese la direccion del archivo a comprimir: "
  file <- getLine 
  putStrLn file
  contenido <- readFile file
  let contenidoCodificado = codifica contenido
  serialize contenidoCodificado file
  (code,arbol) <- deserialize file
  putStrLn "-----------------------------------------"
  putStrLn "Contenido original : "
  putStrLn contenido
  putStrLn "\n"
  putStr "Contenido codificado: Buscar en el directorio actual el archivo Codificacion.hz"
  --auxiliar para ver en consola 
  -- putStrLn code
  writeFile "Codificacion.hz" (code)
  putStrLn "\n"
  putStr "Decodificado: revisar directorio actual el archivo Decodificado.hz"
 --auxiliar para ver en consola 
 -- putStrLn $ (decodifica (code,arbol))
 -- Falto modificar file con un split para separar la cadena
  --writeFile "file.hz" $ (decodifica (code,arbol))
  writeFile "decodificado.hz" $ (decodifica (code,arbol))
  putStrLn "\n"



serialize :: (Code, ArbolDeHuffman) -> FilePath -> IO ()
serialize (code,arbol) file = Binary.encodeFile (file ++ ".hz") (code,arbol)

deserialize :: FilePath -> IO (Code, ArbolDeHuffman)
deserialize file = Binary.decodeFile (file ++ ".hz")
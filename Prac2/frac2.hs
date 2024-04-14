import System.IO
-- Definimos el menú de opciones
main :: IO ()
main = menu
menu = do
  putStrLn "1.- Pasar fraccion normal a egipcia desde teclado. "
  putStrLn "2.- Conversión a egipcia y almacenar en archivo"
  putStrLn "3.- Leer una fraccion egipcia desde archivo"
  putStrLn "4.- Salir"
  opcion <- getLine
  case opcion of
    "1" -> do
      putStrLn "Ha ingresado al punto 1..."
      putStrLn "Ingresa el numerador:"
      numerador <- readLn :: IO Int
      putStrLn "Ingresa el denominador:"
      denominador  <- readLn :: IO Int
      let fraccion = Fraccion numerador denominador
          resultado =  fegipcia fraccion
      putStrLn $ "La fraccion dada es: " ++ show fraccion  
      putStrLn $ "La suma Egipcia quedaria: " ++ show resultado
      menu
    "2" -> do
      putStrLn "Ha ingresado al punto 2..."
      putStrLn "Ingresa el numerador:"
      numerador <- readLn :: IO Int
      putStrLn "Ingresa el denominador:"
      denominador  <- readLn :: IO Int
      let fraccion = Fraccion numerador denominador
          resultado =  fegipcia fraccion
      putStrLn $ "La fraccion dada es: " ++ show fraccion  
      putStrLn $ "La suma Egipcia quedaria: " ++ show resultado
      writeFile "resultados_fegip.txt" (unwords (map show resultado)) -- Se escribe el resultado en el archivo
      menu
    "3" -> do
      putStrLn "Ha ingresado al punto 3..."
      putStrLn "Digite el nombre del archivo: "
      nombreArch <- getLine
      contenido <- readFile nombreArch
      putStrLn "La fracción dada es: "
      putStrLn contenido
      let fraccion = read contenido :: Fraccion
          resultado = fegipcia fraccion
      putStrLn $ "La suma Egipcia quedaría: " ++ show resultado
      menu
    "4" -> return ()
    _   -> do
      putStrLn "Opción inválida. Intente de nuevo."
      menu

instance Read Fraccion where
    readsPrec _ cadena = 
        do
        (numerador, resto) <- lex cadena
        ("/", resto1) <- lex resto
        (denominador, restofinal) <- lex resto1
        return (Fraccion (read numerador) (read denominador), restofinal)

-- Definición del tipo de dato para fraccion, constituye de 2 enteros
data Fraccion = Fraccion Int Int 
-- Definicion de tipo de dato para las operaciones
data Operaciones = Suma Fraccion Fraccion
                  | Resta Fraccion Fraccion
                  | Multiplicacion Fraccion Fraccion
                  | Division Fraccion Fraccion
                  deriving (Show)
--Le damos formato a nuestra fracción
instance Show Fraccion where 
    show (Fraccion a b) = (show a) ++ "/" ++
                          (show b)

operacion :: Operaciones -> Fraccion
operacion operaciones | Suma (Fraccion a b) (Fraccion c d) <- operaciones = Fraccion (a *d + c*b) (b * d)
                      | Resta (Fraccion a b) (Fraccion c d) <- operaciones = Fraccion (a *d - c*b) (b * d)
                      | Multiplicacion (Fraccion a b) (Fraccion c d) <- operaciones = Fraccion (a * c) (b * d)
                      | Division (Fraccion a b) (Fraccion c d) <- operaciones = Fraccion (a * d) (b * c) 

fegipcia :: Fraccion -> [Fraccion]
fegipcia (Fraccion 1 denominador) = [Fraccion 1 denominador] -- Primer caso base
fegipcia (Fraccion numerador 1) = [Fraccion 1 numerador] -- Segundo caso base
fegipcia (Fraccion numerador denominador)
    | numerador == 0 = [] -- Si el numerador es 0, devuelve una lista vacía
    | numerador < denominador && denominador `mod` numerador /= 0 = -- Si el numerador es menor que el denominador y el denominador no es divisible por el numerador
        let nuevoDenominador = (denominador `div` numerador) + 1 -- Calcula un nuevo denominador sumando 1 al cociente de
        in Fraccion 1 nuevoDenominador : fegipcia (operacion (Resta (Fraccion numerador denominador) (Fraccion 1 nuevoDenominador))) -- Crea una nueva fracción con numerador 1 y el nuevo denominador, y llama recursivamente a fegipcia con la fracción resultante de restar la fracción original con la nueva fracción
    | numerador < denominador = -- Si el numerador es menor que el denominador
        let nuevoDenominador = denominador `div` numerador -- Calcula un nuevo denominador como cociente de la división entera del denominador entre el numerador
        in Fraccion 1 nuevoDenominador : fegipcia (operacion (Resta (Fraccion numerador denominador) (Fraccion 1 nuevoDenominador))) -- Crea una nueva fracción con numerador 1 y el nuevo denominador, y llama recursivamente a fegipcia con la fracción resultante de restar la fracción original con la nueva fracción
    | numerador `mod` denominador /= 0 = -- Si el numerador no es divisible por el denominador
        let nuevoDenominador = (numerador `div` denominador) + 1 -- Calcula un nuevo denominador sumando 1 al cociente de la división entera del numerador entre el denominador
        in Fraccion 1 nuevoDenominador : fegipcia (operacion (Resta (Fraccion numerador denominador) (Fraccion 1 nuevoDenominador))) -- Crea una nueva fracción con numerador 1 y el nuevo denominador, y llama recursivamente a fegipcia con la fracción resultante de restar la fracción original con la nueva fracción
    | otherwise = -- En todos los demás casos
        let nuevoDenominador = numerador `div` denominador -- Calcula un nuevo denominador como cociente de la división entera del numerador entre el denominador
        in Fraccion 1 nuevoDenominador : fegipcia (operacion (Resta (Fraccion numerador denominador) (Fraccion 1 nuevoDenominador))) -- Crea una nueva fracción con numerador 1 y el nuevo denominador, y llama recursivamente a fegipcia con la fracción resultante de restar la fracción original con la nueva fracción

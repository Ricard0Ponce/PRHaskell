{-
* Notas sobre esta prácica
* Funcion Lex
lex "+ 1esta es una prueba"
[("+"," 1esta es una prueba")]
** readsPrec -- Permite 
-}

data Imaginario = Im Float Float

instance Show(Imaginario) where
    show(Im a b) = show(a) ++ " + " ++
                   show(b) ++ "i"

instance Eq (Imaginario) where
    (Im a b) == (Im c d) = (a==c) && (b==d)

instance Read (Imaginario) where
    readsPrec p cadena = 
        do
        (preal, resto) <- lex cadena
        ("+",resto1) <- lex resto
        (pimag, resto2) <- lex resto1
        ("i",restofinal) <- lex resto2
        return ((Im (read preal) (read pimag)),restofinal)
-- Se usa de la forma: 
 -- read "3 + 4i"::Imaginario 
-- Forma 2: 
-- read "[3 + 4i, 2 + 1i]"::[Imaginario]
-- read "(2 + 1i, 7.3 + 4.8i)"::(Imaginario, Imaginario)

---- Para leer usamos readFile 
-- Read File lee y luego 
-- readFile "li.txt" >>= \x -> (putStrLn((show(read x::[Imaginario])))
-- LO anterior permite leer desde un archivo los elementos

-- Ejemplo de la funcion: 

--SE USA COMO: suma2p, debes tener un archivo li.txt para poder usarlo y los elementos en ese archivo deben ser con el siguiente formato
-- [3 + 4i , 2 + 1i]

suma2p =
    do
        s <- readFile "li.txt"
        putStrLn (show (suma (fst (lee2 s)) (snd (lee2 s)) ) ) 
        where 
            lee2 cad = ( (read cad::[Imaginario])!!0  , (read cad::[Imaginario])!!1) 


suma (Im a b) (Im c d)=
    Im (a+c) (b+d)

--Para el menú: 
{-
Acá tambien existe el Main 
main = 
    do
        o <- menu
        if(o /= 'q') then
            putStrLn ("Elegiste " ++ [o])
            main
        else 
            return ()

menu = do
    putStrLn "1: opcion 1"
    putStrLn "2: opcion 2"
    putStrLn "q: Termina"
    a <- getChar
    return (a)    
-}
-- Tipo de dato fraccion 
data Fraccion = Fraccion Int Int


--main = 
  --  do
    --    o <- menu
      --  if(o /= 'q') then
        --    putStrLn ("Elegiste " ++ [o])
          --  main
        --else 
          --  return ()

menu = do
    putStrLn "1: Leer fracción"
    putStrLn "2: Conversión a Egipcia"
    putStrLn "3: Leer archivo"
    putStrLn "4: Conversion a normal"
    putStrLn "5: Operaciones"
    putStrLn "q: Saliendo..."
    a <- getChar
    return (a)

data MiTupla = MT Float Float deriving(Eq)

instance Show MiTupla where 
    show (MT a b) = (show a) ++ "| "++ (show b)

instance Read MiTupla where
    readsPrec n cad = 
        do 
            (e1, lqs) <- lex cad -- e1 = elemento 1, lqs = lo que sigue 
            -- En consola: lex "3&4"
            -- lex "&4"
            ("&",lqsd) <- lex lqs
            (e2, lqsdlqs) <- lex lqsd
            return (MT(read e1) (read e2), lqsdlqs)
            -- lex " 4"
--Para la tarea cambiar / 

-- read "3 & 4"::MiTupla 
-- read "[2&1,0&9,5&5]"::[MiTupla]
--Arriba se debe indicar el tipo de dato
-- En consola: MT  3 4
-- (MT 2 2) = (MT 2.0 2.0) 

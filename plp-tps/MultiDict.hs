module MultiDict where

import Data.Maybe
import Data.Char

data MultiDict a b = Nil | Entry a b (MultiDict a b) | Multi a (MultiDict a b) (MultiDict a b) deriving Eq

padlength = 5

isNil Nil = True
isNil _ = False

padMD :: (Show a, Show b) => Int -> MultiDict a b -> String
padMD nivel t = initialPad ++ case t of
                    Nil -> ""
                    Entry k v m -> "\n" ++ initialPad ++ " " ++ show k ++": "++ show v ++ comma m ++ padMD nivel m
                    Multi k m1 m2 -> "\n" ++ initialPad ++ " " ++ show k ++": {"++ rec m1 ++ pad (padlength*nivel) ++"}" ++ comma m2 ++ padMD nivel m2
    where levelPad = (padlength*nivel)
          initialPad = pad levelPad
          rec = padMD (nivel+1)
          comma m = if isNil m then "\n" else ","

pad :: Int -> String
pad i = replicate i ' '

instance (Show a, Show b) => Show (MultiDict a b) where
  show x = "{" ++ padMD 0 x ++ "}"










----------------------Ejercicio 1----------------------
foldMD :: c -> (a->b->c->c) -> (a->c->c->c) -> MultiDict a b -> c
foldMD fn _ _ Nil = fn
foldMD fn fe fm (Entry k v dicc) = fe k v (foldMD fn fe fm dicc) 
foldMD fn fe fm (Multi k dicc1 dicc2) = fm k (foldMD fn fe fm dicc1) (foldMD fn fe fm dicc2)

--Pruebas para el foldMD
sumarTodosLosValores :: MultiDict String Int -> Int
sumarTodosLosValores d = foldMD 0 (\k v rec -> v + rec) (\k rec1 rec2 -> rec1 + rec2) d

concatenarTodasLasClaves :: MultiDict String a -> String
concatenarTodasLasClaves d = foldMD "" (\k v rec -> k ++ rec) (\k rec1 rec2 -> k ++ rec1 ++ rec2) d



recMD :: b  -> (a -> c -> MultiDict a c -> b -> b) -> (a -> MultiDict a c -> MultiDict a c -> b -> b -> b) -> MultiDict a c -> b
recMD fn fe fm Nil              = fn
recMD fn fe fm (Entry k v dicc) = fe k v dicc (recMD fn fe fm dicc)
recMD fn fe fm (Multi k dicc1 dicc2) = fm k dicc1 dicc2 (recMD fn fe fm dicc1) (recMD fn fe fm dicc2)
--TODO TESTEAR


----------------------Ejercicio 2----------------------
profundidad :: MultiDict a b -> Integer
profundidad d = foldMD 0 (\k v rec -> max 1 rec) (\k rec1 rec2 -> 1 + max rec1 rec2) d 



tamanio :: MultiDict a b -> Integer
tamanio d = foldMD 0 (\k v rec -> 1 + rec) (\k rec1 rec2 -> rec1 + rec2 + 1) d



----------------------Ejercicio 3----------------------
podarHasta = foldMD
          (\_ _ _ -> Nil)
          (\k v r l p lorig->cortarOSeguir l p $ Entry k v $ r (l-1) p lorig)
          (\k r1 r2 l p lorig ->cortarOSeguir l p $ Multi k (r1 lorig (p-1) lorig) (r2 (l-1) p lorig))
  where cortarOSeguir l p x = if l <= 0 || p <= 0 then Nil else x

-- Poda a lo ancho y en profundidad.
-- El primer argumento es la cantidad máxima de claves que deben quedar en cada nivel.
-- El segundo es la cantidad de niveles.
podar :: Integer -> Integer -> MultiDict a b -> MultiDict a b
podar long prof m = podarHasta m long prof long







--Dado un entero n, define las claves de n en adelante, cada una con su tabla de multiplicar.
--Es decir, el valor asociado a la clave i es un diccionario con las claves de 1 en adelante, donde el valor de la clave j es i*j.
tablas :: Integer -> MultiDict Integer Integer
tablas = undefined

{-


serialize :: (Show a, Show b) => MultiDict a b -> String
serialize = undefined

mapMD :: (a->c) -> (b->d) -> MultiDict a b -> MultiDict c d
mapMD = undefined

--Filtra recursivamente mirando las claves de los subdiccionarios.
filterMD :: (a->Bool) -> MultiDict a b -> MultiDict a b
filterMD = undefined

enLexicon :: [String] -> MultiDict String b -> MultiDict String b
enLexicon = undefined

cadena :: Eq a => b ->  [a] -> MultiDict a b
cadena = undefined

--Agrega a un multidiccionario una cadena de claves [c1, ..., cn], una por cada nivel,
--donde el valor asociado a cada clave es un multidiccionario con la clave siguiente, y así sucesivamente hasta
--llegar a la última clave de la lista, cuyo valor es el dato de tipo b pasado como parámetro.
definir :: Eq a => [a] -> b -> MultiDict a b -> MultiDict a b
definir (x:xs) v d = (recMD (\ks -> cadena v ks)
       (\k1 v1 m r (k:ks)-> if k1 == k then armarDic ks k m (cadena v ks) else Entry k1 v1 (r (k:ks)))
       (\k1 m1 m2 r1 r2 (k:ks) -> if k1 == k then armarDic ks k m2 (r1 ks) else Multi k1 m1 (r2 (k:ks)))) d (x:xs)
  where armarDic ks k resto interior = if null ks then Entry k v resto else Multi k interior resto

obtener :: Eq a => [a] -> MultiDict a b -> Maybe b
obtener = undefined
-}
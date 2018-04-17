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



tamaño :: MultiDict a b -> Integer
tamaño d = foldMD 0 (\k v rec -> 1 + rec) (\k rec1 rec2 -> rec1 + rec2 + 1) d



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

-- crea la primera entrada con la tabla correspondiente para el diccionario de las entradas sucesivas 

tablas :: Integer -> MultiDict Integer Integer
tablas n = Multi n (agregarTabla n 1) (tablas (n + 1))

-- crea las entries para un n particular

agregarTabla:: Integer -> Integer -> MultiDict Integer Integer
agregarTabla n cur_n = Entry cur_n (cur_n*n) (agregarTabla n (cur_n+1))
                  
----------------------Ejercicio 4----------------------

serialize :: Show a => Show b => MultiDict a b -> String
serialize = foldMD "[ ]" showEntry showMulti

showEntry :: Show a => Show b => a -> b -> String -> String
showEntry = (\x y z -> "[" ++ show x ++ ": " ++ show y ++ ", " ++ z ++ "]")

showMulti :: Show a => a -> String -> String -> String
showMulti = (\x y z -> "[" ++ show x ++ ": " ++ y ++ ", " ++ z ++ "]" )


----------------------Ejercicio 5----------------------
mapMD :: (a->c) -> (b->d) -> MultiDict a b -> MultiDict c d
mapMD f g d = foldMD Nil (\k v d_rec -> Entry(f k) (g v) d_rec)
                         (\k d_rec_1 d_rec_2 -> Multi (f k) d_rec_1 d_rec_2) d

filterMD :: (a->Bool) -> MultiDict a b -> MultiDict a b
filterMD p d = foldMD Nil (\k v dr -> filterEntry k v dr ) (\k dr_1 dr_2 -> filterMulti k dr_1 dr_2) d
                where {
                      filterEntry k v dr        = if p k then Entry k v dr
                                                  else dr;
                      filterMulti k dr_1 dr_2 = if p k then Multi k dr_1 dr_2
                                              else dr_2;
                }

toLowerCase :: String -> String
toLowerCase s = map(\x->toLower x) s

presentInList :: Eq a => a -> [a] -> Bool
presentInList e ls = e `elem` ls

enLexicon :: [String] -> MultiDict String b -> MultiDict String b
enLexicon arr d = filterMD (\k -> presentInList k arr ) $ mapMD toLowerCase (\v->v) d


----------------------Ejercicio 6----------------------

cadena :: b -> [a] -> MultiDict a b
cadena v = recr (error "Lista Vacia") (\x ls y -> if null ls then (Entry x v Nil) else Multi x y Nil)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs)


----------------------Ejercicio 7----------------------

--Agrega a un multidiccionario una cadena de claves [c1, ..., cn], una por cada nivel,
--donde el valor asociado a cada clave es un multidiccionario con la clave siguiente, y así sucesivamente hasta
--llegar a la última clave de la lista, cuyo valor es el dato de tipo b pasado como parámetro.
definir :: Eq a => [a] -> b -> MultiDict a b -> MultiDict a b
definir (x:xs) v d = (recMD (\ks -> cadena v ks)
       (\k1 v1 m r (k:ks)-> if k1 == k then armarDic ks k m (cadena v ks) else Entry k1 v1 (r (k:ks)))
       (\k1 m1 m2 r1 r2 (k:ks) -> if k1 == k then armarDic ks k m2 (r1 ks) else Multi k1 m1 (r2 (k:ks)))) d (x:xs)
  where armarDic ks k resto interior = if null ks then Entry k v resto else Multi k interior resto


obtener :: Eq a => [a] -> MultiDict a b -> Maybe b
obtener [] dicc = Nothing
obtener [x] dicc = obtenerDef x dicc
obtener (x:xs) dicc = obtener xs (obtenerDicc x dicc)

-- obtenerAux (x:xs) dicc = foldMD (Nothing) (\k v rec -> ) (\k rec1 rec2 -> ) dicc

obtenerDicc :: Eq a => a -> MultiDict a b -> MultiDict a b
obtenerDicc c dicc = recMD Nil (\k v d rec -> rec) (\k d1 d2 rec1 rec2 -> if c == k then  d1 else rec2) dicc

obtenerDef :: Eq a => a -> MultiDict a b -> Maybe b
obtenerDef c dicc =  foldMD Nothing (\k v rec -> if c == k then  Just v else rec) (\k rec1 rec2 -> rec2) dicc



import Data.Binary.Builder (append)

listaPares :: Integral a => a -> [a]
listaPares num = [x | x <- [1..num], mod x 2 ==0]

listaImpares :: Integral a => a -> [a]
listaImpares num = [x | x <- [1..num], mod x 2 ==1]

impares :: Integral a => a -> [a]
impares num = reverse ([x | x <- [1..num], mod x 2 ==1]) 

pares :: Integral a => a -> [a]
pares num = reverse ([x | x <- [1..num], even x]) 

multiplos :: Integral a => a -> a -> [a]
multiplos num limite = [x | x <- [1..limite],even x, mod x num ==0]

multiplosletra :: Integral a => a -> a -> [[Char]]
multiplosletra num limite = [if even x then "par" else "impar" | x <- [1..limite], mod x num ==0]

reemplazo limite  = [if x< 10 then "menor" else "mayor" | x <- [1..limite], odd x]
--1. Ejercicio: firstToEnd
firstToEnd' :: [a]-> [a]
firstToEnd' []= []
firstToEnd' (x:xs)= xs ++ [x]

--2. Ejercicio : minAndMax
minAndMax' :: (Ord a) => [a] -> [a]
minAndMax' xs = minimum xs : [maximum xs]

--3. Ejercicio: minorsFirstElement
minorsFirstElement':: (Integral a) => [a] -> [a]
minorsFirstElement' x = filter (head x >) (tail x)

--4. Ejercicio: greaterOrEqualFirstElement
greaterOrEqualFirstElement' :: Integral a => [a] -> [a]
greaterOrEqualFirstElement' [] = error "La lista esta vacia"
greaterOrEqualFirstElement' [a] = error "Solo hay un elemento en la lista"
greaterOrEqualFirstElement' list = [x | x <- tail list , x >= head list]

--5. Ejercicio: minorsToSumFirstAndSecondElem
minorsToSumFirstAndSecondElement' :: Integral a => [a] -> [a]
minorsToSumFirstAndSecondElement' [] = error "La lista esta vacia"
minorsToSumFirstAndSecondElement' [a] = error "Solo hay un elemento en la lista"
minorsToSumFirstAndSecondElement' list= [x | x <- tail (tail list) , x <head list + head(tail list)]

--6. Ejercicio: listSumDuplaToList
listSumDuplaToList' :: Integral a => [(a,a)] -> [a]
listSumDuplaToList' [] = []
listSumDuplaToList' list = fst(head list) + snd (head list) : listSumDuplaToList' (tail list)

--7. Ejercicio: listMultTripletaToList
listMultTripletaToList':: (Integral a)=> [(a,a,a)]-> [a]
listMultTripletaToList' [] = []
listMultTripletaToList' xs = [x*y*z| (x,y,z)<- xs]

--8. Ejercicio: changeFstToSnd
changeFstToSnd' :: (Integral a)=>[(a,a)]-> [(a,a)] 
changeFstToSnd' xs = [(y,x) | (x,y)<- xs]


--10. Ejercicio: dividers
dividers':: Int -> [Int] 
dividers' n = [x | x <- [1..(n)], n `rem` x == 0]

--11. Ejercicio: primeNumbers
factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]
esPrimo :: Int -> Bool
esPrimo n = factores n == [1,n]
primeNumbers' :: Int -> [Int]
primeNumbers' n = [x | x <- [2..n], esPrimo x]

--12. Ejercicio:infinitePrimeNumbers
infinitePrimeNumbers' :: [Int]
infinitePrimeNumbers' = [x | x <- [2..], esPrimo x]


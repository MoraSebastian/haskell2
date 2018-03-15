{-
	Autores: Dvid Romero, Sebastián Mora, Andrés Rentería
	Fecha: 8/3/18
-}

module Listas where
	
--Suma todos los elementos de una lista	
suma::[Int]->Int
suma [] = 0
suma (x:xs) = x + suma xs

--Invierte los elementos de una lista
invertir:: [Int]->[Int]
invertir [] = []
invertir (x:xs) = invertir xs ++ [x]

--Suma todos los elementos pares de una lista
sumaPares::[Int]->Int
sumaPares[] = 0
sumaPares (x:xs) = if even x then x + sumaPares xs
					else sumaPares xs

--Decuelve la cantidad de elementos impares que hay en una lista 					
sumaCImpares::[Int]->Int
sumaCImpares [] = 0
sumaCImpares (x:xs) = if odd x then 1+sumaCImpares(xs)
						else sumaCImpares(xs)

--determina si un elemento pertenece a una lista						
elementoLista::Int->[Int]->Bool
elementoLista n [] = False
elementoLista n (x:xs) = if n == x then True
						else elementoLista n xs
						
--verifica si una lista está incluida en una lista						
contiene:: [Int] ->[Int]-> Bool
contiene (x:xs) [] = True
contiene [] [] = True
contiene [] _ = False
contiene (x:xs) (b:bs) = if elementoLista b (x:xs) then contiene (x:xs) bs	
						else False
--Determina el número mayor en una lista
mayor:: [Int]-> Int
mayor[]=0
mayor (x:y:ys) 
		| x >= y = mayor (x:ys)
		| otherwise = mayor (y:ys)

--Numeros pares usando filter		
paresF:: [Int]->Int
paresF (x:xs) = suma (filter(even)(x:xs))

--ejercicio impares usando filter
imparesF:: [Int]->Int
imparesF (x:xs) =length(filter(odd)(x:xs))

--suma digitos
sumaDigitos:: Int->Int
sumaDigitos x = if x<10 then x
				else  x `mod` 10 + sumaDigitos (div x 10)
    
-- Fibonacci
fibonacci:: Int->Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)



				
--longitud
longitud:: Int->Int
longitud n = if n<10 then 1
			 else 1 + longitud (div n 10)

			
--potencia
potencia:: Int->Int->Int
potencia b 0 = 1
potencia b 1 = b
potencia b e = b*potencia b (e-1)


--palindromo
palindromo::Int->Bool
palindromo n = if n == invertirNum (n) then True
			else False

-- Invertir
invertirNum:: Int -> Int
invertirNum n = if n <10 then n 
				else (n `mod` 10) *(potencia 10 ((longitud n) -1)) + invertirNum ( div n 10)

				
--divisionR
divisionR:: Int->Int->Int
divisionR x 0 = 404
divisionR 0 x = 0
divisionR x y = if x == y then 1
				else 1+ divisionR (x-y) y 
				
				
--Encontrar Mayor
numMayor:: Int -> Int
numMayor x = if x<10 then x
			else if x `mod` 10> numMayor (div x 10) then x 
				else numMayor (div x 10) `mod` 10
				
--divisionR x 0 = 404
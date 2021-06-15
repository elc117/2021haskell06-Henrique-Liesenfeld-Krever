--ends :: [Int] -> [Int]
--ends lista = if length lista > 1
   -- (head lista) : tail lista
--1.pega o primeiro e o ultimo
ends :: [Int] -> [Int]
ends [] = []
ends lst = if (length lst) < 3
  then head lst : tail lst
  else ends [head lst,last lst]

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame xs

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs
  else deduzame2 xs

--4. lista com numeros
geraTabela :: Int -> [(Int,Int)]
geraTabela valor = if valor > 0 
  then (valor, valor^2) : geraTabela (valor-1) 
  else []

--5. ve se uma letra esta contida em uma string
contido :: Char -> String -> Bool
contido letra "" = False
contido letra (x:xs) = if x == letra
  then True 
  else contido letra xs

--6. aumenta duas coordenadas em 2
translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate ((n1,n2):xs) = (n1+2,n2+2) : translate xs

--7.palavras maiores que 5
countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = if length x > 5 
  then 1 + countLongs xs 
  else countLongs xs

--8. palavras maiores que 5 em outra lista
onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if length x > 5 
  then x : onlyLongs xs 
  else onlyLongs xs

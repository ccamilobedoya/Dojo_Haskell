x::Int
x=5

lucky :: (Integral a) => a -> String
lucky 7 = "Numero de la suerte"
lucky x = "Hoy te va a ir mal" 

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial(x - 1)

sumatoria :: Integer -> Integer
sumatoria 0 = 0
sumatoria x = x + sumatoria(x - 1)

sumaCadaDos :: [Integer] -> [Integer]
sumaCadaDos[] = []
sumaCadaDos(x:[]) = [x]
sumaCadaDos(x:y:zs) = (x+y):sumaCadaDos zs

tamanoLista :: [Int] -> Int
tamanoLista[] = 0
tamanoLista(x:[]) = 1
tamanoLista(x:y:zs) = 2 + tamanoLista zs

peso :: Int -> [Char]
peso peso
	|peso <= 40 = "Estas muy flaco"
	|peso <= 60 = "Estas muy bien de peso"
	|peso <= 80 = "Estas como gordo"
	|otherwise = "Estas super gordo"

mayor :: Int -> Int -> Int
mayor a b
	|a > b = a
	|a < b = b
	|otherwise = 0

duplicarPares xs = [x*2 | x<-xs, (mod x 2) == 0] 

multiplosTres xs = [x | x<-xs, (mod x 3) == 0] 
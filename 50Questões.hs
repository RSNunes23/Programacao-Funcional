--1.
enumFromTo2 :: Int -> Int -> [Int]
enumFromTo2 a b | a > b = []
                | otherwise = a : enumFromTo2 (a + 1) b

--2.
enumFromThenTo2 :: Int -> Int -> Int -> [Int]
enumFromThenTo2 a b c | a > c = []
                      | a < c && b <= a = []
                      | otherwise = a : enumFromThenTo2 b (2 * b - a) c

--3.
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (h:t) (h':t') = h : (+++) t (h':t')

--4.
(!!!) :: [a] -> Int -> a
(!!!) (h:_) 0 = h
(!!!) (h:t) a = (!!!) t (a - 1)

--5.
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse :: [a] -> [a]
reverse (h:t) = t ++ reverse2 [h]

--6.
take2 :: Int -> [a] -> [a]
take2 _ [] = []
take2 a (h:t) | a <= 0 = []
              | otherwise = h : take2 (a - 1) t

--7.
drop2 :: Int -> [a] -> [a]
drop2 _ [] = []
drop2 a (h:t) | a <= 0 = (h:t)
              | otherwise = drop2 (a - 1) t

--8.
zip2 :: [a] -> [b] -> [(a,b)]
zip2 _ [] = []
zip2 [] _ = []
zip2 (h:t) (h':t') = (h,h') : zip2 t t'

--9.
replicate2 :: Int -> a -> [a]
replicate2 a b | a <= 0 = []
               | otherwise = b : replicate2 (a - 1) b

--10.
intersperse2 :: a -> [a] -> [a]
intersperse2 _ [] = []
intersperse2 _ [l] = [l]
intersperse2 a (h:t) = h : a : intersperse2 a t

--11.
group2 :: Eq a => [a] -> [[a]]
group2 [] = []
group2 (h:t) = (h : takeWhile (==h) t) : group2 (dropWhile (==h) t)

--12.
concat2 :: [[a]] -> [a]
concat2 [[]] = []
concat2 (h:t) = h ++ concat2 t 

--13.
inits2 :: [a] -> [[a]]
inits2 [] = []
inits2 l = inits2 (init l) ++ [l]

--14.
tails2 :: [a] -> [[a]]
tails2 [] = []
tails2 l = l : tails2 (tail l)

--15.
heads2 :: [[a]] -> [a]
heads2 [[]] = []
heads2 (h:t) = head h : heads2 t

--16.
total2 :: [[a]] -> Int
total2 [] = 0
total2 (h:t) = length h + total2 t

--17.
fun2 :: [(a,b,c)] -> [(a,c)]
fun2 [] = []
fun2 ((a,b,c):t) = (a,c) : fun2 t

--18.
cola2 :: [(String,b,c)] -> String
cola2 [] = ""
cola2 ((a,b,c):t) = a ++ cola2 t

--19.
idade2 :: Int -> Int -> [(String,Int)] -> [String]
idade2 _ _ [] = []
idade2 a b ((x,y):z) | a - y >= b = x : idade2 a b z
                     | otherwise = idade2 a b z

--20.
powerEnumFrom2 :: Int -> Int -> [Int]
powerEnumFrom2 n 1 = [1]
powerEnumFrom2 n m | m < 1 = []
                   | otherwise = powerEnumFrom2 n (m - 1) ++ [n ^ (m-1)]

--21.
isPrime2 :: Int -> Bool
isPrime2 n = n >= 2 && primeCheck n 2

primeCheck :: Int -> Int -> Bool
primeCheck n m | m * m > n = True
               | mod n m == 0 = False
               | otherwise = primeCheck n (m + 1)

--22.
isPrefixOf2 :: Eq a => [a] -> [a] -> Bool
isPrefixOf2 _ [] = False
isPrefixOf2 [] _ = True
isPrefixOf2 (h:t) (h':t') = h == h' && isPrefixOf2 t t'

--23.
isSuffixOf2 :: Eq a => [a] -> [a] -> Bool
isSuffixOf2 _ [] = False
isSuffixOf2 [] _ = True
isSuffixOf2 l l'@(_:t) = l == l' || isSuffixOf2 l t

--24.
isSubsequenceOf2 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf2 _ [] = False
isSubsequenceOf2 [] _ = True 
isSubsequenceOf2 (h:t) (h':t') | h == h' = isSubsequenceOf2 t t' 
                               | otherwise = isSubsequenceOf2 (h:t) t'

--25.
elemIndices2 :: Eq a => a -> [a] -> [Int]
elemIndices2 _ [] = []
elemIndices2 n (h:t) | n == h = 0 : map (+1) (elemIndices2 n t)
                     | otherwise = map (+1) (elemIndices2 n t)

--26.
nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (h:t) = if h `elem` t then nub2 t 
             else h : nub2 t

--27.
delete2 :: Eq a => a -> [a] -> [a]
delete2 _ [] = []
delete2 n (h:t) | n == h = t 
                | otherwise = h : delete2 n t

--28.
(\\\):: Eq a => [a] -> [a] -> [a]
(\\\) [] _ = [] 
(\\\) l [] = l
(\\\) l (h:t) = (\\\) (delete2 h l) t

--29.
union2 :: Eq a => [a] -> [a] -> [a]
union2 l [] = l 
union2 l (h:t) | h `elem` l = union2 l t 
               | otherwise = h : union2 (l++ [h]) t

--30.
intersect2 :: Eq a => [a] -> [a] -> [a]
intersect2 [] _ = []
intersect2 (h:t) l | h `elem` l = h : intersect2 t l
                   | otherwise = intersect2 t l

--31.
insert2 :: Ord a => a -> [a] -> [a]
insert2 n [] = [n]
insert2 n (h:t) | n <= h = n : h : t
                | otherwise = h : insert2 n t

--32.
unwords2 :: [String] -> String
unwords2 [] = ""
unwords2 (h:t) = h ++ (if null t then "" else " ") ++ unwords2 t

--33.
unlines2 :: [String] -> String
unlines2 [] = []
unlines2 (h:t) = h ++ "\n" ++ unlines2 t

--34.
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t) | h >= (t !! x) = 0
             | otherwise = 1 + x
    where x = pMaior t

--35.
lookup2 :: Eq a => a -> [(a,b)] -> Maybe b
lookup2 _ [] = Nothing
lookup2 n ((a,b):t) | n == a = Just b
                    | otherwise = lookup2 n t

--36.
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [l] = [l]
preCrescente (h:s:t) | s >= h = h : preCrescente (s:t)
                     | otherwise = [h]

--37.
iSort2 :: Ord a => [a] -> [a]
iSort2 [] = []
iSort2 (h:t) = insert2 h (iSort2 t)

--38.
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h':t') | h < h' = True
                    | h == h' = menor t t'
                    | otherwise = False

--39.
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet n ((a,_):t) | n == a = True
                     | otherwise = elemMSet n t

--40.
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,1):t) = a : converteMSet t
converteMSet ((a,b):t) = a : converteMSet ((a,b - 1): t)

--41.
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((a,b):t) | n == a = (a,b+1) : t
                       | otherwise = (a,b) : insereMSet n t

--42.
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet n ((a,b):t) | n == a = if b > 1 then (a, b-1) : t else t
                       | otherwise = (a,b) : removeMSet n t

--43.
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

--44.
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a : as,bs)
    where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b : bs)
    where (as,bs) = partitionEithers t

--45.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just h):t) = h : catMaybes t
catMaybes (Nothing:t) = catMaybes t

--46.
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (a, b) (c, d) 
    | a < c = Este : caminho (a + 1, b) (c, d)
    | a > c = Oeste : caminho (a - 1, b) (c, d)
    | b < d = Norte : caminho (a, b + 1) (c, d)
    | b > d = Sul : caminho (a, b - 1) (c, d)
    | otherwise = []

--47.
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (a, b) (Norte:t) = posicao (a, b + 1) t
posicao (a, b) (Sul:t) = posicao (a, b - 1) t
posicao (a, b) (Este:t) = posicao (a + 1, b) t
posicao (a, b) (Oeste:t) = posicao (a - 1, b) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)

--49.
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) 
    | eQuadrado h = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x1,y1) (x2,y2)) = abs (y2 - y1) == abs (x2 - x1)

--49.
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

--50.
data Equipamento = Bom | Razoavel | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t
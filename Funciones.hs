--LOPEZ LOPEZ CITLALLY SOLEDAD
--FUNCIONES BASICAS
--EJERCICIO1
promedio3 x y z = (( x + y + z ) / 3 )

--EJERCICIO2
sumaMonedas a b c d e = (a * 1) + (b * 2) + (c * 5) + (d * 10) + (e * 20)

--EJERCICIO3
volumenEsfera r = (4 / 3) * pi * r^3

--EJERCICIO4
areaCorona r1 r2 = pi * (r2^2 - r1^2)

--EJERCICIO5
ultimaCifra x = x `rem` 10

--EJERCICIO6
maxTres x y z = max x (max y z)

--EJERCICIO7
rota1 xs = tail xs ++ [head xs]

--EJERCICIO8
rota n xs = drop n xs ++ take n xs

--EJERCICIO9
rango xs = [minimum xs, maximum xs]

--EJERCICIO10
palindromo xs = xs == reverse xs

--EJERICIO11
interior xs = init (tail xs)

--EJERCICIO13
segmento m n xs
  | m > n     = []  
  | otherwise = take (n - m + 1) (drop (m - 1) xs)

--EJERCICIO14
extremos n xs = take n xs ++ take n (reverse xs)

--EJERCICIO15
mediano x y z = x + y + z - maximum [x, y, z] - minimum [x, y, z]

--EJERCICIO16
tresIguales x y z = (x == y) && (y == z)

--EJERCICIO17
tresDiferentes x y z = (x /= y) && (x /= z) && (y /= z)

--EJERCICIO18
cuatroIguales x y z u = tresIguales x y z && (z == u)





--Guardas y Patrones
--EJERCICIO1
divisionSegura x y
  | y /= 0    = x / y
  | otherwise = 9999.0

--EJERCICIO2
xor1 x y
  | x == True  && y == False = True
  | x == False && y == True  = True
  | otherwise                = False

--EJERCICIO3
mayorRectangulo (b1, h1) (b2, h2)
  | b1 * h1 >= b2 * h2 = (b1, h1)
  | otherwise          = (b2, h2)

--EJERCICIO4
intercambia (x, y) = (y, x)

--EJERCICIO5
distancia (x1, y1) (x2, y2)
  | x1 == x2 && y1 == y2 = 0.0  
  | otherwise = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

--EJERCICIO6
ciclo []        = []                     
ciclo [x]       = [x]                    
ciclo (x:xs)    = moverUltimo (x:xs)      
  where
    moverUltimo (y:[])   = [y]             
    moverUltimo (y:ys)   = moverUltimo ys ++ [y]  

--EJERCICIO7
numeroMayor x y
  | x >= y    = x * 10 + y  
  | otherwise = y * 10 + x 

--EJERCICIO8
numeroDeRaices a b c
  | d < 0     = 0  
  | d == 0    = 1  
  | otherwise  = 2  
  where d = b^2 - 4 * a * c 

--EJERCICIO9
raices a b c
  | d < 0     = []                      
  | d == 0    = [r]                     
  | otherwise  = [r1, r2]               
  where d  = b^2 - 4 * a * c            
        r  = -b / (2 * a)               
        r1 = (-b + sqrt d) / (2 * a)     
        r2 = (-b - sqrt d) / (2 * a)     

--EJERCICIO10
area a b c 
  | esTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))  -- Cálculo del área si es un triángulo
  | otherwise = 0.0  -- Si no es un triángulo, el área es 0
  where
    s = (a + b + c) / 2  -- Semiperímetro
    esTriangulo x y z = (x + y > z) && (x + z > y) && (y + z > x)

--EJERCICIO11
interseccion [a, b] [c, d]
  | b < c = []  
  | d < a = []  
  | otherwise = [max a c, min b d]  

interseccion _ _ = []  

--EJERCICIO12
linea n
  | n <= 0    = []  -- Devuelve una lista vacía si n es menor o igual a 0
  | otherwise = [((n * (n - 1)) `div` 2 + 1) .. ((n * (n - 1)) `div` 2 + n)]





--Recursividad 
--EJERCICIO1
potencia x 0 = 1  
potencia x n = x * potencia x (n - 1)  

--EJERCICIO2
mcd a 0 = a  
mcd a b = mcd b (a `mod` b)  

--EJERCICIO3
pertenece _ [] = False  
pertenece x (y:ys)
    | x == y    = True   
    | otherwise = pertenece x ys 

--EJERCICIO4
tomar _ [] = []  
tomar 0 _ = []   
tomar n (x:xs) = x : tomar (n - 1) xs

--EJERCICIO5
digitosC 0 = []
digitosC n = digitosC (n `div` 10) ++ [n `mod` 10]

--EJERCICIO6
sumaDigitosR 0 = 0  
sumaDigitosR n = (n `mod` 10) + sumaDigitosR (n `div` 10)

--EJERCICIO2.1
ordenaRapida [] = []  
ordenaRapida (x:xs) = ordenaRapida [y | y <- xs, y <= x] ++ [x] ++ ordenaRapida [y | y <- xs, y > x]





--Nuevos Tipos De Datos

-- Definición del tipo Estudiante
data Estudiante = Estudiante
  { nombre        :: String
  , apellido      :: String
  , edad          :: Int
  , numeroControl :: String
  } deriving (Show)

-- Crear una lista de personas
listaEstudiantes :: [Estudiante]
listaEstudiantes = [
    Estudiante "Juan" "Pèrez" 30 "21160589",
    Estudiante "Ana" "Lopez" 22 "21160458",
    Estudiante "Luis" "Martinez" 25 "21160587",
    Estudiante "Beatriz" "Guzman" 27 "21160457",
    Estudiante "Carlos" "Diaz" 28 "21160453",
    Estudiante "Maria" "Sarmiento" 20 "21160692",
    Estudiante "Sandra" "Santiago" 21 "21148750",
    Estudiante "Miriam" "Velazco" 21 "21148790",
    Estudiante "Santiago" "Diaz" 21 "21148850",
    Estudiante "Soledad" "Lopez" 21 "21148500"
    ]

-- La función quicksort que toma una función de comparación
quicksortEdad [] = []
quicksortEdad (x:xs) = quicksortEdad [y | y <- xs, edad y <= edad x] ++ [x] ++ quicksortEdad [y | y <- xs, edad y > edad x]

ordenarPorEdad :: [Estudiante]
ordenarPorEdad = quicksortEdad listaEstudiantes

-- Obtener el estudiante menor
estudianteMenor :: [Estudiante] -> Estudiante
estudianteMenor xs = head xs  

-- Obtener el estudiante mayor
estudianteMayor :: [Estudiante] -> Estudiante
estudianteMayor xs = last xs  
--Calcular promedio
promedioEdades :: [Estudiante] -> Double
promedioEdades xs = fromIntegral (sum edades) / fromIntegral (length xs)
  where
    edades = map edad xs  


--Arboles 
--crear el tipo arbol
data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show)

-- Insertar un elemento en el árbol
insertar x Hoja = Nodo x Hoja Hoja
insertar x (Nodo y izq der)
    | x < y     = Nodo y (insertar x izq) der
    | x > y     = Nodo y izq (insertar x der)
    | otherwise = Nodo y izq der  -- No se inserta si ya existe

-- Insertar desde un arreglo
insertarDesdeArreglo xs = foldr insertar Hoja xs

-- Buscar un elemento en el árbol
buscar _ Hoja = False
buscar x (Nodo y izq der)
    | x == y  = True
    | x < y   = buscar x izq
    | x > y   = buscar x der

-- Recorrido inorden
inorden Hoja = []
inorden (Nodo x izq der) = inorden izq ++ [x] ++ inorden der

-- Recorrido preorden
preorden Hoja = []
preorden (Nodo x izq der) = [x] ++ preorden izq ++ preorden der

-- Recorrido posorden
posorden Hoja = []
posorden (Nodo x izq der) = posorden izq ++ posorden der ++ [x]



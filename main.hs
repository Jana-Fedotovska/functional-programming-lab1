import Data.List (sort)
import Data.Char (toUpper)

-- | Funkcija, kas atrod maksimālo vērtību sarakstā.
--
-- Saraksts tiek sakārtots augošā secībā un apgriezts, 
-- tiek izvadīta modificētā saraksta pirmā vērtība, 
-- kas ir maksimāla saraksta vērtība.
--
-- Parametri:
--
-- * @list@ - satur veselus skaitļus
--
-- Atgriežamā vērtība:
--
-- * atgriež saraksta lielāko vērtību
-- * tukšs saraksts izraisa kļūdu, tāpēc pārbaude jāveic pirms izsaukšanas
--
-- Piemērs:
--
-- >>> findMax [5, 10, 11, 15, 3, 8]
-- 15
findMax :: [Int] -> Int
findMax list =
    let sortedList = sort list 
        maxElement = head (reverse sortedList)
    in maxElement

-- | Funkcija, kas apgriež ievadīto vārdu.
--      
-- Ievdītajs vārds teik sadalīt sarakstā kur kats burts ir atsevišķs simbols,
-- tad sarakst tiek apgriests un katrs saraksta elements atkal apvienot vienā vārdā.
-- Parametri:
--
-- * @input@ - satur simbolu virkni
--
-- Atgriežamā vērtība:
--
-- * atgriež apgrieztu simbolu virkni
--
-- Piemērs:
--
-- >>> reverseString "Hello"
-- "olleH"
reverseString :: String -> String
reverseString input =
        let chars = map(:[]) input 
            reversedChars = reverse chars
            reversedString = concat reversedChars 
        in reversedString
-- | Funkcija, kas pārveido vārda pirmo burtu par lielo burtu
-- 
-- Parametri:
--          
-- * @input@ - satur simbolu virkni
--
-- Atgriežamā vērtība:
--
-- * atgriež simbolu virkni ar pirmo burtu lielo
--
firstToUpper :: String -> String
firstToUpper input = 
    let capitalize (x:xs) = toUpper x:xs 
        result = capitalize input 
    in result
-- | Funkcija, kas pārveido teikuma pirmo burtu par lielo burtu
--
-- Parametri:
--
-- * @word@ - satur simbolu virkni
--
-- * @restSentence@ - satur simbolu virkni
--
-- Atgriežamā vērtība:
--
-- * atgriež simbolu virkni ar pirmo burtu lielo
--
addWord :: String -> String -> String
addWord word restSentence =
    if null restSentence
        then firstToUpper word
        else firstToUpper word ++ " " ++ restSentence

-- | Funkcija, kas pārveido teikuma pirmo burtu par lielo burtu
--
-- Parametri:
-- * @input@ - satur simbolu virkni
--
-- Atgriežamā vērtība:
--
-- * atgriež simbolu virkni ar pirmo burtu lielo
--              
capitalizeSentence :: String -> String
capitalizeSentence input = foldr addWord "" (words input)
-- | Funkcija, kas aprēķina faktoriālu
fac n | n == 0    = 1
      | otherwise = n * fac (n - 1)
-- | Funkcija, kas aprēķina faktoriālu ar kļūdu pārbaudi
facUpgr n | n < 0     = error "Faktoriāls nav definēts negatīviem skaitļiem"
      | n == 0    = 1
      | otherwise = n * fac (n - 1)
-- | Funkcija, kas grupē saraksta elementus
--
--Funkcija, kas agrubē blakus esošo elementus, ja ite ier vienādi.
--
--Parametri:
--
-- * @list@ - satur veselus skaitļus
--
--Atgriežamā vērtība:
--
-- * atgriež sarakstu ar grupētiem elementiem
groupElements :: [Int] -> [[Int]]
groupElements [] = []
groupElements [x] = [[x]]
groupElements (x:y:xs) =
    if x == y
        then let rest       = groupElements (y : xs)
                 firstGroup = head rest
                 others     = tail rest
             in (x : firstGroup) : others
        else
            let rest = groupElements (y : xs)
            in [x] : rest
-- | Funkcija, kas sadala sarakstu n elementu grupās
--
-- Parametri:
--
-- * @n@ - satur veselus skaitļus
--
-- * @list@ - satur veselus skaitļus
--
-- Atgriežamā vērtība:
--
-- * atgriež sarakstu ar n elementu grupām
split :: Int -> [a] -> [[a]]
split n [] = []                         
split n list = take n list : split n (drop n list)        

main = do
    putStrLn "--------------------------------"
    putStrLn "1.uzdevums" 
    putStrLn "--------------------------------"
    let list = [5, 10, 11, 15, 3, 8]
    if null list
        then putStrLn "Sarakst ir tukšs, nevar atrast maksimālo vērtību."
        else putStrLn ("Maksimālā vērtība sarakstā ir " ++ show (findMax list))
    
    putStrLn "\n\n--------------------------------"
    putStrLn "2.uzdevums" 
    putStrLn "--------------------------------"
    putStrLn "Ievadiet vārdu:"
    inputString <- getLine
    if words inputString == [] 
        then putStrLn "Ievadītais vārds ir tukšs, nevar apgriezt."
        else putStrLn ("Ievadītais vārds apgrieztā secībā ir " ++ reverseString inputString)

    putStrLn "\n\n--------------------------------"
    putStrLn "3.uzdevums" 
    putStrLn "--------------------------------"

    putStrLn "Ievadiet teikumu(bez garumzīmēm):"
    inputSentence <- getLine
    if null (words inputSentence)
        then putStrLn "Ievadītais teikums ir tukšs."
        else putStrLn ("Teikums ar lielajiem burtiem: " ++ capitalizeSentence inputSentence)

    putStrLn "\n\n--------------------------------"
    putStrLn "4.uzdevums" 
    putStrLn "--------------------------------"


    putStrLn "Ievadiet skaitli, lai aprēķinātu faktoriālu:"
    inputNumber <- getLine 
    putStrLn ("Faktoriāls no " ++ inputNumber ++ " ir " ++ show (fac (read inputNumber :: Int)))    
    putStrLn ("Faktoriāls (uzlabota funkcija) no " ++ inputNumber ++ " ir " ++ show (facUpgr (read inputNumber :: Int)))    

    putStrLn "\n\n--------------------------------"
    putStrLn "5.uzdevums" 
    putStrLn "--------------------------------"

    let list2 = [1, 2, 2, 2, 3, 2, 4]

    putStrLn ("Saraksts: " ++ show list2)
    putStrLn ("Saraksts ar grupētiem elementiem: " ++ show (groupElements list2))

    putStrLn "\n\n--------------------------------"
    putStrLn "6.uzdevums" 
    putStrLn "--------------------------------"

    let list3 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 5, 6, 7, 8, 9]
    let n = 3   
    if n <= 0
        then putStrLn "Sadalīšanas skaitlis ir jābūt lielākam par 0."
        else do
            putStrLn ("Saraksts: " ++ show list3)
            putStrLn ("Saraksts sadalīts " ++ show n ++ " elementu grupās: " ++ show (split n list3))  
        

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

concatenateText a b = a ++ " " ++ b

capitalizeSentence input =
    let wordList = words input
        capitalizedWords = map firstToUpper wordList
        capitalizedSentence = foldl1 concatenateText capitalizedWords
    in capitalizedSentence


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
    if words inputSentence == [] 
        then putStrLn "Ievadītais teikums ir tukšs."
        else putStrLn ("Ievadītais teikums ar lielajiem burtiem ir " ++ capitalizeSentence inputSentence)
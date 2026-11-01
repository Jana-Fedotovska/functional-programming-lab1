0.uzdevums	3
1. uzdevums	3
2. uzdevums	4
3. uzdevums	5
4. uzdevums	6
5. uzdevums	7
6. uzdevums	8
7. uzdevums	8
8. uzdevums	10
Secinājumi	10


0.uzdevums

Lejupielādējiet un instalējiet savā datorā Haskell kompilatoru. 

Rezultāts:



uzdevums 
	Izveidot funkciju, lai noteiktu maksimālo skaitli no skaitļu saraksta.
	[5, 10, 11, 15, 3, 8]

	5.variants
kārtot sarakstu dilstošā secībā
izvadīt pirmo skaitli
Kods:
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
findMax :: [Int] -> Int
findMax list =
    let sortedList = sort list
        maxElement = head (reverse sortedList)
    in maxElement




Rezultāts:


2. uzdevums
	Izveidot funkciju, kas apgriež ievadītā vārda simbolus.
1.risinājums
definēt funkciju ar šādu funkcionalitāti:
ar nosaukumu, aprēķināt un izdrukāt apgriezto versiju. Programmai ir jāpieņem ievadu no lietotāja(vikrnes formātā)
sadalīt virkni prakstuzīju masīvā
sakārtot apgrieztā secībā
apveinot rakstuzīmes atpakaļ
Kods:
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

Rezultāts:


uzdevums
Izveidot funkciju, kas Katra Teikuma Vārda Pirmo Burtu Pārveido Par Lielo Burtu.
Programmai ir jāpieņem ievadi no lietotāja(virkni formātā)
2.risinājums
Uzrakstiet reducēšanas funkciju, kas katram vārdam pievieno teikumu ar atbilstošo pārveidoto vārdu.

Kods:
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
- | Funkcija, kas pārveido teikuma pirmo burtu par lielo burtu
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


Rezultāts:

uzdevums
Rekursīva funkcija ir jēgpilna tikai tad, ja ir izpildīts nosacījums, ka tās parametru vērtības katrā
rekursīvā izsaukumā kļūst vienkāršākas.
Apsveriet šādu faktoriāļa funkcijas definīciju:
fac n | n == 0 = 1
         | otherwise = n * fac (n − 1)
• Kas notiek, ja izsauc fac (−3)?
Funkcija ieiet bezgalīgā ciklā.

• Kā šo nosacījumu var precīzāk formulēt?
facUpgr n | n < 0     = error "Faktoriāls nav definēts negatīviem skaitļiem"
      | n == 0    = 1
      | otherwise = n * fac (n - 1)

Kods:
-- | Funkcija, kas aprēķina faktoriālu
fac n | n == 0    = 1
      | otherwise = n * fac (n - 1)
-- | Funkcija, kas aprēķina faktoriālu ar kļūdu pārbaudi
facUpgr n | n < 0     = error "Faktoriāls nav definēts negatīviem skaitļiem"
      | n == 0    = 1
      | otherwise = n * fac (n - 1)
 n * fac (n - 1)


uzdevums
Definējiet funkciju, kas grupē secīgus atkārtotus elementus sarakstā apakšsarakstos: piemēram, [1, 2, 2, 3, 2, 4] tiek pārveidots par [[1], [2, 2], [3], [2], [4]].

Kods:
-- | Funkcija, kas grupē saraksta elementus
--
--Funkcija, kas sagrubē blakus esošo elementus, ja tie ir vienādi.
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


Rezultāts:


uzdevums
Implementējiet funkciju split ar sekojošu tipu: split :: Int -> [a] -> [[a]].Šī funkcija sadala doto sarakstu apakšsarakstos, ku r katram apakšsarakstam ir norādītais garums. Tikai pēdējais apakšsaraksts var būt īsāks. 

Kods:
- | Funkcija, kas sadala sarakstu n elementu grupās
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




Rezultāts:


uzdevums

Apsveriet šādu bināro koku datu tipa definīciju, kuru mēs izmantosim, lai realizētu bināros meklēšanas kokus: data Tree a = Branch a (Tree a) (Tree a) | Leaf Uzrakstiet funkciju isSearchTree :: Tree a −> Bool , kas pārbauda, vai tās arguments ir binārais meklēšanas koks. Pēc tam pārbaudiet īpašību, ka, ja tiek dots binārais meklēšanas koks t, jaunas vērtības ievietošana kokā rada vēl vienu bināro meklēšanas koku. Kods jaunās vērtības ievietošanai kokā ir: insertTree :: Ord a => a −> Tree a −> Tree a insertTree e Leaf = Branch e Leaf Leaf insertTree e (Branch x li re) | e <= x = Branch x (insertTree e li) re | e > x = Branch x li (insertTree e re) Eksperimentējiet ar insertTree implementācijas mainīšanu, lai noskaidrotu, vai jūsu īpašība spēj atklāt, ka modificētā implementācija vairs neatspoguļo bināros meklēšanas kokus kā bināros meklēšanas kokus. 

Kods:
-- | Pārvērš bināro koku sarakstā, no kreisās puses uz labo.
--
-- Parametri:
--
-- * @tree@ - binārais koks
--
-- Atgriežamā vērtība:
--
-- * visu koka elementu saraksts (kreisais apakškoks, sakne, labais apakškoks)
-- * tukšam kokam atgriež tukšu sarakstu
treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList ( Branch x li re) = treeToList li ++ [x] ++ treeToList re


-- | Pārbauda, vai koks ir binārais koks.
--
-- Katram mezglam jāizpildās: visi kreisā apakškoka elementi ir @<=@ par mezgla
-- vērtību, visi labā apakškoka elementi ir @>@ par to, un abi apakškoki
-- paši ir meklēšanas koki.
--
-- Atgriežamā vērtība:
--
-- * @True@, ja koks ir meklēšanas koks (arī tukšam kokam)
-- * @False@ citādi
isTreeBS :: Ord a => Tree a -> Bool
isTreeBS Leaf = True
isTreeBS (Branch x li re) =
    all (<=x) (treeToList li)
    && all(>x) (treeToList re)
    && isTreeBS li
    && isTreeBS re


Rezultāts:


8.uzdevums

Pierādiet šādu likumu par list funkciju.
map f . concat = concat . map (map f)

Rezultāts:

Secinājums:
Rezultāts neatšķirās, jo funkcijas pielietošana katram elementam nav atkarīga no tā, vai elemneti ir jau salikti kopā vai nav.

Secinājumi

Veicot laboratorijas darbu tika apgūta haskell programmēšanas valoda. Uzdevumos tika risinātas dažādas problēmas, kas deva iespēju redzēt kā darbojas funkcionāla programmēšanas valoda.

Darbā grūtības sagādāja tas, ka valoda tika pielietota pirmo reizi un daudz laiak bija jāiegulda valdoas princupu apguvei un sintaksei.
Paši algitmi sākumā bija vienāršāki, taču 7-8 uzdevuma algritms sagādāja lielākas grūtībvas un bija nepieciešama padziļinātāka teorijas izpēte.

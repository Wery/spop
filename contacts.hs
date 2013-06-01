module Contacts where

import Data.Time
import System.Random
import System.IO
import Data.List
import Text.Regex.Posix
import Debug.Trace
import Data.Map
import Data.Maybe

plikOsoby = "osoby.txt"
plikGrupy = "grupy.txt"

-- ****************************************************************** FUNKCJE SPRAWDZAJACA POPRAWNOSC DANYCH WEJSCIOWYCH

patNazwa = "^[A-Z][a-z]*$"	
patTel = "^[0-9]{3}-[0-9]{3}-[0-9]{3}$"	
patEmail = "^[a-z0-9_.-]+@[a-z0-9_.-]+[.][a-z]{2,4}$"	
patData = "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"	
patInt = "^[0-9]*$"
 
pobierzDane pat = do{
	x <- getLine;
	if (x =~ pat :: Bool) then
		return x;
	else 
		do {
		putStrLn "sprobuj ponownie: ";
		pobierzDane pat;
		}		
}


-- ********************************************************************************** DODAWANIE OSOBY DO BAZY
	

dodajOsobe = do {
	putStr "imie: ";
	imie <- pobierzDane patNazwa;
	
	putStr "nazwisko: ";
	nazwisko <- pobierzDane patNazwa;
	
	putStr "firma: ";
	firma <- pobierzDane patNazwa;
	
	putStr "telefon: ";
	tel <- pobierzDane patTel;
	
	putStr "email: ";
	email <- pobierzDane patEmail;
	
	putStr "data urodzenia: ";
	dt_ur <- pobierzDane patData;
	
	dodajOsobeDoBD imie nazwisko firma tel email dt_ur;
	putStr "Dodano osobe do bazy!";
}	
								
dodajOsobeDoBD i n f t e d = do {
                osID  <- randomRIO (1,100000 :: Int);
				handler <- openFile plikOsoby AppendMode;
                hPutStrLn handler (show osID++" "++i++" "++n++" "++f++" "++show t++" "++e++" "++ (take 10 d));
                hClose handler;
}				

-- ********************************************************************************** DODAWANIE OSOBY DO GRUPY
dodajOsobeDoGrupy = do {
	putStrLn "Podaj nazwe grupy: ";
	gr <- getLine;
	putStrLn "Podaj ID osoby:";
	os <- getLine;
	
	dodajDoGrupy os gr;
}

dodajDoGrupy id_osoby nazwa = do {
	handler <- openFile plikGrupy AppendMode;
    hPutStrLn handler (nazwa++" "++show id_osoby);
	hClose handler;
}	
								
-- ********************************************************************************** WYSWIETLENIE WSZYSTKICH OSOB

wyswietlWszystkich = do{
	handler <- openFile plikOsoby ReadMode;
	wczytaj handler;
	hClose handler;
}

wczytaj hdl = do {
	t <- hIsEOF hdl;                                                
	if t then return()
	else do {
		contents <- hGetLine hdl;
		x<-return(contents);

		if x == [] then return()
		else do {
			putStr "ID ";  putStrLn((words x) !! 0);
			putStr "imie: ";  putStrLn((words x) !! 1);
			putStr "nazwisko: ";  putStrLn((words x) !! 2);
			putStr "firma: ";  putStrLn((words x) !! 3);
			putStr "telefon: ";  putStrLn((words x) !! 4);
			putStr "email: "; putStrLn((words x) !! 5);
			putStr "data urodzenia: ";  putStrLn((words x) !! 6);
			putStrLn " ";
			
			wczytaj hdl;                                
		}
	}
}	

-- ********************************************************************************** WCZYTAJ OSOBY DO TABLICY
wczytajDane hdl xs = do {
                    t <- hIsEOF hdl;                                                
                    if t then return xs; --return()
                    else do {
                    	contents <- hGetLine hdl;
                    	x<-return(contents);
                                                
                    	if x == [] then return xs; --return()
                    	else do {
                            wczytajDane hdl ([(words x)]++xs);
                       }
     		    }
}		


wczytajOsoby = do {
	 	handler <- openFile plikOsoby ReadMode;
        --wczytaj handler;
		listaOsob <- wczytajDane handler [];
		--print listaOsob;
		--putStrLn ("Wczytano osob: " ++ (show (length listaOsob)));
        hClose handler;
		return listaOsob;
}

-- ********************************************************************************** USOWANIE OSOBY O DANYM ID
-- usun wpis na liscie osob
usunOsobeZListy :: [[String]] -> Int -> [[String]]
usunOsobeZListy [] id = []
usunOsobeZListy (o:reszta) id 
		| o!!0 == (show id) = (usunOsobeZListy reszta id)
		| otherwise			= o:(usunOsobeZListy reszta id)

-- konwertuj liste na string		
listToString [] = []
listToString (o:osoby) = (listToString osoby)++(o!!0)++" "++(o!!1)++" "++(o!!2)++" "++(o!!3)++" "++(o!!4)++" "++(o!!5)++" "++(o!!6)++"\n"

-- wczytaj tablice osob, usun osobe i zapisz skonwertowana tablice w pliku
usunOsobe = do{
	putStrLn ("Podaj ID osoby do usuniecia:");
	id_os <- (pobierzDane patInt);
	listaOsob <- wczytajOsoby;
	writeFile plikOsoby (listToString (usunOsobeZListy listaOsob (read id_os :: Int)));	
	putStrLn ("Pomyslnie usunieto osobe o id " ++ id_os);
}

-- ********************************************************************************** EDYCJA OSOBY

edytujDaneOsoby = do {
	--let id_os = os!!0
	
	putStr "imie: ";
	imie <- pobierzDane patNazwa;
	{-
	if [] then imie = imie
	else imie = tmp
	-}
	
	putStr "nazwisko: ";
	nazwisko <- pobierzDane patNazwa;
	
	putStr "firma: ";
	firma <- pobierzDane patNazwa;
	
	putStr "telefon: ";
	tel <- pobierzDane patTel;
	
	putStr "email: ";
	email <- pobierzDane patEmail;
	
	putStr "data urodzenia: ";
	dt_ur <- pobierzDane patData;
	
	return (imie:nazwisko:firma:tel:email:dt_ur:[]);
}

edytujOsobeZListy :: [[String]] -> [String] -> Int -> [[String]]
edytujOsobeZListy [] _ id = []
edytujOsobeZListy (o:reszta) newOs id
		| o!!0 == (show id) = newOs:(edytujOsobeZListy reszta newOs id)
		| otherwise = o:(edytujOsobeZListy reszta newOs id)
		
		
edytujOsobe = do{
	putStrLn ("Podaj ID osoby do edycji:");
	id_os <- (pobierzDane patInt);
	listaOsob <- wczytajOsoby;
	
	-- SPRAWDZIC czy OSOBA ID ISTNIEJE
	listaOsob <- wczytajOsoby;
	if (sprawdz listaOsob (read id_os)) == True
	then do {
		putStrLn ("Edytuj dane:");	
		edytowaneDane <- edytujDaneOsoby;	
		print (id_os:edytowaneDane);
		--writeFile plikOsoby (listToString (usunOsobeZListy listaOsob (read id_os :: Int)));	
		writeFile plikOsoby (listToString (edytujOsobeZListy listaOsob (id_os:edytowaneDane) (read id_os :: Int)));	
		putStrLn ("Pomyslnie edytowano osobe o id " ++ id_os);
	}
	else do {
		putStrLn "Podano bledne ID. Sprobowac ponownie ? [T/N]";
		answ <- getLine;
		case answ of
		"T" -> edytujOsobe
		"N" -> return ()
		_   -> return (); 
	}
}

sprawdz :: [[String]] -> Int -> Bool
sprawdz [] i = False
sprawdz (x:xs) i
		| (head x) == (show i) = True
		| otherwise = sprawdz xs i
		
-- ********************************************************************************** SZUKANIE OSOBY ???

myParMap = [(0,"id"),(1,"imie"),(2,"nazwisko"),(3,"firma"),(4,"telefon"),(5,"email"),(6,"data")]

myFind xs par = find (\(_,a) -> a == par) xs

myfromJust :: Maybe a -> a
myfromJust Nothing = error "Maybe.fromJust: Nothing"
myfromJust (Just x) = x

currentTime = fmap show getCurrentTime
zonedTime = fmap show getZonedTime

getIDofPar :: [(Int, String)] -> String -> Int
getIDofPar x par = fst $ myfromJust $ myFind x par

szukajOsob fname par = do {
	handler <- openFile fname ReadMode;
	putStrLn "Podaj szukana fraze:";
	val <- getLine;
	check <- (szukajWierszOId handler par val);
	hClose handler;
}												
		
szukajWierszOId hdl par val = do {
	t <- hIsEOF hdl;                                                
	if t then return()
	else do {
		contents <- hGetLine hdl;
		x<-return(contents);

		if x == [] then putStrLn "Osoba nie istnieje !"
		else do {
			indeks <- return $ getIDofPar myParMap par;
			
			if (((words x)!!indeks) == val) then do {
				putStrLn " ";
				putStr "ID ";  putStrLn((words x) !! 0);
				putStr "imie: ";  putStrLn((words x) !! 1);
				putStr "nazwisko: ";  putStrLn((words x) !! 2);
				putStr "firma: ";  putStrLn((words x) !! 3);
				putStr "telefon: ";  putStrLn((words x) !! 4);
				putStr "email: "; putStrLn((words x) !! 5);
				putStr "data urodzenia: ";  putStrLn((words x) !! 6);
				szukajWierszOId hdl par val;
			}
			else szukajWierszOId hdl par val;
		}
	}						
}


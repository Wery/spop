module Main where

import Data.Time
import System.Random
import System.IO
import System.Directory
import System.Exit
import Data.List.Split
import Text.Regex.Posix
plikOsoby = "osoby.txt"
plikGrupy = "grupy.txt"
plikTemp = ".tmp"


-- ******************************************* DODAWANIE
			

dodajRekord fname i n f t e d = do {
	osID  <- randomRIO (1,100000 :: Int);
	handler <- openFile fname AppendMode;
	hPutStrLn handler (show osID++" "++i++" "++n++" "++f++" "++show t++" "++e++" "++ (take 10 d));
	hClose handler;
}				
								
--GRUPY***************************************************								

dodajGrupe fname nazwa = do {
	x <- sprawdzCzyGrupaIstnieje fname nazwa;
	if ( x ) then do {
		putStrLn("Taka grupa już istnieje!");
	}
	else do {
		grID  <- randomRIO (1,100000 :: Int);
		handler <- openFile fname AppendMode;
		hPutStrLn handler (show grID++" "++nazwa++" ");
		hClose handler;
	}
}

sprawdzCzyGrupaMaUsera fname id_grupy id_usera = do {
	handler <- openFile fname ReadMode;
	check <- (szukajUseraWGrupie handler id_grupy id_usera);
	hClose handler;
	return $ check;
	
}

szukajUseraWGrupie handler_grp id_grupy id_usera = do {
	t <- hIsEOF hdl;                                                
	if t then return $ False;
	else do {	
		contents <- hGetLine handler_grp;
		x<-return(contents);

		if x == [] then return $ False;
		else do {
				if (((words x)!!0) == id_grupy) then do {
					
					String::usersInGroup;
					usersInGroup = (words x)!!2;
					putStr(usersInGroup);
				}
				else do {
					 szukajUseraWGrupie handler_grp id_grupy id_usera;
				}
		}
	}
}

modyfikujWpisyGrup handler_grp handler_tmp nazwa_stara nazwa_nowa = do {
	t <- hIsEOF handler_grp;                                                
	if t then return()
	else do {
		contents <- hGetLine handler_grp;
		x<-return(contents);

		if x == [] then return();
		else do {
			if((head(tail(words x))) == nazwa_stara) then do {
				if ( (length (words x)) == 2 ) then do {
					-- Nie ma żadnych kontaktów przypisanych do grupy
					hPutStrLn handler_tmp (head(words x)++" "++nazwa_nowa);
					modyfikujWpisyGrup handler_grp handler_tmp nazwa_stara nazwa_nowa;                                
				}
				else do {
					-- Są kontakty przypisane do grupy
					hPutStrLn handler_tmp (head(words x)++" "++nazwa_nowa++" "++head(tail $ tail(words x)));
					modyfikujWpisyGrup handler_grp handler_tmp nazwa_stara nazwa_nowa;                                
				}
			}
			else do {
				hPutStrLn handler_tmp x;
				modyfikujWpisyGrup handler_grp handler_tmp nazwa_stara nazwa_nowa;                                
			}
		}
	}
}

zmienNazweGrupy filename_grp filename_tmp nazwa_stara nazwa_nowa = do {
	handler_grp <- openFile filename_grp ReadMode;
	handler_tmp <- openFile filename_tmp WriteMode;

	modyfikujWpisyGrup handler_grp handler_tmp nazwa_stara nazwa_nowa;
	
	hClose handler_grp;
	hClose handler_tmp;

	switchTmp plikGrupy plikTemp;
}

switchTmp filename filename_tmp = do {
	removeFile filename;
	renameFile filename_tmp filename;
	handler <- openFile plikTemp WriteMode;
	hClose handler;
}

usunOsobeZGrupy fname nazwa = do {
	x <- sprawdzCzyGrupaIstnieje fname nazwa;
	if ( x ) then do {
		putStrLn("Halo!");
	}
	else do {
		putStrLn("Taka grupa nie istnieje!");
	}

}

sprawdzCzyGrupaIstnieje fname val = do {
	handler <- openFile fname ReadMode;
	check <- (szukajGrupy handler "nazwa" val);
	hClose handler;
	return $ check;
}

szukajGrupy hdl par val = do {
	t <- hIsEOF hdl;                                                
	if t then return $ False;
	else do {	
		contents <- hGetLine hdl;
		x<-return(contents);

		if x == [] then return $ False;
		else do {
			case par of
				"nazwa" -> do {
					if (((words x)!!1) == val) then do {
						return $ True
					}
					else do {
						 szukajGrupy hdl par val;
					}
				}
		}
	}
}

wyswietlGrupy = do {
	handler <- openFile plikGrupy ReadMode;
	wczytajGrupy handler;
	hClose handler;
}

wczytajGrupy hdl = do {
	t <- hIsEOF hdl;                                                
	if t then return()
	else do {
		contents <- hGetLine hdl;
		x<-return(contents);

		if x == [] then return()
		else do {
			putStr "ID grupy: "; putStrLn (head(words x));
			putStr "Nazwa: "; putStrLn( head( tail(words x)));
			if ( (length (words x)) == 2 ) then do {
				putStrLn "Osoby z grupy: brak"; putStrLn(" ");
				wczytajGrupy hdl;
			}else do {
				putStr "Osoby z grupy: "; putStrLn( head $tail $ tail (words x));
				putStrLn(" ");
				wczytajGrupy hdl;
			}
		}
	}
}						
-- ***********************************************************


--OSOBY *************************
wyswietl = do {
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
			putStr "ID "; putStrLn (head(words x));
			putStr "imie: "; putStrLn( head( tail(words x)));
			putStr "nazwisko: "; putStrLn( head $tail $ tail (words x));
			putStr "firma: "; putStrLn( head $ tail $ tail $ tail (words x));
			putStr "telefon: "; putStrLn( head $ tail $ tail $ tail $ tail (words x)); 
			putStr "email: "; putStrLn( head $ tail $ tail $ tail $ tail $ tail (words x));
			putStr "data urodzenia: "; putStrLn( head $ tail $ tail $ tail $ tail $ tail $ tail  (words x));
			putStrLn(" ");

			wczytaj hdl;                                
		}
	}
}						
-- ***********************************************************

sprawdzCzyIDIstnieje fname par val = do {
	handler <- openFile fname ReadMode;
	check <- (szukajWierszOId handler par val);
	hClose handler;
	-- return $ check;
}										

szukajWierszOId hdl par val = do {
	t <- hIsEOF hdl;                                                
	if t then return()
	else do {
		contents <- hGetLine hdl;
		x<-return(contents);

		if x == [] then putStrLn ( "Osoba nie istnieje !"  )
		else do {
			case par of
				"id" -> if (((words x)!!0) == val) then do {
					putStr "ID "; putStrLn (head(words x));
					putStr "imie: "; putStrLn( head( tail(words x)));
					putStr "nazwisko: "; putStrLn( head $tail $ tail (words x));
					putStr "firma: "; putStrLn( head $ tail $ tail $ tail (words x));
					putStr "telefon: "; putStrLn( head $ tail $ tail $ tail $ tail (words x)); 
					putStr "email: "; putStrLn((words x) !! 5);
					putStr "data urodzenia: "; putStrLn( head $ tail $ tail $ tail $ tail $ tail $ tail  (words x));
				}
				else szukajWierszOId hdl par val;

				"imie" -> if (((words x)!!1) == val) then do {
					putStr "ID "; putStrLn (head(words x));
					putStr "imie: "; putStrLn( head( tail(words x)));
					putStr "nazwisko: "; putStrLn( head $tail $ tail (words x));
					putStr "firma: "; putStrLn( head $ tail $ tail $ tail (words x));
					putStr "telefon: "; putStrLn( head $ tail $ tail $ tail $ tail (words x)); 
					putStr "email: "; putStrLn((words x) !! 5);
					putStr "data urodzenia: "; putStrLn( head $ tail $ tail $ tail $ tail $ tail $ tail  (words x));
				}
				else szukajWierszOId hdl par val;
		}
	}						
}

currentTime = fmap show getCurrentTime
zonedTime = fmap show getZonedTime

pat1 = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"	
pat2 = "^[A-Z][a-z]*$"	
 
pobierzDane pat = do {
	x <- getLine;
	if (x =~ pat :: Bool) then
		return x;
	else 
		do {
			putStrLn "sprobuj ponownie: ";
			pobierzDane pat;
		}		
}
-- ************************ MENU *********************************
main = do
	putStrLn "=========================="
	putStrLn "== Menu główne programu =="
	putStrLn "=========================="
	putStrLn "1. Wyświetl kontakty"
	putStrLn "2. Zarządzaj kontaktami"
	putStrLn "3. Zarządzaj grupami"
	putStrLn "4. Wyszukiwanie kontaktów"
	putStrLn "5. Urodziny"
	putStrLn "6. Koniec"
	putStrLn "=========================="
	cmd <- getLine
	case cmd of
		"1" -> showContacts
		"2" -> manageContacts
		"3" -> manageGroups
		"4" -> searchContacts
		"5" -> birthday
		"6" -> exit
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       main

showContacts = do 
	wyswietl
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"6" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       showContacts

manageContacts = do
	putStrLn "== Zarządzanie kontaktami =="
	putStrLn "1. Dodaj kontakt"
	putStrLn "2. Usuń kontakt"
	putStrLn "3. Zmień kontakt"
	putStrLn "4. Przypisz kontakt do grupy"
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"6" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       manageContacts

manageGroups = do
	putStrLn "== Zarządzanie grupami =="
	putStrLn "1. Pokaż wszystkie grupy"
	putStrLn "2. Dodaj grupę"
	putStrLn "3. Zmień nazwę grupy"
	putStrLn "4. Usuń grupę"
	putStrLn "5. Dodaj użytkownika do grupy"
	putStrLn "6. Usuń użytkownika z grupy"
	putStrLn "7. Scal dwie grupy"
	putStrLn "8. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"1" -> do 
			wyswietlGrupy	
			manageGroups
		"2" -> do
			putStrLn "Podaj nazwę grupy: ";
			grpName <- getLine
		 	dodajGrupe plikGrupy grpName
			manageGroups
		"3" -> do
			putStrLn "Grupy obecne w systemie:"
			wyswietlGrupy	
			putStrLn "Podaj nazwę grupy do zmiany: ";
			grpNameOld <- getLine
			putStrLn "Podaj nową nazwę dla grupy: ";
			grpNameNew <- getLine
			zmienNazweGrupy plikGrupy plikTemp grpNameOld grpNameNew
			putStrLn "Gotowe.";
			manageGroups	
		"4" -> do
			putStrLn "Podaj nazwę grupy: ";
			grpName <- getLine
			
			manageGroups
		"8" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       manageGroups


searchContacts = do
	putStrLn "== Szukanie kontaktów =="
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"6" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       searchContacts

birthday = do
	putStrLn "== Osoby obchodzące dzisiaj urodziny =="
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"6" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       birthday 

exit = do
	exitWith ExitSuccess

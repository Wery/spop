module Groups where

import Data.Time
import System.Random
import System.IO
import System.Directory
import System.Exit
import System.Locale
import Data.List.Split
import Data.List
import Text.Regex.Posix

import Globals

--GRUPY***************************************************								

-- DODAWANIE NOWEJ GRUPY
dodajGrupe fname nazwa = do {
	x <- sprawdzCzyGrupaIstnieje fname nazwa "nazwa";
	if ( x == True ) then do {
		putStrLn("Taka grupa już istnieje!");
	}
	else do {
		grID  <- randomRIO (1,100000 :: Int);
		handler <- openFile fname AppendMode;
		hPutStrLn handler (show grID++" "++nazwa++" ");
		hClose handler;
	}
}

-- USUWANIE GRUPY
usunGrupe filename_grp filename_tmp nazwa = do {
	x <- sprawdzCzyGrupaIstnieje filename_grp nazwa "nazwa";
	if ( x ) then do {
		handler_grp <- openFile filename_grp ReadMode;
		handler_tmp <- openFile filename_tmp WriteMode;

		usunWpisGrupy handler_grp handler_tmp nazwa;
	
		hClose handler_grp;
		hClose handler_tmp;

		switchTmp plikGrupy plikTemp;
	}
	else do {
		putStrLn("Taka grupa nie istnieje!");
	}
}

usunWpisGrupy handler_grp handler_tmp nazwa = do {
	t <- hIsEOF handler_grp;                                                
	if t then return()
	else do {
		contents <- hGetLine handler_grp;
		x<-return(contents);

		if x == [] then return();
		else do {
			if((head(tail(words x))) == nazwa) then do {
				usunWpisGrupy handler_grp handler_tmp nazwa;                                
			}
			else do {
				hPutStrLn handler_tmp x;
				usunWpisGrupy handler_grp handler_tmp nazwa;                                
			}
		}
	}
}



-- ZMIANA NAZWY GRUPY
zmienNazweGrupy filename_grp filename_tmp id_grupy nazwa_nowa = do {
	handler_grp <- openFile filename_grp ReadMode;
	handler_tmp <- openFile filename_tmp WriteMode;

	modyfikujWpisyGrup handler_grp handler_tmp id_grupy nazwa_nowa;
	
	hClose handler_grp;
	hClose handler_tmp;

	switchTmp plikGrupy plikTemp;
}

modyfikujWpisyGrup handler_grp handler_tmp id_grupy nazwa_nowa = do {
	t <- hIsEOF handler_grp;                                                
	if t then return()
	else do {
		contents <- hGetLine handler_grp;
		x<-return(contents);

		if x == [] then return();
		else do {
			if((head(words x)) == id_grupy) then do {
				if ( (length (words x)) == 2 ) then do {
					-- Nie ma żadnych kontaktów przypisanych do grupy
					hPutStrLn handler_tmp (head(words x)++" "++nazwa_nowa);
					modyfikujWpisyGrup handler_grp handler_tmp id_grupy nazwa_nowa;                                
				}
				else do {
					-- Są kontakty przypisane do grupy
					hPutStrLn handler_tmp (head(words x)++" "++nazwa_nowa++" "++head(tail $ tail(words x)));
					modyfikujWpisyGrup handler_grp handler_tmp id_grupy nazwa_nowa; 
				}
			}
			else do {
				hPutStrLn handler_tmp x;
				modyfikujWpisyGrup handler_grp handler_tmp id_grupy nazwa_nowa;                                
			}
		}
	}
}

--DODAWANIE OSOBY DO GRUPY
dodajOsobeDoGrupy fname id_grupy id_usera = do {
	czyJestSens <- sprawdzCzyUserIDIstnieje plikOsoby id_usera;
	if ( czyJestSens ) then do {
		x <- sprawdzCzyGrupaIstnieje fname id_grupy "id";
		if ( x ) then do {
			y <- sprawdzCzyGrupaMaUsera fname id_grupy id_usera;
			if ( y ) then do {
				putStrLn("Ten użytkownik już istnieje!")
			}
			else do {
				handler_grp <- openFile fname ReadMode;
				handler_tmp <- openFile plikTemp WriteMode;

				dodawanieOsoby handler_grp handler_tmp id_grupy id_usera;

				hClose handler_grp;
				hClose handler_tmp;

				switchTmp plikGrupy plikTemp;
			}
		}
		else do {
			putStrLn("Taka grupa nie istnieje!");
		}
	}else do {
			putStrLn("Ten użytkownik nie istnieje!");
	}
}

dodawanieOsoby handler_grp handler_tmp id_grupy id_usera = do {
	t <- hIsEOF handler_grp;                                                
	if t then return()
	else do {
		contents <- hGetLine handler_grp;
		x<-return(contents);

		if x == [] then return();
		else do {
			if((head(words x)) == id_grupy) then do {
				if (length (splitOn "," ((words x)!!2)) > 0) then do {
					hPutStrLn handler_tmp (head(words x)++" "++((words x)!!1)++" "++((((words x)!!2)++",")++id_usera));
					dodawanieOsoby handler_grp handler_tmp id_grupy id_usera;
				}
				else do {
					hPutStrLn handler_tmp (head(words x)++" "++((words x)!!1)++" "++id_usera);
					dodawanieOsoby handler_grp handler_tmp id_grupy id_usera;
				}
			}
			else do {
				hPutStrLn handler_tmp x;
				dodawanieOsoby handler_grp handler_tmp id_grupy id_usera;                                
			}
		}
	}
}

-- MERGOWANIE GRUPY
polaczDwieGrupy fname id_grupy1 id_grupy2 name = do {
	new_grID  <- randomRIO (1,100000 :: Int);


	outGrp1 <- userzyWGrupie fname id_grupy1;
	outGrp2 <- userzyWGrupie fname id_grupy2;

	handler <- openFile fname AppendMode;

	--outGrp <- show nub ((show outGrp1)++(show outGrp2));
	--putStrLn (splitOn "," ((show outGrp1)++","++(show outGrp2)));
	hPutStrLn handler (show new_grID++" "++name++" "++((show outGrp1)++","++(show outGrp2)));

	hClose handler;

	--hPutStrLn handler (show new_grID++" "++name++" "++(nub ()));

}

-- USUWANIE OSOBY Z GRUPY
usunOsobeZGrupy fname id_grupy id_usera = do {
	x <- sprawdzCzyGrupaIstnieje fname id_grupy "id";
	if ( x ) then do {
		y <- sprawdzCzyGrupaMaUsera fname id_grupy id_usera;
		if ( y ) then do {
			handler_grp <- openFile fname ReadMode;
			handler_tmp <- openFile plikTemp WriteMode;

			usuwanieOsoby handler_grp handler_tmp id_grupy id_usera;

			hClose handler_grp;
			hClose handler_tmp;
			
			switchTmp plikGrupy plikTemp;
		}
		else putStrLn("Grupa istnieje ale nie ma tego użytkownika!")
	}
	else do {
		putStrLn("Taka grupa nie istnieje!");
	}
}


usuwanieOsoby handler_grp handler_tmp id_grupy id_usera = do {
	t <- hIsEOF handler_grp;                                                
	if t then return()
	else do {
		contents <- hGetLine handler_grp;
		x<-return(contents);

		if x == [] then return();
		else do {
			if((head(words x)) == id_grupy) then do {
				if (length (splitOn "," ((words x)!!2)) > 1) then do {
					hPutStrLn handler_tmp (head(words x)++" "++((words x)!!1)++" "++intercalate "," (filter (\x -> x /= id_usera) (splitOn "," ((words x)!!2))));
					usuwanieOsoby handler_grp handler_tmp id_grupy id_usera;
				}
				else do {
					hPutStrLn handler_tmp (head(words x)++" "++((words x)!!1));
					usuwanieOsoby handler_grp handler_tmp id_grupy id_usera;
				}
			}
			else do {
				hPutStrLn handler_tmp x;
				usuwanieOsoby handler_grp handler_tmp id_grupy id_usera;                                
			}
		}
	}
}

--WYSWIETLANIE GRUP
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

--FUNKCJE POMOCNICZE
switchTmp filename filename_tmp = do {
	removeFile filename;
	renameFile filename_tmp filename;
	handler <- openFile plikTemp WriteMode;
	hClose handler;
}

sprawdzCzyGrupaIstnieje fname val par = do
	handler <- openFile fname ReadMode
	if (par == "nazwa") then do {
		check <- (szukajGrupy handler "nazwa" val);
		hClose handler;
		return $ check;
	}
	else do {
		check <- (szukajGrupy handler "id" val);
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
				"id" -> do {
					--putStrLn(((words x)!!0));
	 				if (((words x)!!0) == val) then do {
						return $ True
					}
					else do {
						 szukajGrupy hdl par val;
					}

				}
		}
	}
}

sprawdzCzyGrupaMaUsera fname id_grupy id_usera = do {
	handler <- openFile fname ReadMode;
	check <- (szukajUseraWGrupie handler id_grupy id_usera);
	hClose handler;
	return $ check;
}

szukajUseraWGrupie handler_grp id_grupy id_usera = do {
	t <- hIsEOF handler_grp;                                                
	if t then return $ False;
	else do {	
		contents <- hGetLine handler_grp;
		x<-return(contents);

		if x == [] then return $ False;
		else do {
				if (((words x)!!0) == id_grupy) then do {
					if(elem id_usera (splitOn "," ((words x)!!2))) then do {
						return $ True;
					}
					else do {
						szukajUseraWGrupie handler_grp id_grupy id_usera;
					}
				}
				else do {
					 szukajUseraWGrupie handler_grp id_grupy id_usera;
				}
		}
	}
}

userzyWGrupie fname id_grupy = do {
	handler <- openFile fname ReadMode;
	check <- (pobierzUserowZGrupy handler id_grupy);
	hClose handler;
	return $ check;
}

pobierzUserowZGrupy handler_grp id_grupy = do {
	t <- hIsEOF handler_grp;                                                
	if t then return "";
	else do {	
		contents <- hGetLine handler_grp;
		x<-return(contents);

		if x == [] then return "";
		else do {
			if ((head(words x)) == id_grupy) then do {
				return $ ((words x)!!2);
			}
			else do {
				pobierzUserowZGrupy handler_grp id_grupy;
			}
		}
	}
}

-- ***********************************************************


--URODZINY ***************************************************

getCurrMonthAndDate :: IO String
getCurrMonthAndDate = do
   now <- getCurrentTime
   return (formatTime defaultTimeLocale "%m-%d" now)

urodziny fname = do {
	handler <- openFile fname ReadMode;
	todays_date <- getCurrMonthAndDate;
	check <- (szukajUrodzin handler todays_date);
	hClose handler;
}

szukajUrodzin hdl todays_date = do {
	t <- hIsEOF hdl;                                                
	if t then return()
	else do {
		contents <- hGetLine hdl;
		x<-return(contents);

		if x == [] then return();
		else do {
			if((drop 5 ((words x)!!6)) == todays_date) then do {
				putStr "ID "; putStrLn (head(words x));
				putStr "imie: "; putStrLn( head( tail(words x)));
				putStr "nazwisko: "; putStrLn( head $tail $ tail (words x));
				putStr "firma: "; putStrLn( head $ tail $ tail $ tail (words x));
				putStr "telefon: "; putStrLn( head $ tail $ tail $ tail $ tail (words x)); 
				putStr "email: "; putStrLn( head $ tail $ tail $ tail $ tail $ tail (words x));
				putStr "data urodzenia: "; putStrLn( head $ tail $ tail $ tail $ tail $ tail $ tail  (words x));
				putStrLn(" ");
			
				szukajUrodzin hdl todays_date
			}	
			else szukajUrodzin hdl todays_date
		}
	}						
}

--OSOBY *************************
dodajRekord fname i n f t e d = do {
	osID  <- randomRIO (1,100000 :: Int);
	handler <- openFile fname AppendMode;
	hPutStrLn handler (show osID++" "++i++" "++n++" "++f++" "++show t++" "++e++" "++ (take 10 d));
	hClose handler;
}

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

sprawdzCzyUserIDIstnieje fname val = do {
	handler <- openFile fname ReadMode;
	check <- (szukajIDUsera handler val);
	hClose handler;
	return $ check;
}

szukajIDUsera hdl val = do {
	t <- hIsEOF hdl;                                                
	if t then return $ False;
	else do {
		contents <- hGetLine hdl;
		x<-return(contents);

		if x == [] then do {
			 return $ False;
		}
		else do {
			if (((words x)!!0) == val) then do {
				return $ True;
			}
			else do { 
				szukajIDUsera hdl val;
			}
		}
	}

}

-- ***********************************************************

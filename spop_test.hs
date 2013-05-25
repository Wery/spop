import Data.Time
import System.Random
import System.IO
import Data.List.Split
import Text.Regex.Posix
plikOsoby = "c:/osoby.txt"
plikGrupy = "c:/grupy.txt"

-- ******************************************************************************************* DODAWANIE
dodajOsobeDoGrupy fname id_osoby nazwa = do {
								handler <- openFile fname AppendMode;
                                hPutStrLn handler (nazwa++" "++show id_osoby);
                                hClose handler;
								}				
								
dodajRekord fname i n f t e d = do {
                                osID  <- randomRIO (1,100000 :: Int);
								handler <- openFile fname AppendMode;
                                hPutStrLn handler (show osID++" "++i++" "++n++" "++f++" "++show t++" "++e++" "++ (take 10 d));
                                hClose handler;
								}				
								
-- ***********************************************************************************************								
wyswietl = do{
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
																		 
                                                                        wczytaj hdl;                                
                                                                }
                                                        }
                                                }						
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
                                                                                --return ()
                                                                        
																		
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
												{-					
wyswietlOs (Osoba {id_osoby=i, imie=imie, nazwisko=nazwisko, firma=firma, telefon=tel, email=email, data_ur=dt_ur})	=
			putStrLn ("ID: " ++ show id_osoby)
			putStrLn ("imie: " ++ imie)
			putStrLn ("nazwisko: " ++ nazwisko)
			putStrLn ("firma: " ++ firma)
			putStrLn ("telefon: " ++ show tel)
			putStrLn ("email: " ++ email)
			putStrLn ("data urodzenia: " ++ dt_ur)
			-}

--wyswietlWszystkich [] = return ()
--wyswietlWszystkich (o:os) = wyswietlOs o
									
currentTime = fmap show getCurrentTime
zonedTime = fmap show getZonedTime


main = do   {
			zt <- currentTime;
		    imie <- getLine;
			nazw <- getLine;
			firma <- getLine;
			tel <- getLine;
			email <- getLine;
			dodajRekord plikOsoby imie nazw firma tel email zt;
			main;
			
		}--print (take 10 zt)
	
pat1 = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"	
pat2 = "^[A-Z][a-z]*$"	

 
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
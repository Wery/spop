module Main where

import Contacts
import Groups
import Globals

import System.IO
import System.Exit

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
		"1" -> wyswietlWszystkich
		"2" -> manageContacts
		"3" -> manageGroups
		"4" -> searchContacts
		"5" -> birthday
		"6" -> exit
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       main
	main

manageContacts = do
	putStrLn "== Zarządzanie kontaktami =="
	putStrLn "1. Dodaj kontakt"
	putStrLn "2. Usuń kontakt"
	putStrLn "3. Zmień kontakt"
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"1" -> dodajOsobe
		"2" -> usunOsobe
		"3" -> edytujOsobe
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
		"1" -> wyswietlGrupy	
		"2" -> do
			putStrLn "Podaj nazwę grupy: ";
			grpName <- getLine
		 	dodajGrupe plikGrupy grpName
			manageGroups
		"3" -> do
			putStrLn "Grupy obecne w systemie:"
			wyswietlGrupy;
			putStrLn "Podaj ID grupy do zmiany: ";
			grpID <- getLine;
			putStrLn "Podaj nową nazwę dla grupy: ";
			grpNameNew <- getLine;
			zmienNazweGrupy plikGrupy plikTemp grpID grpNameNew;
			putStrLn "Gotowe.";
			manageGroups	
		"4" -> do
			putStrLn "Podaj ID grupy do usunięcia: ";
			grpID <- getLine;
			usunGrupe plikGrupy plikTemp grpID;
			putStrLn "Gotowe.";
			manageGroups
		"5" -> do
			putStrLn "Podaj ID grupy do której chcesz dodać użytkownika: ";
			grpID <- getLine;
			putStrLn "Podaj ID użytkownika którego chcesz dodać do tej grupy: ";
			userID <- getLine;
			dodajOsobeDoGrupy plikGrupy grpID userID;
			putStrLn "Gotowe.";
			manageGroups
		"6" -> do
			putStrLn "Podaj ID grupy: ";
			grpID <- getLine;
			putStrLn "Podaj ID użytkownika do usunięcia: ";
			userID <- getLine;
			usunOsobeZGrupy plikGrupy grpID userID;
			putStrLn "Gotowe.";
			manageGroups
		"7" -> do
			putStrLn "Podaj ID pierwszej grupy do scalenia: ";
			grpID1 <- getLine;
			putStrLn "Podaj ID drugiej grupy do scalenia: ";
			grpID2 <- getLine;
			putStrLn "Podaj nazwę dla nowej grupy: ";
			new_name <- getLine;
			manageGroups
		"8" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       manageGroups

searchContacts = do
	putStrLn "== Szukanie kontaktów =="
	putStrLn "Szukaj po:"
	putStrLn "1. ID"
	putStrLn "2. Imieniu"
	putStrLn "3. Nazwisku"
	putStrLn "4. Firmie"
	putStrLn "5. Telefonie"
	putStrLn "6. Emailu"
	putStrLn "7. Dacie urodzenia"
	putStrLn "8. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"1" -> szukajOsob plikOsoby "id"
		"2" -> szukajOsob plikOsoby "imie"
		"3" -> szukajOsob plikOsoby "nazwisko"
		"4" -> szukajOsob plikOsoby "firma"
		"5" -> szukajOsob plikOsoby "telefon"
		"6" -> szukajOsob plikOsoby "email"
		"7" -> szukajOsob plikOsoby "data"
		"8" -> main
		_   -> do { putStrLn "Nieprawidłowa opcja!";
					searchContacts; }
	searchContacts

birthday = do
	putStrLn "== Osoby obchodzące dzisiaj urodziny =="
	urodziny plikOsoby
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"6" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       birthday 

exit = do
	exitWith ExitSuccess

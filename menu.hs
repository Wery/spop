module Main where

import System.IO
import System.Exit

import Contacts

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
	putStrLn "4. Przypisz kontakt do grupy"
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"1" -> dodajOsobe
		"2" -> usunOsobe
		"3" -> edytujOsobe
		"4" -> dodajOsobeDoGrupy
		"6" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
	manageContacts

manageGroups = do
	putStrLn "== Zarządzanie grupami =="
	putStrLn "1. Dodaj grupę"
	putStrLn "2. Usuń grupę"
	putStrLn "3. Zmień grupę"
	putStrLn "4. Scal dwie grupy"
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"6" -> main
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
	putStrLn "6. Powrót do menu głównego"
	cmd <- getLine
	case cmd of
		"6" -> main
		_   -> do
		       putStrLn "Nieprawidłowa opcja!"
		       birthday 

exit = do
	exitWith ExitSuccess

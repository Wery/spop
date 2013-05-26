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
	putStrLn "== Wyświetlanie kontaktów =="
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

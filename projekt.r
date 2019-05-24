#' Konwersja temperatur I
#' 
#' @description Funkcja sluzaca do konwersji
#'  temperatury ze stopni Celsjusza do stopni Fahrenheita 
#' 
#' @comment Skala Fahrenheita uzywana jest m.in. w USA 
#'  zero przesuniete jest wzgledem zera skali Celsjusza o wartosc 32
#' 
#' @param temperatura_c wektor zawierajacy wartosci temperatury
#'  w stopniach Celsjusza
#' 
#' @return wektor numeryczny odpowiadajacy skali Fahrenheita
#' 
#' @examples
#' konwersja_c_f(20) 
#' konwersja_c_f(-65) 
#' konwersja_c_f(36.6)
#' konwersja_c_f(c(10,30,82)) 


konwersja_c_f = function(temperatura_c){
  (temperatura_c *1.8) + 32
}


#' Konwersja temperatur II
#' 
#' @description Funkcja sluzaca do konwersji temperatury 
#'  ze stopni Fahrenheita do stopni Kelvina
#' 
#' @comment Skala Kelvina nazywana jest bezwzgledna skala temperatur,
#'  powszechnie stosowana w badaniach naukowych na calym swiecie.
#'  
#' @param temperatura_f wektor zawierajacy wartosci temperatury
#'  w stopniach Fahrenheita
#'  
#' @return wektor numeryczny odpowiadajacy skali Kelvina
#'
#' @examples
#' konwersja_f_k(10)
#' konwersja_f_k(111)
#' konwersja_f_k(0)
#' konwersja_f_k(c(15,115,1115))


konwersja_f_k = function(temperatura_f){
  (temperatura_f + 459.67) *5/9
}

-----------------------------------------------------------------------------------------

#'Konwersja walut I
#'
#' @description Funkcja sluzaca zmianie waluty PLN
#'  na walute EUR i na odwrot
#' 
#' @comment Przy zamianie walut nalezy wziasc pod uwage kurs,
#'  ktory z dnia na dzien zmienia swoja wartosc
#'  
#' @param zl wektor odpowiadajacy wartosci w walucie PLN
#' 
#' @return wektor numeryczny
#' 
#' @examples
#' waluta_zl_eur(20)
#' waluta_zl_eur(250)
#' waluta_zl_eur(c(50,500,5000))
#' 
#' waluta_eur_zl(50)
#' waluta_eur_zl(500) 
#' waluta_eur_zl(c(10,40,90))

 
waluta_zl_eur = function(zl){
  (kurs=zl/4.307)
}

waluta_eur_zl = function(zl){
  (kurs=zl*4.307)
}



#' Konwersja walut II
#' 
#' @description Funkcja sluzaca zmianie waluty PLN
#'  na walute USD i na odwrot 
#' 
#' @comment Dolar amerykanski jest najwazniejsza waluta swiata,
#'  rozlicza sie w nim wiekszosc panstw swiata
#'  
#' @param zl wektor odpowiadajacy wartosci pieniedzy w walucie PLN
#' 
#' @return wektor numeryczny
#' 
#' @examples 
#' waluta_zl_usd(20)
#' waluta_zl_usd(100)
#' waluta_zl_usd(c(40,222,350))
#' 
#' waluta_usd_zl(20)
#' waluta_usd_zl(5)
#' waluta_usd_zl(c(10,50,200))


waluta_zl_usd = function(zl){
  (kurs=zl/3.854)
}
waluta_zl_usd(20)


waluta_usd_zl = function(zl){
  (kurs=zl*3.854)
}
waluta_usd_zl(20)


-----------------------------------------------------------------------------------------
  
#' Konwersja predkosci I  
#' 
#' @description Funkcja sluzaca zamianie km/h na mph i na odwrot
#' 
#' @comment Umiejetnosc przeliczania mph na km/h przydatna jest, gdy uzywamy aut
#'  pochodzacych z USA czy Wielkiej Brytanii, lub gdy wybieramy się do tych krajow,
#'  aby wiedziec z jaka predkoscia sie poruszamy
#'  
#' @param km wektor odpowiadajacy predkosci poruszania sie w km/h
#' 
#' @return wektor numeryczny
#' 
#' @warning
predkosc = -5
if(predkosc < 0){
  cat("predkosc nie moze byc ujemna")
}
#' 
#' @examples 
#' zamiana_kmh_mph(50)
#' zamiana_kmh_mph(120)
#' zamiana_kmh_mph(c(20,60,150))  
#'  
#' zamiana_mph_kmh(50)
#' zamiana_mph_kmh(140)
#' zamiana_mph_kmh(c(30,100,150))
  

zamiana_kmh_mph = function(km){
  (predkosc=km/1.609344)
}

zamiana_mph_kmh = function(km){
  (predkosc=km*1.609344)
}



#' Konwersja predkosci II
#' 
#' @description Funkcja sluzaca zamianie km/h na m/s i na odwrot
#' 
#' @comment Umiejetnosc przeliczania tych jednostek wykorzystywana jest
#'  przy obliczaniu predkosci wiatru, czy tez poruszania sie jakiejs jednostki 
#' 
#' @param km wektor odpowiadajacy predkosci poruszania sie w km/h
#' 
#' @return wektor numeryczny
#' 
#' @warning
predkosc = - 1
if(predkosc < 0){
  cat("predkosc nie moze byc ujemna")
}
#' 
#' @examples 
#' zamiana_kmh_ms(50)
#' zamiana_kmh_ms(100)
#' zamiana_kmh_ms(c(20,40,140))
#' 
#' zamiana_ms_kmh(2)
#' zamiana_ms_kmh(20)
#' zamiana_ms_kmh(c(5,10,15))


zamiana_kmh_ms = function(km){
  (predkosc=km*1000/3600)
}


zamiana_ms_kmh = function(km){
  (predkosc=km*3600/1000)
}


-----------------------------------------------------------------------------------


# Konwersja odleglosci morskich I

#' @description Funkcja sluzaca do konwersji odleglosci z km na miedzynarodowe mile morskie(inm) i na odwrot  
#' 
#' @comment Umiejetnosc przeliczania tych jednostek wykorzystywana jest w nawigacji morskiej oraz w lotnictwie,
#' dotyczy dlugosci, glebokosci, predkosci i kierunku
#'
#' 
#' @param km wektor zawierajacy wartosci odleglosci w kilometrach
#' 
#' @return wektor numeryczny
#' 
#' @warning 
odleglosc = -2 
if(odleglosc < -2){
  cat("odleglosc nie moze byc ujemna")
}

#' @examples
#' zamiana_km_inm(20)
#' zamiana_km_inm(100)
#' zamiana_km_inm(c(200,500,1000))
#' 
#' zamiana_inm_km(20)
#' zamiana_inm_km(100)
#' zamiana_inm_km(c(200,350,999))
 
   
zamiana_km_inm = function(km){ 
 (odleglosc = km / 1.852)
} 

zamiana_inm_km = function(km){
  (odleglosc = km * 1.852)
}


  
# Konwersja dlugosci

#' @description Funkcja sluzaca do zamiany jednostek
#'  dlugosci z cali na centrymetry
#' 
#' @param cal wektor odpowiadajacy dlugosci w cal
#'
#' @return wektor numeryczny w cm
#'
#' @warning dlugosci ujemne nie istnieja
#'
#' @examples
#' zamiana_cale_cm(2)
#' zamiana_cale_cm(0)
#' zamiana_cale_cm(c(10,20,30))


zamiana_cale_cm = function(cm){
  (cal = 2.54*cm)
}









#' Konwersja temperatur I
#' 
#' @description Funkcja służąca do konwersji
#'  temperatury ze stopni Celsjusza do stopni Fahrenheita 
#' 
#' @comment Skala Fahrenheita używana jest m.in. w USA 
#'  zero przesunięte jest względem zera skali Celsjusza o wartość 32
#' 
#' @param temperatura_c wektor zawierający wartości temperatury
#'  w stopniach Celsjusza
#' 
#' @return wektor numeryczny odpowiadający skali Fahrenheita
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
#' @description Funkcja służąca do konwersji temperatury 
#'  ze stopni Fahrenheita do stopni Kelvina
#' 
#' @comment Skala Kelvina nazywana jest bezwzgledna skala temperatur,
#'  powszechnie stosowana w badaniach naukowych na całym świecie.
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
#' @description Funkcja sluzaca zamianie waluty PLN
#'  na walute EUR i na odwrot
#' 
#' @comment Przy zamianie walut nalezy wziac pod uwage kurs,
#'  ktory z dnia na dzien zmienia swoja wartosc
#'  
#' @param zl wektor odpowiadajacy wartosci pieniedzy w walucie PLN
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
#' @description Funkcja sluzaca zamianie waluty PLN
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
#'  pochodzacych z USA czy Wielkiej Brytanii, lub gdy wybieramy się do tych krajów,
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


# Konwersja odległości I

konwersja_Mile_KM = function(mile) { 
 wynik = mile * 1.61
 wynik
} 

formals(konwersja_Mile_KM) 
body(konwersja_Mile_KM) 
environment(konwersja_Mile_KM) 
konwersja_Mile_KM(10)

# Konwersja odległości morskich I

konwersja_Nautical_mile_KM = function(mile) { 
  wynik = mile * 1.85
  wynik
} 

formals(konwersja_Nautical_mile_KM) 
body(konwersja_Nautical_mile_KM) 
environment(konwersja_Nautical_mile_KM) 
konwersja_Nautical_mile_KM(10)

  










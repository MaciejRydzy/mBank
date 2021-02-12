library(FinancialMath)

# typ modelu:
# mBank - oprocentowanie zmieniane decyzja zarzadu
# OSZDPU - Oprocentowanie Stale Z Dnia Podpisania Umowy
model_opr <- "mBank"

# wektor liczby dni w miesiacu: rok zwykly
dni_w_roku <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# wektor liczby dni w miesiacu: rok przestepny
dni_w_roku_p <- replace(dni_w_roku, dni_w_roku==28, 29)

# dane wejsciowe dla kredytu
dzien_splaty <- 5
transza <- c(12155.37, 12482.28, 12873.94, 12821.8, 12860.73, 12856.88, 12682.3, 23006.46)
oprocentowanie_mB <- rep(c(3.75, 4.05, 4.35, 4.60, 3.60, 3.95, 3.70, 3.55, 3.25, 2.35), times=c(2, 2, 9, 10, 2, 5, 10, 6, 55, 64))

# wektor z wysokoscia i krotnoscia rat pobranych przez mBank
rata_pobrana <- rep(c(102.03, 102.39, 171.71, 211.66, 249.18, 440.83, 445.37, 458.35, 458.36, 409.06, 425.92, 502.35, 539.58, 524.4, 515.76, 498.71, 457.04), times=c(rep(1, 6), 7, 9, 1, 2, 2, 1, 2, 10, 6, 55, 64))

# wektor liczby dni w miesiacu dla kolejnej raty
wek_dni_w_mies <- c(dni_w_roku[10:12], rep(c(dni_w_roku, dni_w_roku_p, dni_w_roku, dni_w_roku), length.out=356))

# ustalenie oprocentowania w zaleznosci od przyjetego modelu
oprocentowanie <- if (model_opr == "mBank") oprocentowanie_mB else c(rep_len(oprocentowanie_mB[1], 23), rep_len(oprocentowanie_mB[1] - 1, length(oprocentowanie_mB) - 23))

# wartosci poczatkowe
kapital <- transza[1]
oprocentowanie <- oprocentowanie / 100

# funkcja obliczajaca rate odsetkowa dla zmiennego kapitalu
odsetki_zmiana_kap <- function(oprocentowanie, kapital_poczatkowy, kapital_delta, okres_naliczania, dzien_zmiany) {
  odsetki <- (dzien_zmiany - 1 - dzien_splaty) / 365 * kapital_poczatkowy * oprocentowanie +
    (okres_naliczania - dzien_zmiany + 1 + dzien_splaty) / 365 * (kapital_poczatkowy + kapital_delta) * oprocentowanie
  
  return(list(kapital = kapital_poczatkowy + kapital_delta, odsetki = round(odsetki, digits = 2)))
}

##########################################
#
#                rata 1
#
# za okres od 11/9/2006 do 5/11/2006
# oprocentowanie 3.75%
# kapital 12155.37 do 10/10/2006
# kapital 24637.65 od 11/10/2006 
#
##########################################
rata <- 1
rata_odsetkowa <- (dni_w_roku[9] - 11 + 10) / 365 * kapital * oprocentowanie[rata]
kapital <- kapital + transza[2]
rata_odsetkowa <- rata_odsetkowa + (dni_w_roku[10] - 11 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[rata]
rata_odsetkowa <- round(rata_odsetkowa, digits = 2)

harmonogram <- data.frame(rata_calkowita = rata_odsetkowa,
                          kwota_kapitalu = 0,
                          kwota_odsetek = rata_odsetkowa,
                          saldo_zadluzenia_po_splacie_raty = kapital,
                          nadplata = rata_pobrana[rata] - rata_odsetkowa)


##########################################
#
#                rata 2
#
# za okres od 6/11/2006 do 5/12/2006
# oprocentowanie 3.75%
# kapital 24637.65 do 15/11/2006
# kapital 37511.59 od 16/11/2006 
#
##########################################
rata <- 2
przelicz <- odsetki_zmiana_kap(oprocentowanie = oprocentowanie[rata],
                               kapital_poczatkowy = kapital,
                               kapital_delta = transza[3],
                               okres_naliczania = wek_dni_w_mies[rata],
                               dzien_zmiany = 16)
kapital <- przelicz$kapital
harmonogram <- rbind(harmonogram, data.frame(rata_calkowita = przelicz$odsetki,
                                             kwota_kapitalu = 0,
                                             kwota_odsetek = przelicz$odsetki,
                                             saldo_zadluzenia_po_splacie_raty = kapital,
                                             nadplata = rata_pobrana[rata] - przelicz$odsetki))



##########################################
#
#                rata 3
#
# za okres od 6/12/2006 do 5/1/2007
# oprocentowanie 4.05% 
# kapital 37511.59 do 6/12/2006
# kapital 50333.39 od 7/12/2006 
#
##########################################
rata <- 3
przelicz <- odsetki_zmiana_kap(oprocentowanie = oprocentowanie[rata],
                               kapital_poczatkowy = kapital,
                               kapital_delta = transza[4],
                               okres_naliczania = wek_dni_w_mies[rata],
                               dzien_zmiany = 7)
kapital <- przelicz$kapital
harmonogram <- rbind(harmonogram, data.frame(rata_calkowita = przelicz$odsetki,
                                             kwota_kapitalu = 0,
                                             kwota_odsetek = przelicz$odsetki,
                                             saldo_zadluzenia_po_splacie_raty = kapital,
                                             nadplata = rata_pobrana[rata] - przelicz$odsetki))


##########################################
#
#                rata 4
#
# za okres od 6/1/2007 do 5/2/2007
# oprocentowanie 4.05% 
# kapital 50333.39 do 9/1/2007
# kapital 63194.12 od 10/1/2006 
#
##########################################
rata <- 4
przelicz <- odsetki_zmiana_kap(oprocentowanie = oprocentowanie[rata],
                               kapital_poczatkowy = kapital,
                               kapital_delta = transza[5],
                               okres_naliczania = wek_dni_w_mies[rata],
                               dzien_zmiany = 10)
kapital <- przelicz$kapital
harmonogram <- rbind(harmonogram, data.frame(rata_calkowita = przelicz$odsetki,
                                             kwota_kapitalu = 0,
                                             kwota_odsetek = przelicz$odsetki,
                                             saldo_zadluzenia_po_splacie_raty = kapital,
                                             nadplata = rata_pobrana[rata] - przelicz$odsetki))



##########################################
#
#                rata 5
#
# za okres od 6/2/2007 do 5/3/2007
# oprocentowanie 4.35% 
# kapital 63194.12 do 8/2/2007
# kapital 76051.00 od 9/2/2007 
#
##########################################
rata <- 5
przelicz <- odsetki_zmiana_kap(oprocentowanie = oprocentowanie[rata],
                               kapital_poczatkowy = kapital,
                               kapital_delta = transza[6],
                               okres_naliczania = wek_dni_w_mies[rata],
                               dzien_zmiany = 9)
kapital <- przelicz$kapital
harmonogram <- rbind(harmonogram, data.frame(rata_calkowita = przelicz$odsetki,
                                             kwota_kapitalu = 0,
                                             kwota_odsetek = przelicz$odsetki,
                                             saldo_zadluzenia_po_splacie_raty = kapital,
                                             nadplata = rata_pobrana[rata] - przelicz$odsetki))




##########################################
#
#                rata 6
#
# za okres od 6/3/2007 do 5/4/2007
# oprocentowanie 4.35% 
# kapital 63194.12 do 8/3/2007
# kapital 88733.30 od 9/3/2007 
#
##########################################
rata <- 6
rata_odsetkowa <- (8 - dzien_splaty) / 365 * kapital * oprocentowanie[rata]
kapital <- kapital + transza[7]
rata_odsetkowa <- rata_odsetkowa + (wek_dni_w_mies[rata] - 9 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[rata]
rata_odsetkowa <- round(rata_odsetkowa, digits = 2)
rata_calkowita <- round(amort.period(Loan = kapital, n = 354, i = oprocentowanie[rata], ic = 12, pf = 12)[2], digits = 2)
# rata kapitalowa liczona tak, jakby odsetki przez caly miesiac dotyczyly kwoty po uruchomieniu transza[7]
rata_kapitalowa <- rata_calkowita - round(wek_dni_w_mies[rata] / 365 * kapital * oprocentowanie[rata], digits = 2)
kapital <- kapital - rata_kapitalowa

harmonogram <- rbind(harmonogram, data.frame(rata_calkowita = sum(rata_kapitalowa, rata_odsetkowa),
                                             kwota_kapitalu = rata_kapitalowa,
                                             kwota_odsetek = rata_odsetkowa,
                                             saldo_zadluzenia_po_splacie_raty = kapital,
                                             nadplata = round(rata_pobrana[rata] - sum(rata_kapitalowa, rata_odsetkowa), digits = 2)))





##########################################
#
#                raty 7 -> 27
#
##########################################
for(rata in 7:27) {
  if (oprocentowanie[rata] != oprocentowanie[rata - 1]) {
    rata_calkowita <- round(amort.period(Loan = kapital, n = 360 - rata, i = oprocentowanie[rata], ic = 12, pf = 12)[2], digits = 2)
  }
  rata_odsetkowa <- round(wek_dni_w_mies[rata] / 365 * oprocentowanie[rata] * kapital, digits = 2)
  rata_kapitalowa <- rata_calkowita - rata_odsetkowa
  kapital <- kapital - rata_kapitalowa
  harmonogram <- rbind(harmonogram, data.frame(rata_calkowita = sum(rata_kapitalowa, rata_odsetkowa),
                                               kwota_kapitalu = rata_kapitalowa,
                                               kwota_odsetek = rata_odsetkowa,
                                               saldo_zadluzenia_po_splacie_raty = kapital,
                                               nadplata = rata_pobrana[rata] - rata_calkowita))
}




##########################################
#
#                rata 28
#
# za okres od 6/1/2009 do 5/2/2009
# oprocentowanie 3.95% 
# kapital 85935.47 do 20/1/2009
# kapital 108866.67 od 21/1/2009
#
##########################################
rata <- 28
rata_odsetkowa <- (20 - dzien_splaty) / 365 * kapital * oprocentowanie[rata]
kapital <- kapital + transza[8] - sum(35.28, 39.98)
rata_odsetkowa <- rata_odsetkowa + (wek_dni_w_mies[rata] - 21 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[rata]
rata_odsetkowa <- round(rata_odsetkowa, digits = 2)
rata_calkowita <- round(amort.period(Loan = kapital, n = 360 - rata, i = oprocentowanie[rata], ic = 12, pf = 12)[2], digits = 2)
# rata kapitalowa liczona tak, jakby odsetki przez caly miesiac dotyczyly kwoty po uruchomieniu transza[8] - sum...
rata_kapitalowa <- rata_calkowita - round(wek_dni_w_mies[rata] / 365 * oprocentowanie[rata] * kapital, digits = 2)
kapital <- kapital - rata_kapitalowa
harmonogram <- rbind(harmonogram, data.frame(rata_calkowita = sum(rata_kapitalowa, rata_odsetkowa),
                                             kwota_kapitalu = rata_kapitalowa,
                                             kwota_odsetek = rata_odsetkowa,
                                             saldo_zadluzenia_po_splacie_raty = kapital,
                                             nadplata = rata_pobrana[rata] - sum(rata_kapitalowa, rata_odsetkowa)))




##########################################
#
#                raty 29 -> 
#
##########################################
for(rata in 29:165) {
  if (oprocentowanie[rata] != oprocentowanie[rata - 1]) {
    rata_calkowita <- round(amort.period(Loan = kapital, n = 360 - rata, i = oprocentowanie[rata], ic = 12, pf = 12)[2], digits = 2)
  }
  rata_odsetkowa <- round(wek_dni_w_mies[rata] / 365 * oprocentowanie[rata] * kapital, digits = 2)
  rata_kapitalowa <- rata_calkowita - rata_odsetkowa
  kapital <- kapital - rata_kapitalowa
  harmonogram <- rbind(harmonogram, data.frame(rata_calkowita = sum(rata_kapitalowa, rata_odsetkowa),
                                               kwota_kapitalu = rata_kapitalowa,
                                               kwota_odsetek = rata_odsetkowa,
                                               saldo_zadluzenia_po_splacie_raty = kapital,
                                               nadplata = rata_pobrana[rata] - rata_calkowita))
}

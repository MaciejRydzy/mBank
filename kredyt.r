library(FinancialMath)

# liczba dotad splaconych rat
lba_splaconych_rat <- 174

# wektor liczby dni w miesiacu: rok zwykly
dni_w_roku <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# wektor liczby dni w miesiacu: rok przestepny
dni_w_roku_p <- replace(dni_w_roku, dni_w_roku==28, 29)

# wektor liczby dni w miesiacu dla kolejnej raty
wek_dni_w_mies <- c(dni_w_roku[7:12], rep(c(dni_w_roku, dni_w_roku_p, dni_w_roku, dni_w_roku), length.out=353))

# wektor liczby dni w roku dla kolejnej raty
wek_dni_w_roku <- c(rep(365, times=6), rep(rep(c(365,366,365,365), each=12), length.out=353))

# na podstawie § 11 ust 7 rok liczy zawsze 365 dni
dni_w_roku_sum <- sum(dni_w_roku)

# wektory z wysokoscia rat pobranych przez mBank
raty_pobrane <- rep(c(479.42, 334.69, 306.84, 306.83, 306.84, 318.53, 330.45, 342.35, 332.86, 327.42, 316.68, 290.48), times=c(1,1,1,1,1,2,6,20,10,6,55,70))

# wczytanie danych z kursami walut
kursy_walut <- read.csv('~/R/mBank/kursy_walut.csv', header = TRUE)

# daty ksiegowania rat
data_ksiegowania <- as.vector(kursy_walut$data)


##########################################
#
#               rata #1
#
##########################################
kapital_poczatkowy <- 72161.65
oprocentowanie <- 3.45
poz_lba_rat <- 359
dni_w_mcu <- c("cze" = 23, "lip" = 31)
##########################################
cat("-----------  rata #1  -----------\n")

# wyliczenie raty kapitalowo-odsetkowej wylacznie w miesiacu lipcu
oprocentowanie <- oprocentowanie / 100
rata_calkowita <- amort.period(Loan = kapital_poczatkowy, n = poz_lba_rat, i = oprocentowanie, ic = 12, pf = 12)
rata_calkowita <- round(rata_calkowita[2], digits = 2)

# wyliczenie raty odsetkowej: suma odsetek w miesiącach czerwiec oraz lipiec
rata_odsetkowa <- dni_w_mcu / 365 * kapital_poczatkowy * oprocentowanie
cat("rata_odsetkowa[1] =", round(sum(rata_odsetkowa), digits = 2), "\n")

# wyliczenie raty kapitalowej
rata_kapitalowa <- rata_calkowita - round(as.vector(rata_odsetkowa["lip"]), digits = 2)
cat("rata_kapitalowa[1] =", rata_kapitalowa, "\n")

# kapital pozostajacy do splaty po racie #1
kapital <- kapital_poczatkowy - rata_kapitalowa
cat("kapital_do_splaty[1] =", kapital, "\n\n")

harmonogram <- data.frame(termin_splaty = data_ksiegowania[1], 
                          kwota_kapitalu = as.vector(rata_kapitalowa),
                          kwota_odsetek = round(sum(rata_odsetkowa), digits = 2),
                          kwota_raty_lacznie = rata_calkowita + round(as.vector(rata_odsetkowa["cze"]), digits = 2),
                          saldo_zadluzenia_po_splacie_raty = kapital, 
                          nadplata = 0)





##########################################
#
#               rata #2
#
##########################################
oprocentowanie <- c(3.45,2.45)
oprocentowanie_mB <- 3.75       # oprocentowanie ustalone przez mBank
dni_oprocentowania <- c(1,30)   # jeden dzień z oprocentowaniem 3,45% oraz 30 dni z oprocentowaniem 2,45%
poz_lba_rat <- 358
##########################################
cat("-----------  rata #2  -----------\n")

# wyliczenie raty calkowitej
oprocentowanie <- oprocentowanie / 100
dni_oprocentowania <- dni_oprocentowania / 365
rata_calkowita <- amort.period(Loan = kapital, n = poz_lba_rat, i = oprocentowanie[1], ic = 12, pf = 12)
rata_calkowita <- round(rata_calkowita[2], digits = 2)

# wyliczenie raty odsetkowej
rata_odsetkowa <- sum(kapital * oprocentowanie * dni_oprocentowania)
rata_odsetkowa <- round(rata_odsetkowa, digits = 2)
cat("rata_odsetkowa[2] =", rata_odsetkowa, "\n")

# wyliczenie raty kapitalowej
rata_kapitalowa <- rata_calkowita - rata_odsetkowa
cat("rata_kapitalowa[2] =", rata_kapitalowa, "\n")

# kapital pozostajacy do splaty po racie #2
kapital_mB <- kapital
kapital <- kapital - rata_kapitalowa
cat("kapital_do_splaty[2] =", kapital, "\n")

# wyliczenie nadplaty
oprocentowanie_mB <- oprocentowanie_mB / 100
rata_calkowita_mB <- amort.period(Loan = kapital_mB, n = poz_lba_rat, i = oprocentowanie_mB, ic = 12, pf = 12)
rata_calkowita_mB <- round(rata_calkowita_mB[2], digits = 2)
nadplata <- rata_calkowita_mB - rata_calkowita
cat("nadplata[2] =", nadplata, "\n\n")

harmonogram <- rbind(harmonogram, data.frame(termin_splaty = data_ksiegowania[2],
                                             kwota_kapitalu = rata_kapitalowa,
                                             kwota_odsetek = rata_odsetkowa,
                                             kwota_raty_lacznie = rata_calkowita,
                                             saldo_zadluzenia_po_splacie_raty = kapital, 
                                             nadplata = nadplata))




##########################################
#
#           rata #3 i kolejne
#
##########################################
oprocentowanie = 2.45
poz_lba_rat <- 357
##########################################
cat("----------  rata #3 +  ----------\n")

# wyliczenie raty calkowitej
oprocentowanie <- oprocentowanie / 100
rata_calkowita <- amort.period(Loan = kapital, n = poz_lba_rat, i = oprocentowanie, ic = 12, pf = 12)
rata_calkowita <- round(rata_calkowita[2], digits = 2)
cat("rata_calkowita =", rata_calkowita, "\n\n")

# wyliczenie pozostalej czesci harmonogramu
for(rata in 3:359) {
  rata_odsetkowa <- round(kapital * oprocentowanie * wek_dni_w_mies[rata] / dni_w_roku_sum, digits = 2)
  rata_kapitalowa <- rata_calkowita - rata_odsetkowa
  kapital <- kapital - rata_kapitalowa
  
  harmonogram <- rbind(harmonogram, data.frame(termin_splaty = data_ksiegowania[rata],
                                               kwota_kapitalu = rata_kapitalowa,
                                               kwota_odsetek = rata_odsetkowa,
                                               kwota_raty_lacznie = rata_calkowita,
                                               saldo_zadluzenia_po_splacie_raty = kapital, 
                                               nadplata = raty_pobrane[rata] - rata_calkowita))
}

cat("-----  stan na 01.01.2021  -----\n")
splacony_kapital <- sum(harmonogram$kwota_kapitalu[1:174])
kapital_do_splaty <- kapital_poczatkowy - splacony_kapital
cat("splacony_kapital =", splacony_kapital, "\n")
cat("kapital_do_splaty =", kapital_do_splaty, "\n")




##########################################
#
#         wyliczenie nadplaty
#
##########################################
# raty 1 do 82 liczone po kursie mBanku
nadplata_PLN_mBank <- round(harmonogram$nadplata[1:82] * kursy_walut$kurs.mBank[1:82], digits = 2)

# od raty 83 splata bezposrednio w CHF, nadplata liczona po kursie srednim NBP
nadplata_PLN_NBP <- round(harmonogram$nadplata[83:lba_splaconych_rat] * kursy_walut$kurs.NBP[83:lba_splaconych_rat], digits = 2)

nadplata_PLN <- c(nadplata_PLN_mBank, nadplata_PLN_NBP)
cat("nadplata =", sum(nadplata_PLN, na.rm=TRUE), "PLN\n")

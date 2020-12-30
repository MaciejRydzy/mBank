library(FinancialMath)



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

harmonogram <- data.frame(rata = rata_calkowita + round(as.vector(rata_odsetkowa["cze"]), digits = 2), kapital = as.vector(rata_kapitalowa), odsetki = round(sum(rata_odsetkowa), digits = 2), kapital_do_splaty = kapital, nadplata = 0)




##########################################
#
#               rata #2
#
##########################################
oprocentowanie <- c(3.45,2.45)
oprocentowanie_mB <- 3.75
dni_oprocentowania <- c(1,30) # jeden dzień z oprocentowaniem 3,45% oraz 30 dni z oprocentowaniem 2,45%
poz_lba_rat <- 358
##########################################

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

harmonogram <- rbind(harmonogram, data.frame(rata = rata_calkowita[], kapital = rata_kapitalowa, odsetki = rata_odsetkowa, kapital_do_splaty = kapital, nadplata = nadplata))




##########################################
#
#           rata #3 i kolejne
#
##########################################
oprocentowanie = 2.45
poz_lba_rat <- 357
##########################################
# wektor liczby dni w miesiacu: rok zwykly
dni_w_roku <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# wektor liczby dni w miesiacu: rok przestepny
dni_w_roku_p <- dni_w_roku
dni_w_roku_p[2] <- 29

# wektor liczby dni w miesiacu dla danej raty
wek_dni_w_mies <- c(dni_w_roku[9:12], rep(c(dni_w_roku, dni_w_roku_p, dni_w_roku, dni_w_roku), times = 7), dni_w_roku, dni_w_roku_p[1:5])
# wektor liczby dni w roku dla danej raty
wek_dni_w_roku <- c(rep(365, times = 4), rep(c(rep(365, times = 12), rep(366, times = 12), rep(365, times = 12), rep(365, times = 12)), times = 7), rep(365, times = 12), rep(366, times = 5))

# wektory z ustalonym przez mBank poziomem oprocentowania oraz wysokoscia pobranych rat poczawszy od raty #3
oprocentowanie_raty <- rep(c(3.05, 3.35, 3.65, 3.95, 3.70, 3.55, 3.25, 2.35), times=c(3,2,6,20,10,6,55,70))
raty_pobrane <- rep(c(306.84, 306.83, 306.84, 318.53, 330.45, 342.35, 332.86, 327.42, 316.68, 290.48), times=c(1,1,1,2,6,20,10,6,55,70))
##########################################

# wyliczenie raty calkowitej
oprocentowanie <- oprocentowanie / 100
rata_calkowita <- amort.period(Loan = kapital, n = poz_lba_rat, i = oprocentowanie, ic = 12, pf = 12)
rata_calkowita <- round(rata_calkowita[2], digits = 2)
cat("rata_calkowita =", rata_calkowita, "\n")

# wyliczenie pozostalej czesci harmonogramu
for(rata in 1:length(wek_dni_w_mies)) { 
  rata_odsetkowa <- round(kapital * oprocentowanie * wek_dni_w_mies[rata] / wek_dni_w_roku[rata], digits = 2)
  rata_kapitalowa <- rata_calkowita - rata_odsetkowa
  kapital <- kapital - rata_kapitalowa
  
  harmonogram <- rbind(harmonogram, data.frame(rata = rata_calkowita, kapital = rata_kapitalowa, odsetki = rata_odsetkowa, kapital_do_splaty = kapital, nadplata = raty_pobrane[rata] - rata_calkowita))
}

splacony_kapital <- sum(harmonogram[1:174,1])
cat("splacony_kapital =", splacony_kapital, "\n")
cat("kapital_do_splaty =", kapital_poczatkowy - splacony_kapital, "\n")
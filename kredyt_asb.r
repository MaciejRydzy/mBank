library(FinancialMath)

# wektor liczby dni w miesiacu: rok zwykly
dni_w_roku <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# wektor liczby dni w miesiacu: rok przestepny
dni_w_roku_p <- replace(dni_w_roku, dni_w_roku==28, 29)

# dane wejsciowe dla kredytu
dzien_splaty <- 5
transza <- c(12155.37, 12482.28, 12873.94, 12821.8, 12860.73, 12856.88, 12682.3, 23006.46)
oprocentowanie_mB <- c(3.75, 4.05, 4.35, 4.60, 3.60, 3.95, 3.70, 3.55, 3.25, 2.35)

# wybor pomiedzy oprocentowaniem narzuconym przez mBank a oprocentowaniem stalym
# tylko jedna linia ma byc odkomentowana!
oprocentowanie <- oprocentowanie_mB # oprocentowanie mBanku
# oprocentowanie <- rep_len(oprocentowanie_mB[1], length(oprocentowanie_mB)) # oprocentowanie stale

# wartosci poczatkowe
kapital <- transza[1]
oprocentowanie <- oprocentowanie / 100

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
rata_odsetkowa <- (dni_w_roku[9] - 11 + 10) / 365 * kapital * oprocentowanie[1]
kapital <- kapital + transza[2]
rata_odsetkowa <- rata_odsetkowa + (dni_w_roku[10] - 11 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[1]

harmonogram <- data.frame(kwota_kapitalu = 0,
                          kwota_odsetek = round(rata_odsetkowa, digits = 2),
                          saldo_zadluzenia_po_splacie_raty = kapital)


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
rata_odsetkowa <- (15 - dzien_splaty) / 365 * kapital * oprocentowanie[1]
kapital <- kapital + transza[3]
rata_odsetkowa <- rata_odsetkowa + (dni_w_roku[11] - 16 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[1]
harmonogram <- rbind(harmonogram, data.frame(kwota_kapitalu = 0,
                                             kwota_odsetek = round(rata_odsetkowa, digits = 2),
                                             saldo_zadluzenia_po_splacie_raty = kapital))



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
rata_odsetkowa <- (6 - dzien_splaty) / 365 * kapital * oprocentowanie[2]
kapital <- kapital + transza[4]
rata_odsetkowa <- rata_odsetkowa + (dni_w_roku[12] - 7 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[2]
harmonogram <- rbind(harmonogram, data.frame(kwota_kapitalu = 0,
                                             kwota_odsetek = round(rata_odsetkowa, digits = 2),
                                             saldo_zadluzenia_po_splacie_raty = kapital))



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
rata_odsetkowa <- (9 - dzien_splaty) / 365 * kapital * oprocentowanie[2]
kapital <- kapital + transza[5]
rata_odsetkowa <- rata_odsetkowa + (dni_w_roku[1] - 10 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[2]
harmonogram <- rbind(harmonogram, data.frame(kwota_kapitalu = 0,
                                             kwota_odsetek = round(rata_odsetkowa, digits = 2),
                                             saldo_zadluzenia_po_splacie_raty = kapital))




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
rata_odsetkowa <- (8 - dzien_splaty) / 365 * kapital * oprocentowanie[3]
kapital <- kapital + transza[6]
rata_odsetkowa <- rata_odsetkowa + (dni_w_roku[2] - 9 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[3]
harmonogram <- rbind(harmonogram, data.frame(kwota_kapitalu = 0,
                                             kwota_odsetek = round(rata_odsetkowa, digits = 2),
                                             saldo_zadluzenia_po_splacie_raty = kapital))




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
rata_odsetkowa <- (8 - dzien_splaty) / 365 * kapital * oprocentowanie[3]
kapital <- kapital + transza[7]
rata_odsetkowa <- rata_odsetkowa + (dni_w_roku[3] - 9 + 1 + dzien_splaty) / 365 * kapital * oprocentowanie[3]
harmonogram <- rbind(harmonogram, data.frame(kwota_kapitalu = 0,
                                             kwota_odsetek = round(rata_odsetkowa, digits = 2),
                                             saldo_zadluzenia_po_splacie_raty = kapital))

# rata_calkowita <- amort.period(Loan = kapital, n = 359, i = oprocentowanie[3], ic = 12, pf = 12)

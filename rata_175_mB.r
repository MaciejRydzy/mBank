library(FinancialMath)

# kapital pozostajacy do splacenia
kapital <- 45058.08

# liczba pozostalych rat
lba_pozostalych_rat <- 185

# oprocentowanie
oprocentowanie <- c(2.35, 2.45)

# liczba dni w jakich obowiazywalo oprocentowanie
# 20.01.2021  ZMIANA WYSOKOSCI ODSETEK
dni_oprocentowania <- c(20, 11)

##########################################
#
#               rata #175
#
##########################################

oprocentowanie <- oprocentowanie / 100
rata <- amort.period(Loan = kapital, n = lba_pozostalych_rat, i = oprocentowanie[2], ic = 12, pf = 12)
rata <- round(rata[2], digits = 2)

# obliczanie raty kapitalowej przy zalozeniu ze przez caly okres obowiazuje oprocentowanie docelowe
rata_odsetkowa_doc <- round(kapital * sum(dni_oprocentowania) / 365 * oprocentowanie[2], digits = 2)
rata_kapitalowa <- rata - rata_odsetkowa_doc

# oblicanie faktycznej raty odsetkowej
rata_odsetkowa <- kapital * dni_oprocentowania / 365 * oprocentowanie
rata_odsetkowa <- round(sum(rata_odsetkowa), digits = 2)

cat("rata_kapitalowa[175] = ", rata_kapitalowa, "\n")
cat("rata_odsetkowa[175] = ", rata_odsetkowa, "\n")
cat("rata[175] = ", rata_kapitalowa + rata_odsetkowa, "\n")
cat("rata[176+] = ", rata, "\n")
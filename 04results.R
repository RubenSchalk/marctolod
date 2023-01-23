### summaries FAST reconciliation

library(data.table)
library(reshape)


x <- fread("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections_vs_FAST_merged.csv")

setDT(x)
test <- x[, .N, list(oclc_no, first_year_special, match_eval)]

wide <- reshape(test, timevar = "match_eval", idvar = "oclc_no", direction = "wide")

setDT(wide)

wide[ , year := first_year_special.good,]
wide[ , year2 := first_year_special.NA,]


wide[nchar(year) == 4, century := paste0(substr(year, 1,2), "00")]
wide[nchar(year2) == 4, century2 := paste0(substr(year2, 1,2), "00")]
wide[is.na(century), century := century2]
wide$century2 <- NULL
wide$century <- as.numeric(wide$century)


overtime1 <- wide[!is.na(N.good), .N, by = century][order(century)]
overtime2 <- wide[!is.na(N.likely) & is.na(N.good), .N, by = century][order(century)]
all <- wide[, .N, by = century][order(century)]
all <- all[-c(11), ]
setnames(overtime1, "N", "N_good")
setnames(overtime2, "N", "N_likely")
overtime <- merge(overtime1,overtime2, by = "century")

overtime <- merge(all, overtime, by = "century")
overtime[, match_rate_good := N_good / N * 100 ]
overtime[, match_rate_likely := N_likely / N * 100 ]
overtime[, match_rate_overall := (N_good + N_likely)/ N *100 ]


fwrite(overtime, "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\match_rate_centuries_items.csv")

#################### authors


x <- fread("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections_vs_FAST_merged.csv")

setDT(x)
test <- x[, .N, list(name_st_special, first_year_special, match_eval)]

wide <- reshape(test, timevar = "match_eval", idvar = "name_st_special", direction = "wide")

setDT(wide)

wide[ , year := first_year_special.good,]
wide[ , year2 := first_year_special.NA,]


wide[nchar(year) == 4, century := paste0(substr(year, 1,2), "00")]
wide[nchar(year2) == 4, century2 := paste0(substr(year2, 1,2), "00")]
wide[is.na(century), century := century2]
wide$century2 <- NULL
wide$century <- as.numeric(wide$century)


overtime1 <- wide[!is.na(N.good), .N, by = century][order(century)]
overtime2 <- wide[!is.na(N.likely) & is.na(N.good), .N, by = century][order(century)]
all <- wide[, .N, by = century][order(century)]
all <- all[-c(11), ]
setnames(overtime1, "N", "N_good")
setnames(overtime2, "N", "N_likely")
overtime <- merge(overtime1,overtime2, by = "century")

overtime <- merge(all, overtime, by = "century")
overtime[, match_rate_good := N_good / N * 100 ]
overtime[, match_rate_likely := N_likely / N * 100 ]
overtime[, match_rate_overall := (N_good + N_likely)/ N *100 ]


fwrite(overtime, "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\match_rate_centuries_authors.csv")



## script to transform MARC csv file into workable csv for LOD conversion

# step 1: load csv file as converted with Marcedit

library(data.table)
library(stringr)
library(dplyr)
library(splitstackshape)
library(janitor)
library(stringi)
library(naniar)
library(purrr)
library(readr)
library(stringdist)
library(lubridate)

x <- fread("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\FAST_Personal\\FASTPersonal_all_utf8.txt",
           encoding="UTF-8")

# replace '$' in variables with '_'

names(x) <- gsub(x = names(x), pattern = "\\$", replacement = "_") 

# add string to colnames to avoid breaking over only-number variables

colnames(x) <- paste("marc", colnames(x), sep = "_")

# select columns to use for reconciling (only 700 because 100 is always in 700)

fast700 <- x[, c(167,168,169,170,171,172,173,174,9)] # select all marc_700 plus fast and other links (column 9 and 169)

# split multiple  values in column (delimiter "%") into multiple columns

fast700 <- cSplit(fast700, colnames(fast700), sep='%') # split columns where % appears into  max times % appears

fast700 <- fast700 %>% remove_empty("cols") # remove empty columns (cSplit takes max % for ALL columns)

fast700$marc_700_c_02 <- NULL

fast700[grepl("viaf.org", marc_700_0_03), viaflink := marc_700_0_03]
fast700[grepl("viaf.org", marc_700_0_04), viaflink := marc_700_0_04]
fast700[grepl("viaf.org", marc_700_0_05), viaflink := marc_700_0_05]
fast700[grepl("viaf.org", marc_700_0_06), viaflink := marc_700_0_06]
fast700[grepl("viaf.org", marc_700_0_07), viaflink := marc_700_0_07]
fast700[grepl("viaf.org", marc_700_0_08), viaflink := marc_700_0_08]
fast700[grepl("viaf.org", marc_700_0_09), viaflink := marc_700_0_09]
fast700[grepl("viaf.org", marc_700_0_10), viaflink := marc_700_0_10]
fast700[grepl("viaf.org", marc_700_0_11), viaflink := marc_700_0_11]
fast700[grepl("viaf.org", marc_700_0_12), viaflink := marc_700_0_12]
fast700[grepl("viaf.org", marc_700_0_13), viaflink := marc_700_0_13]
fast700[grepl("viaf.org", marc_700_0_14), viaflink := marc_700_0_14]
fast700[grepl("viaf.org", marc_700_0_15), viaflink := marc_700_0_15]
fast700[grepl("viaf.org", marc_700_0_16), viaflink := marc_700_0_16]
fast700[grepl("viaf.org", marc_700_0_17), viaflink := marc_700_0_17]
fast700[grepl("viaf.org", marc_700_0_18), viaflink := marc_700_0_18]


fast700[grepl("wikipedia.org", marc_700_0_02), wikilink := marc_700_0_02]
fast700[grepl("wikipedia.org", marc_700_0_03), wikilink := marc_700_0_03]
fast700[grepl("wikipedia.org", marc_700_0_04), wikilink := marc_700_0_04]
fast700[grepl("wikipedia.org", marc_700_0_05), wikilink := marc_700_0_05]
fast700[grepl("wikipedia.org", marc_700_0_06), wikilink := marc_700_0_06]
fast700[grepl("wikipedia.org", marc_700_0_07), wikilink := marc_700_0_07]
fast700[grepl("wikipedia.org", marc_700_0_08), wikilink := marc_700_0_08]
fast700[grepl("wikipedia.org", marc_700_0_09), wikilink := marc_700_0_09]
fast700[grepl("wikipedia.org", marc_700_0_10), wikilink := marc_700_0_10]
fast700[grepl("wikipedia.org", marc_700_0_11), wikilink := marc_700_0_11]
fast700[grepl("wikipedia.org", marc_700_0_12), wikilink := marc_700_0_12]
fast700[grepl("wikipedia.org", marc_700_0_13), wikilink := marc_700_0_13]
fast700[grepl("wikipedia.org", marc_700_0_14), wikilink := marc_700_0_14]
fast700[grepl("wikipedia.org", marc_700_0_15), wikilink := marc_700_0_15]
fast700[grepl("wikipedia.org", marc_700_0_16), wikilink := marc_700_0_16]
fast700[grepl("wikipedia.org", marc_700_0_17), wikilink := marc_700_0_17]
fast700[grepl("wikipedia.org", marc_700_0_18), wikilink := marc_700_0_18]

setnames(fast700, "marc_024_a_01", "fastlink")

fast700_2 <- fast700[, c(1,6,7,8,9,36,37,38,40,41)]


# alles per naam onder elkaar

fast1 <- fast700_2[, list(fastlink, marc_700_a_01, marc_700_d_01, viaflink, wikilink)]
fast2 <- fast700_2[, list(fastlink, marc_700_a_02, marc_700_d_02, viaflink, wikilink)]
fast3 <- fast700_2[, list(fastlink, marc_700_a_03, marc_700_d_03, viaflink, wikilink)]

setnames(fast1, "marc_700_a_01", "marc_700_a")
setnames(fast2, "marc_700_a_02", "marc_700_a")
setnames(fast3, "marc_700_a_03", "marc_700_a")

setnames(fast1, "marc_700_d_01", "marc_700_d")
setnames(fast2, "marc_700_d_02", "marc_700_d")
setnames(fast3, "marc_700_d_03", "marc_700_d")

special <- rbind(fast1, fast2, fast3)
special <- special[order(viaflink)]

# namen opschonen (spaties, punctionering etc)

# save

write_excel_csv(special, "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\FAST_Personal\\FAST_clean.csv")

##### try to get all spelling variants of viaf links (too large, will not work) ###############################

library(viafr)

special <- read_csv("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\FAST_Personal\\FAST_clean.csv")

# make viaf id variable

special[, viaf_id := sub(".*/", "", special$viaflink)]

# sample to test

fast_dedup <- special[-which(duplicated(viaf_id)), ]
fast2 <- fast_dedup[1:10000]
result_get <- viaf_get(paste(fast2$viaf_id))


########### try to standardize SpecialCollections against FAST_clean ######################

### attempt 1: full strings standardized

fast <- read_csv("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\FAST_Personal\\FAST_clean.csv")

special <- read_csv("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections_allnames_alldates.csv")

setDT(special)
special <- special[!is.na(marc_700_a),]

# remove special characters, whitespace, capitals, numbers, and punctuation

setDT(fast)
fast[, name_st := stri_trans_general(name, "Latin-ASCII")]
fast[, name_st := tolower(name_st)]
fast[, name_st := gsub('[[:punct:] ]+', "", name_st)]
fast[, name_st := gsub('[[:digit:]]+', '', name_st)]

setDT(special)
special[, name_st := stri_trans_general(marc_700_a, "Latin-ASCII")]
special[, name_st := tolower(name_st)]
special[, name_st := gsub('[[:punct:] ]+', "", name_st)]
special[, name_st := gsub('[[:digit:]]+', '', name_st)]

# remove empty name_st due to "?"

special <- special[name_st != "",]

# how many names are there to be matched?

specialnames <- special[, .N, by = name_st] # 9,220

# check direct matches between the two files

directhit <- specialnames[name_st %in% special$name_st ]
setnames(directhit, "N", "n_specialnames")

# direct match rates without post-correction (unique names, all names): 

nrow(directhit) / nrow(specialnames) * 100

sum(directhit$n_specialnames) / nrow(special) * 100

### attempt 2: approximate string matching

fastnames <- special[!is.na(viaflink), .N, list(name_st, viaflink)]

fuzzy.matcher = function(a,b) {
  dists<- stringdistmatrix(a,b, useBytes = TRUE) # calculate the distance matrix.
  simi <- -dists     # converts it to a similarity matrix
  bestbyindex <- max.col(simi)  
  matches     <- cbind( a, b[bestbyindex], apply(simi,1,max) )
  return(as.data.frame(matches))
  }

# matched_names <- fuzzy.matcher(specialnames$name_st[1:500],fastnames$name_st ) # 500 rows max to load matrix in memory

# 500 works, so split specialnames

names1 <- specialnames[1:500,]
names2 <- specialnames[501:1000,]
names3 <- specialnames[1001:1500,]
names4 <- specialnames[1501:2000,]
names5 <- specialnames[2001:2500,]
names6 <- specialnames[2501:3000,]
names7 <- specialnames[3001:3500,]
names8 <- specialnames[4001:4500,]
names9 <- specialnames[4501:5000,]
names10 <- specialnames[5001:5500,]
names11 <- specialnames[5501:6000,]
names12 <- specialnames[6001:6500,]
names13 <- specialnames[6501:7000,]
names14 <- specialnames[7001:7500,]
names15 <- specialnames[7501:8000,]
names16 <- specialnames[8001:8500,]
names17 <- specialnames[8501:9000,]
names18 <- specialnames[9001:9221,]
names19 <- specialnames[3501:4000,]

gc()

matched_names1 <- fuzzy.matcher(names1$name_st,fastnames$name_st )
matched_names2 <- fuzzy.matcher(names2$name_st,fastnames$name_st )
matched_names3 <- fuzzy.matcher(names3$name_st,fastnames$name_st )
matched_names4 <- fuzzy.matcher(names4$name_st,fastnames$name_st )
matched_names5 <- fuzzy.matcher(names5$name_st,fastnames$name_st )
matched_names6 <- fuzzy.matcher(names6$name_st,fastnames$name_st )
matched_names7 <- fuzzy.matcher(names7$name_st,fastnames$name_st )
matched_names8 <- fuzzy.matcher(names8$name_st,fastnames$name_st )
matched_names9 <- fuzzy.matcher(names9$name_st,fastnames$name_st )
matched_names10 <- fuzzy.matcher(names10$name_st,fastnames$name_st )
matched_names11 <- fuzzy.matcher(names11$name_st,fastnames$name_st )
matched_names12 <- fuzzy.matcher(names12$name_st,fastnames$name_st )
matched_names13 <- fuzzy.matcher(names13$name_st,fastnames$name_st )
matched_names14 <- fuzzy.matcher(names14$name_st,fastnames$name_st )
matched_names15 <- fuzzy.matcher(names15$name_st,fastnames$name_st )
matched_names16 <- fuzzy.matcher(names16$name_st,fastnames$name_st )
matched_names17 <- fuzzy.matcher(names17$name_st,fastnames$name_st )
matched_names18 <- fuzzy.matcher(names18$name_st,fastnames$name_st )
matched_names19 <- fuzzy.matcher(names19$name_st,fastnames$name_st )

matched_names <- rbind(matched_names1,matched_names2, matched_names3, matched_names4, 
                       matched_names5,matched_names6, matched_names7, 
                       matched_names8, matched_names9,
                       matched_names10, matched_names11, matched_names12,
                       matched_names13, matched_names14, matched_names15,
                       matched_names16, matched_names17, matched_names18,
                       matched_names19)


rm(matched_names1,matched_names2, matched_names3, matched_names4, matched_names5,
                    matched_names6, matched_names7, matched_names8, matched_names9,
                    matched_names10, matched_names11, matched_names12,
                    matched_names13, matched_names14, matched_names15,
                    matched_names16, matched_names17, matched_names18,
                      matched_names19)

rm(names1, names2, names3, names4, names5, names6, names7,
   names8, names9, names10, names11, names12, names13,
   names14, names15, names16, names17, names18, names19)

gc()

colnames(matched_names)
setnames(matched_names, "a", "name_st_special")
setnames(matched_names, "V2", "name_st_fast")
setnames(matched_names, "V3", "distance")

setDT(matched_names)
matched_names <- matched_names[order(distance),]

# match rate very rough

matched_names[distance == "0", .N] # 1,325
matched_names[distance == "-1", .N] # 620
matched_names[distance == "-2", .N] # 1,114
matched_names[distance == "-3", .N] # 1, 348

#save

write_excel_csv(matched_names, "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections_vs_FAST.csv")

# overview of matches

matched_names$distance <- as.numeric(matched_names$distance)
matched_names[, .N, list(distance)][order(-distance)]

# next: merge back in + post-check with dates where possible

special <- special[name != "",]
setnames(special, "name_st", "name_st_special")
special_merged <- merge(special, matched_names, by = "name_st_special", all = T)
special_merged <- special_merged[name_st_special != "",]

setDT(special)
special <- special[, .N, list(name_st, marc_700_a, marc_700_d, viaflink, wikilink)]
special <- special[!is.na(viaflink),]
setnames(special, "name_st", "name_st_fast")

# merge all back together = more rows because name_st_fast#oclc_no = often more than one occurrence 

all_merged <- merge(special_merged, special, by = "name_st_fast", all.x = T, all.y = F)
all_merged <- all_merged[!duplicated(all_merged), ]

# clean dates and use to post-check

dates <- data.frame(str_extract_all(all_merged$date,"\\(?[0-9]+", simplify = T))
setnames(dates, "X1", "first_year_special")
setnames(dates, "X2", "last_year_special")
all_merged <- cbind(all_merged, dates[,c("first_year_special", "last_year_special")])

dates2 <- data.frame(str_extract_all(all_merged$marc_700_d,"\\(?[0-9]+", simplify = T))
setnames(dates2, "X1", "first_year_fast")
setnames(dates2, "X2", "last_year_fast")
all_merged <- cbind(all_merged, dates2[,c("first_year_fast", "last_year_fast")])

# empty values to NA

na_strings <- c("#", "##", "###", "####","#####","geen", "", " ")
all_merged <- all_merged %>% replace_with_na_all(condition = ~.x %in% na_strings)

###### evaluating matches ##############

# 1: exact date matches in first and last

all_merged$distance <- as.numeric(all_merged$distance)

all_merged[first_year_special == first_year_fast & last_year_special == last_year_fast, match_eval := "good"]

all_merged[match_eval == "good", .N] # 1,304

# 2: exact date match in first or last date

all_merged[first_year_special == first_year_fast & is.na(match_eval),match_eval := "good" ]
all_merged[last_year_special == last_year_fast & is.na(match_eval), match_eval := "good"]

all_merged[match_eval == "good", .N] # 1,518

# 3: string distance > -3 but only for names of at least 7 characters within 10 years distance 

all_merged[!is.na(first_year_special) & !is.na(first_year_fast), interval_first := first_year_special - first_year_fast]
all_merged[!is.na(last_year_special) & !is.na(last_year_fast), interval_last := last_year_special - last_year_fast]

all_merged[, interval_first := gsub("-", "", interval_first)]
all_merged[, interval_last := gsub("-", "", interval_last)]
all_merged$interval_first <- as.numeric(all_merged$interval_first)
all_merged$interval_last <- as.numeric(all_merged$interval_last)

all_merged[distance > -3 & nchar(name_st_special) >7 & is.na(match_eval) 
                 & interval_first < 10 |distance == 0 & nchar(name_st_special) >7 & is.na(match_eval) & interval_last < 10  ,
           match_eval := "likely" ]

# 4: string distance 0 but only for names of at least 7 characters (to exclude "bb" etc. ) with no date interval

all_merged[distance > -3 & nchar(name_st_special) >9 & is.na(match_eval) &
          is.na(interval_first) |
             distance == 0 & nchar(name_st_special) >9 & is.na(match_eval) &
          is.na(interval_last), match_eval := "likely"]

# 5: remove likely matches where either interval is too high

all_merged[match_eval == "likely" & interval_first > 10, match_eval := NA]
all_merged[match_eval == "likely" & interval_last > 10, match_eval := NA]

# TODO: 133 authors with more than one VIAF, possibly wrong. Assign into match_eval?

all_merged[!is.na(match_eval) & !duplicated(viaflink), .N, list(name)][N > 1]

# evaluate n and share matches

all_merged[match_eval == "good", .N, by = name] # 892
all_merged[match_eval == "likely", .N, by = name] # 1,541

count(all_merged[!is.na(match_eval), .N, by = name ]) / count(all_merged[, .N, list(name)]) * 100 # 20.1 % of authors
count(all_merged[!is.na(match_eval), .N, list(oclc_no)]) / count(all_merged[, .N, list(oclc_no)]) * 100 # 29.6 % of items

nonmatch <- all_merged[is.na(match_eval) & distance > -11,][order(-distance)]

# save final 

write_excel_csv(all_merged, "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections_vs_FAST_merged.csv")














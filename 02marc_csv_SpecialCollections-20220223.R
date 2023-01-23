
## script to transform MARC csv file into workable csv for LOD conversion

# step 1: load csv file as converted with Marcedit

library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(splitstackshape)
library(janitor)
library(stringi)
library(naniar)
library(readr)

x <- fread("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections-20220223.txt", 
           encoding= "UTF-8")

# split multiple  values in column (delimiter "%") into multiple columns

x$`000` <- NULL # duplicated column 

x <- cSplit(x, colnames(x), sep='%') # split columns where % appears into  max times % appears

x <- x %>% remove_empty("cols") # remove empty columns (cSplit takes max % for ALL columns)

# empty values to NA

na_strings <- c("#", "##", "###", "####","#####","geen", "", " ")
x <- x %>% replace_with_na_all(condition = ~.x %in% na_strings)


# remove trailing dots and commas

x <- as.data.frame(sapply(x, function(x) gsub("\\.$", "", x)))

x <- as.data.frame(sapply(x, function(x) gsub("\\,$", "", x)))

# replace '$' in variables with '_'

names(x) <- gsub(x = names(x), pattern = "\\$", replacement = "_") 

# add string to colnames to avoid breaking over only-number variables

colnames(x) <- paste("marc", colnames(x), sep = "_")

# remove trailing "\" from variable 

x$marc_000_01 <- stri_sub(x$marc_000_01, 1, -2)

# keep clean OCLC number and change name (otherwise jinja won't work in COW)

x$marc_035_a_01 <- stri_sub(x$marc_035_a_01, 8, -1)

setnames(x, "marc_035_a_01", "oclc_no")


# generate oclc urls (impossible in COW because read as string in jinja) (not done yet)

#x$oclc_url <- paste0("http://www.worldcat.org/oclc/", x$oclc_no)

#x$uu_url <- paste0("https://utrechtuniversity.on.worldcat.org/search?queryString=no:", x$oclc_no)


# save

write_excel_csv(x, "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections-20220223_clean_v2.csv")

#### TO DO:
# remove invalid urls to fast from marc_600_0_1 and marc_700_0_1 (e.g.: '(NL-LeOCL)069857326')
# else LOD conversion in COW will break because not a valid URL


###############################################################################################################################

x <- fread("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections-20220223_clean_v2.csv", 
           encoding= "UTF-8")

# subsetting the csv to get all names, plus oclc number

ubu700a <- x[, grep("marc_700_a", names(x)) , with = FALSE] # get all columns with "marc_700_a"

ubu700d <- x[, grep("marc_700_d", names(x)) , with = FALSE] # get all columns with "marc_700_d"

oclc_no <- x[, grep("oclc_no", names(x)) , with = FALSE]

test_1 <- cbind(oclc_no, ubu700a)

long_names <- melt(setDT(test_1), id.vars = c("oclc_no"), variable.name = "marc_700_a")

long_dates <- melt(setDT(test_2), id.vars = c("oclc_no"), variable.name = "marc_700_d")

long_names <- long_names[!is.na(value),]
long_dates <- long_dates[!is.na(value),]

long_dates[, marc_700_d := gsub("d", "a", marc_700_d),]

setnames(long_names, "value", "name")
setnames(long_dates, "value", "date")

test_4 <- merge(long_names,long_dates,by.x=c('oclc_no','marc_700_a'),by.y=c('oclc_no','marc_700_d'),all.x=TRUE, all.y = F)

test_5 <- test_4[!duplicated(test_4),]

# do the same with marc_100

# subsetting the csv to get all names, plus oclc number

ubu100a <- x[, grep("marc_100_a", names(x)) , with = FALSE] # get all columns with "marc_100_a"

ubu100d <- x[, grep("marc_100_d", names(x)) , with = FALSE] # get all columns with "marc_100_d"

test_6 <- cbind(oclc_no, ubu100a)
test_7 <- cbind(oclc_no, ubu100d)

long_names100 <- melt(setDT(test_6), id.vars = c("oclc_no"), variable.name = "marc_100_a")

long_dates100 <- melt(setDT(test_7), id.vars = c("oclc_no"), variable.name = "marc_100_d")

long_dates100[, marc_100_d := gsub("d", "a", marc_100_d),]

setnames(long_names100, "value", "name")
setnames(long_dates100, "value", "date")


long_names100 <- long_names100[!is.na(name),]
long_dates100 <- long_dates100[!is.na(date),]
long_names100 <- long_names100[!duplicated(long_names100),]
long_dates100 <- long_dates100[!duplicated(long_dates100),]

test_8 <- merge(long_names100,long_dates100,by.x=c('oclc_no'),by.y=c('oclc_no'),all.x=TRUE, all.y = TRUE)

test_9$marc_100_d <- NULL

setnames(test_5, "marc_700_a", "marc_field")
setnames(test_9, "marc_100_a", "marc_field")

colnames(test_5)
colnames(test_9)


all_names <- rbind(test_9,test_5)

all_names <- all_names[order(oclc_no),]

write_excel_csv(all_names, "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\SpecialCollections_allnames_alldates.csv")

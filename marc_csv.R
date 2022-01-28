
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

setwd("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Linked Data experiments")

someren <- fread("someren_csv_jan2022.txt", encoding = "UTF-8") 

# step 2: seperate cells with multiple values (indicated by semi-colon)

vars <- colnames(someren)[grepl("~", someren)] # find colnames to split

# columns to split: "600$a" "600$c" "600$d" "600$0" "600$1" "610$a" "700$a" "700$c" "700$d" "700$e" "700$0"

someren <- cSplit(someren, colnames(someren), sep='~') # split columns where ~ appears into  max times ~ appears

someren <- someren %>% remove_empty("cols") # remove empty columns (cSplit takes max ~ for ALL columns)

# remove trailing dots and commas

someren <- as.data.frame(sapply(someren, function(x) gsub("\\.$", "", x)))

someren <- as.data.frame(sapply(someren, function(x) gsub("\\,$", "", x)))

# encode to UTF8 to ensure that conversion doesn't break

for (col in colnames(someren)) {
  names(someren) <- enc2utf8(names(someren)) # Column names need to be encoded too
  someren[[col]] <- as.character(someren[[col]]) # Allows for enc2utf8() and Encoding()
  someren[[col]] <- enc2utf8(someren[[col]]) 
  Encoding(someren[[col]]) <- "unknown"
}

# empty values to NA

na_strings <- c("#", "##", "###", "####","#####","geen", "", " ")
someren <- someren %>% replace_with_na_all(condition = ~.x %in% na_strings)

# remove trailing "\" from variable 

someren$`001_1` <- stri_sub(someren$`001_1`, 1, -2)

# keep clean OCLC number and change name (otherwise jinja won't work in COW)

someren$`035` <- stri_sub(someren$`035$a`, 8, -1)

setnames(someren, "035$a", "oclc_no")

# generate oclc urls (impossible in COW because read as string in jinja)

someren$oclc_url <- paste0("http://www.worldcat.org/oclc/", someren$oclc_no)

someren$uu_url <- paste0("https://utrechtuniversity.on.worldcat.org/search?queryString=no:", someren$oclc_no)

# replace '$' in variables with '_'

names(someren) <- gsub(x = names(someren), pattern = "\\$", replacement = "_") 

# add string to colnames to avoid breaking over numbers in COW

colnames(someren) <- paste("marc", colnames(someren), sep = "_")

# save

fwrite(someren, "someren_csv_jan2022_clean.csv", bom = T)

#### TO DO:
# remove invalid urls to fast from marc_600_0_1 and marc_700_0_1 (e.g.: '(NL-LeOCL)069857326')
# else COW will break because not a valid URL
# not yet in metadata, see 'blocks to add.csv' for blocks

###############################################################################################################################
# attempt 1 to manually code this.. (deprecated)

y <- max(stringr::str_count(someren$`600$a`, "~")) + 1
someren[, paste0(vars[1],"_", 1:y) := tstrsplit(`600$a`, "~")]

y <- max(stringr::str_count(someren$`600$c`, "~")) + 1
someren[, paste0("600$c_", 1:y) := tstrsplit(`600$c`, "~")]

y <- max(stringr::str_count(someren$`600$d`, "~")) + 1
someren[, paste0("600$d_", 1:y) := tstrsplit(`600$d`, "~")]

y <- max(stringr::str_count(someren$`600$0`, "~")) + 1
someren[, paste0("600$0_", 1:y) := tstrsplit(`600$0`, "~")]

y <- max(stringr::str_count(someren$`600$1`, "~")) + 1
someren[, paste0("600$1_", 1:y) := tstrsplit(`600$1`, "~")]

y <- max(stringr::str_count(someren$`610$a`, "~")) + 1
someren[, paste0("610$a_", 1:y) := tstrsplit(`610$a`, "~")]

y <- max(stringr::str_count(someren$`700$a`, "~")) + 1
someren[, paste0("700$a_", 1:y) := tstrsplit(`700$a`, "~")]

y <- max(stringr::str_count(someren$`700$c`, "~")) + 1
someren[, paste0("700$c_", 1:y) := tstrsplit(`700$c`, "~")]

y <- max(stringr::str_count(someren$`700$d`, "~")) + 1
someren[, paste0("700$d_", 1:y) := tstrsplit(`700$d`, "~")]

y <- max(stringr::str_count(someren$`700$e`, "~")) + 1
someren[, paste0("700$e_", 1:y) := tstrsplit(`700$e`, "~")]

y <- max(stringr::str_count(someren$`700$0`, "~")) + 1
someren[, paste0("700$0_", 1:y) := tstrsplit(`700$0`, "~")]






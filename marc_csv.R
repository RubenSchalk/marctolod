
## testing script to transform MARC csv file into workable csv for LOD conversion

# step 1: loas csv file as converted with Marcedit

library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(splitstackshape)
library(janitor)

setwd("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Linked Data experiments")

someren <- fread("someren_csv_jan2022.txt") 

# step 2: seperate cells with multiple values (indicated by semi-colon)

vars <- colnames(someren)[grepl("~", someren)] # find colnames to split

# columns to split: "600$a" "600$c" "600$d" "600$0" "600$1" "610$a" "700$a" "700$c" "700$d" "700$e" "700$0"

someren <- cSplit(someren, colnames(someren), sep='~') # split columns where ~ appears into  max times ~ appears

someren <- someren %>% remove_empty("cols") # remove empty columns (cSplit takes max ~ for ALL columns)

# remove trailing dots and commas

someren <- as.data.frame(sapply(someren, function(x) gsub("\\.$", "", x)))

someren <- as.data.frame(sapply(someren, function(x) gsub("\\,$", "", x)))

# save

fwrite(someren, "someren_csv_jan2022_clean.csv")






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






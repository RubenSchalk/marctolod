
### get comprehensive txt file as input for MARC to csv step in Marcedit (including ALL MARC fields) ###

# Step 1 outside R: open MARC Mnemonic "SpecialCollections-20220223".
# or Step 1 outside R: open MARC Mnemonic "FASTPersonal_all".
# In Marcedit > Reports > Field Count > count subfield > save as txt tab delimited

# open this txt file in R:

library(data.table)
library(zoo)

marcfield <- fread("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\marcedit_field_count.txt")
#marcfield <- fread("C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\FAST_Personal\\marcedit_field_count_fast.txt")

# replace missing field numbers with ones above (MarcEdit gives an odd txt...)

marcfield[, field_filled := na.locf(Field)]
marcfield <- marcfield[, c(1, 5, 2,3,4)]

# merge field_filled with subfield as LDR

marcfield[, LDR := paste0(field_filled,Subfield)]
marcfield[Field <8 , LDR := NA] # only keep relevant (sub)fields

# past "00" to LDR 1-8 and "0" to 10-99 fields

marcfield[Field <= 8, LDR := paste0("00", Field)]
marcfield[Subfield == "", Subfield := NA]
marcfield[field_filled < 100 & field_filled > 8 & !is.na(Subfield) , LDR := paste0("0", field_filled, Subfield)]
marcfield <- marcfield[Field < 9 | !is.na(Subfield),  ]

# save only LDR variable as txt file, to use for MARC to csv in marcedit

fwrite(marcfield[,c("LDR")], "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\marcedit_export_settings_SpecialCollections.txt")
#fwrite(marcfield[,c("LDR")], "C:\\Users\\Schal107\\Documents\\UBU\\Team DH\\Metadata reconciliation\\FAST_Personal\\marcedit_export_settings_fast.txt")


### VIAF dataset from VIAF API

install.packages("viafr")
library(data.table)
library(viafr)
library(dplyr)
library(purrr)

# call VIAF API

result_get <- viaf_get("15873")
result_suggest <- viaf_suggest("a")

text <- dplyr::pull(result_get, text) %>%  purrr::pluck(1)

setDT(text)

text[text$a %in% result_get$name_type, viafID := result_get$viaf_id]

result_get[, list("name_type")]

setDT(test)

test <- cbind.data.frame(result_get$viaf_id, text$a, text$b, text$c, text$d, text$e, text$f, text$g, text$q, text$`4`, text$`5`,
                         text$`7`, text$`8`, text$`9`)

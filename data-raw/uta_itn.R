# Would it be better to read this directly from Box?
uga_itn <- data.table::fread("data-raw/uga_itn_fmt.csv")
usethis::use_data(uga_itn, overwrite=TRUE)

# Would it be better to read this directly from Box?
uga_irs <- data.table::fread("data-raw/uga_irs_fmt.csv")
usethis::use_data(uga_irs, overwrite=TRUE)

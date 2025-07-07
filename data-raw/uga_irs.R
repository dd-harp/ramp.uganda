# Would it be better to read this directly from Box?
uga_irs <- data.table::fread("data-raw/uga_irs_fmt.csv")
uga_irs$round <- uga_irs$formulation
ix = which(uga_irs$round == "Actellic")
uga_irs$round[ix] = "actellic"
ix = which(uga_irs$round == "Bendiocarb")
uga_irs$round[ix] = "bendiocarb"
ix = which(uga_irs$round == "Fludora Fusion")
uga_irs$round[ix] = "fludora_fusion"
ix = which(uga_irs$round == "Sumishield")
uga_irs$round[ix] = "sumishield"
usethis::use_data(uga_irs, overwrite=TRUE)

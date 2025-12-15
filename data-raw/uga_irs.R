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
ix = which(uga_irs$round == "Alpha-cypermethrin")
uga_irs$round[ix] = "pyrethroid"
ix = which(uga_irs$round == "Lambda-cyhalothrin")
uga_irs$round[ix] = "pyrethroid"
ix = which(uga_irs$round == "Prallethrin")
uga_irs$round[ix] = "pyrethroid"
ix = which(uga_irs$round == "DDT")
uga_irs$round[ix] = "actellic"
start = as.Date("2015-01-01")
start_date <- as.Date(uga_irs$spray_start)
uga_irs$jdate = julian(start_date, origin = start)
usethis::use_data(uga_irs, overwrite=TRUE)


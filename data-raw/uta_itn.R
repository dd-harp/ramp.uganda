# Would it be better to read this directly from Box?
uga_itn <- data.table::fread("data-raw/uga_itn_fmt.csv")

ix <- which(uga_itn$district_name == "Arua District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Arua City"
new$district_name = "Arua City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Jinja District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Jinja City"
new$district_name = "Jinja City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Gulu District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Gulu City"
new$district_name = "Gulu City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Hoima District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Hoima City"
new$district_name = "Hoima City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Lira District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Lira City"
new$district_name = "Lira City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Masaka District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Masaka City"
new$district_name = "Masaka City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Mbale District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Mbale City"
new$district_name = "Mbale City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Mbarara District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Mbarara City"
new$district_name = "Mbarara City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Soroti District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Soroti City"
new$district_name = "Soroti City"
uga_itn = rbind(uga_itn, new)

ix <- which(uga_itn$district_name == "Kabarole District")
uga_itn[ix,]$distribution_date
new <- uga_itn[ix[-1],]
new$district = "Fort Portal City"
new$district_name = "Fort Portal City"
uga_itn = rbind(uga_itn, new)



usethis::use_data(uga_itn, overwrite=TRUE)

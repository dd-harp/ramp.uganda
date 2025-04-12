#district_adjacency <- read.csv("data-raw/Adjacency_district.csv", row.names=1)
#colnames(district_adjacency) <- rownames(district_adjacency)
#write.csv(district_adjacency, "data-raw/District_Adjacency.csv")
district_adjacency <- read.csv("data-raw/District_Adjacency.csv", row.names=1)
usethis::use_data(district_adjacency, overwrite=TRUE)


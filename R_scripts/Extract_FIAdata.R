library(DBI)
library(RSQLite)

path_db <- "~/Downloads/"
path_out <- "~/Downloads/"
path_db

MN_con <- dbConnect(RSQLite::SQLite(),
paste0(path_db, "FIADB_MN.db"))

db_table_names <- dbListTables(MN_con)
db_table_names
db_table_names <- c("COND")
db_table_names

lapply(db_table_names, function(x) {
write.csv(dbReadTable(x, conn = MN_con),
file = paste0(path_db, "MN_", x, ".csv"))
})


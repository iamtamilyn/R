# database test?

library(odbc)
con <- dbConnect(odbc(),
     Driver = "SQLServer",
     Server = "TPECK\\SQLEXPRESS",
     Database = "WATTapplication",
     # UID = "myuser",
     # PWD = rstudioapi::askForPassword("Database password")
     Port = 1433)

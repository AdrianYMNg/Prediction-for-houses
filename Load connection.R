#install.packages("RMySQL")
library("RMySQL")

con <- dbConnect(MySQL(),
                 user="root", password="root",
                 dbname="iowa", host="127.0.0.1")

rs <- dbSendQuery(con, "SELECT * FROM iowa.train;")
database <- fetch(rs , n = -1)

database[database=="0"] <- 1

dbClearResult(rs)
dbDisconnect(con)
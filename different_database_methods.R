library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)

#connect to a database
con <- DBI::dbConnect(RSQLite::SQLite(), # The database back end / SQL driver
                      dbname = ":memory:")

#con <- dbConnect(odbc::odbc(), "Oracle DB")

#Check and see what tables are in the db

dbListTables(con)

#to check and see some of a table
dbReadTable(con, "tablename")



#More quickly we can do this:

# Get table names
table_names <- dbListTables(con)

# Import all tables
tables <- lapply( table_names,dbReadTable, conn = con)

# Print out tables
tables

#QUERY USING DBI
dbGetQuery(con,'
  select "month_idx", "year", "month",
  sum(case when "term_deposit" = \'yes\' then 1.0 else 0.0 end) as subscribe,
  count(*) as total
  from "bank"
  group by "month_idx", "year", "month"
')


#2. QUERY USING DPLYR SYNTAX
q1 <- tbl(con, "bank") %>%
    group_by(month_idx, year, month) %>%
    summarise(
        subscribe = sum(ifelse(term_deposit == "yes", 1, 0)),
        total = n())
show_query(q1)





# Very end make sure you disconnnect

dbDisconnect(con)









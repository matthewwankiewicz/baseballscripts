# For MySQL, adjust according to your database
library(DBI)
library(RPostgres)
library(rvest)

# Create a new connection using RPostgres
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "mlbdata",
                 host = "localhost",
                 port = 5432,
                 user = "postgres",
                 password = "root")

### pitcher props
## strikeouts -----------------
so_props <- read_csv("mlb_pitcherprops.csv")

so_props <- so_props %>% 
  distinct(full_name, ou, .keep_all = T) %>% 
  mutate(bet_date = Sys.Date()) %>% 
  select(bet_date, full_name, "line" = value, money, ou)


## append to table
dbWriteTable(con, name = "pitcherstrikeoutprops", value = so_props, row.names = FALSE, overwrite = FALSE,
             append = TRUE)

## table for overs
dbWriteTable(con, name = "pitcheroverprops", value = so_props %>% filter(ou == "over"), row.names = F,
             overwrite = FALSE, append = TRUE)

## table for unders
dbWriteTable(con, name = "pitcherunderprops", value = so_props %>% filter(ou == "under"), row.names = F,
             overwrite = FALSE, append = TRUE)



dbGetQuery(con, 'select max(bet_date) from "pitcherstrikeoutprops";')



link <- "https://sportsbook.draftkings.com/leagues/baseball/mlb?category=pitcher-props&subcategory=strikeouts-thrown"


## earned runs -----------------
link <- "https://sportsbook.draftkings.com/leagues/baseball/mlb?category=pitcher-props&subcategory=earned-runs-allowed"
tables <- read_html(link) %>% 
  html_table()




## outs ------------------
link <- "https://sportsbook.draftkings.com/leagues/baseball/mlb?category=pitcher-props&subcategory=strikeouts-thrown"

### batter props
## hits ------------
hit_props <- read_csv("mlb_hitterprops.csv")

hit_props <- hit_props %>% 
  distinct(full_name, ou, .keep_all = T) %>% 
  mutate(bet_date = Sys.Date()) %>% 
  select(bet_date, full_name, "line" = value, money, ou)


## create table
dbWriteTable(con, name = "batterhitprops", value = hit_props, row.names = FALSE, append = TRUE)

dbGetQuery(con, 'select max(bet_date) from  "batterhitprops";')

## total bases -----------------
link <- "https://sportsbook.draftkings.com/leagues/baseball/mlb?category=batter-props&subcategory=total-bases"
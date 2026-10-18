# 1: Setup. Every other file in this folder runs this file first.
# It does 2 things:
#   1. connects to the database (same way as our other code)
#   2. makes 2 small helpers, so the other files stay short:
#        run_sql("...")   runs SQL that builds a table (shows nothing)
#        show("...")      runs a SELECT and prints the result (for checks)
# You don't need to run this file by hand.

library(DatabaseConnector)
library(keyring)

Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/wang.life/Desktop/drivers")   # change to your own drivers folder

con <- connect(createConnectionDetails(
  dbms     = "redshift",
  server   = "ohdsi-lab-redshift-cluster-prod.clsyktjhufn7.us-east-1.redshift.amazonaws.com/ohdsi_lab",
  port     = 5439,
  user     = key_get("db_username"),
  password = key_get("db_password")
))

run_sql <- function(sql) executeSql(con, sql)
show    <- function(sql) print(querySql(con, sql))

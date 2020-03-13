# Set the analysis environment --------------------------------------------
setwd("Set the working directory") 

hospital <- "Choose from 'SNUH', 'SNUBH', 'AMC'"

dbms <- "The type of DBMS running on the server"
# "oracle" for Oracle
# "postgresql" for PostgreSQL
# "redshift" for Amazon Redshift
#"sql server" for Microsoft SQL Server#
# "pdw" for Microsoft Parallel Data Warehouse (PDW)
#"netezza" for IBM Netezza
# "bigquery" for Google BigQuery
# "sqlite" for SQLite

server <- "The name of the server"
user <- "The user name used to access the server"
password <- "The password for that user"

cohort_id <- "The ID of the cohort"
result_database_schema <- ""
cdm_database_schema <- ""
voca_database_schema <- ""


# Run analysis ------------------------------------------------------------
source("r/function.R")

run_analysis(hospital = hospital,
             dbms = dbms,
             user = user,
             password = password,
             server = server,
             cohort_id = cohort_id,
             result_database_schema = result_database_schema,
             cdm_database_schema = cdm_database_schema,
             voca_database_schema = voca_database_schema)
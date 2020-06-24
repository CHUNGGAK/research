prepare_viewer <- function(output_path) {
    balance_path <- file.path(output_path, "shiny_data/balance")
    dir.create(balance_path)
    
    result <- read_csv(file.path(output_path, "result.csv"))
    write_csv(result, file.path(output_path, "shiny_data"))
    
    dBalance <- result %>%
        filter(strataFile != "" & cohortMethodDataFolder != "" ) %>% 
        distinct(strataFile, cohortMethodDataFolder)
    
    for (i in 1:nrow(dBalance)) {
        strata <- read_rds(file.path(output_path, dBalance[[i, "strataFile"]]))
        cm <- loadCohortMethodData(file.path(output_path, dBalance[[i, "cohortMethodDataFolder"]]))
        
        write_csv(balance, file.path(balance_folder,
                                     paste0("Balance_",
                                            str_extract(dTrimmedPop[[i, "strataFile"]],
                                                        "l\\d+_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_o\\d+"),
                                            ".csv")))
    }
}
path_assistant <- function(path) {
    if (!dir.exists(path)) {
        print(paste0("'", path, "'", " does not exist. Therefore, new directory is created."))
        dir.create(path, recursive = TRUE)
    } else {
        print(paste0("'", path, "'", " exists, Therefore, use an existing path."))
    }
}

remove_outlier_with_qti <- function(datum, na.rm = TRUE) {
    qti <- quantile(datum, probs = c(0.25, 0.75), na.rm = na.rm)
    a <- 1.5 * IQR(datum, na.rm = na.rm)
    output <- datum
    output[datum < (qti[1] - a)] <- NA
    output[datum > (qti[2] + a)] <- NA
    return(output)
}

print_t_test <- function(x, y) {
    output <- t.test(x, y)
    
    data.table(Method = output$method,
               t = output$statistic,
               degrees_of_freedom = output$parameter,
               p_value = output$p.value,
               alternative_hypothesis = output$alternative,
               confidence_interval_lower = output$conf.int[1],
               confidence_interval_upper = output$conf.int[2],
               mean_of_x = output$estimate[1],
               mean_of_y = output$estimate[2]) %>% 
        xtable() %>% 
        print.xtable(type = "html")
}

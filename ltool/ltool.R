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

print_t_test <- function(x, y, title, white_space = 2) {
    cat(title)
    
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
    
    cat(rep("<br>", white_space))
}

print_chisq_test <-  function(x, title, white_space = 2) {
    cat(title)
    
    output <- chisq.test(x)
    data.frame(Method = output$method,
         X_squared = output$statistic,
         degrees_of_freedom = output$parameter,
         p_value = output$p.value) %>% 
        xtable() %>% 
        print.xtable(type = "html")
    cat("<br>")
    
    cat("Observed(Expected)")
    matrix(c(paste0(output$observed[1], "(", round(output$expected[1]), ")"),
             paste0(output$observed[2], "(", round(output$expected[2]), ")"),
             paste0(output$observed[3], "(", round(output$expected[3]), ")"),
             paste0(output$observed[4], "(", round(output$expected[4]), ")")
           ), ncol = 2, dimnames = list(Sex = c("Female", "Male"), Drug = c("False", "True"))) %>% 
        xtable() %>% 
        print.xtable(type = "html")
    cat("<br>")
    
    cat("Residuals(Stdres)")
    matrix(c(paste0(output$residuals[1], "(", round(output$stdres[1]), ")"),
             paste0(output$residuals[2], "(", round(output$stdres[2]), ")"),
             paste0(output$residuals[3], "(", round(output$stdres[3]), ")"),
             paste0(output$residuals[4], "(", round(output$stdres[4]), ")")
    ), ncol = 2, dimnames = list(Sex = c("Female", "Male"), Drug = c("False", "True"))) %>% 
        xtable() %>% 
        print.xtable(type = "html")
    cat(rep("<br>", white_space))
}

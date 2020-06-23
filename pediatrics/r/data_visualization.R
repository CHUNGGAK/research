setwd("E:/Users/DLCG001/workspace/pediatrics")
output_path <- "data_visualization_v2"
dir.create(output_path)

library(tidyverse)


# Create Data Tables -------------------------------------------------------
vTime <- factor(paste0(c(0, 3, 6, seq(12, 60, 12)), "M"),
                levels = paste0(c(0, 3, 6, seq(12, 60, 12)), "M"))

d_type <- data.frame(time = rep(vTime, each = 2),
                     type = c("T1D", "T2D"),
                     overweight = c(0.079, 0.621,
                                    0.094, 0.621,
                                    0.138, 0.592,
                                    0.192, 0.711,
                                    0.244, 0.684,
                                    0.335, 0.656,
                                    0.409, 0.708,
                                    0.472, 0.706),
                     pooly_controlled = c(0.903, 0.779,
                                          0.593, 0.245,
                                          0.654, 0.236,
                                          0.745, 0.407,
                                          0.811, 0.549,
                                          0.843, 0.640,
                                          0.896, 0.608,
                                          0.896, 0.703))

d_type_2 <- data.frame(time = rep(vTime, each = 2),
                       type = c("T1D", "T2D"),
                       well_controlled = 1 - d_type$pooly_controlled)

d_sex <- data.frame(time = rep(vTime, each = 4),
                    type = c("T1D boys", "T1D girls", "T2D boys", "T2D girls"),
                    overweight = c(0.087, 0.073, 0.589, 0.655,
                                   0.087, 0.100, 0.561, 0.711,
                                   0.101, 0.168, 0.564, 0.628,
                                   0.171, 0.207, 0.638, 0.791,
                                   0.254, 0.236, 0.632, 0.732,
                                   0.260, 0.396, 0.647, 0.667,
                                   0.345, 0.463, 0.708, 0.708,
                                   0.404, 0.533, 0.750, 0.783),
                    pooly_controlled = c(0.900, 0.905, 0.809, 0.748,
                                         0.594, 0.591, 0.212, 0.284,
                                         0.623, 0.678, 0.192, 0.288,
                                         0.688, 0.785, 0.436, 0.347,
                                         0.770, 0.841, 0.517, 0.578,
                                         0.819, 0.863, 0.617, 0.660,
                                         0.872, 0.916, 0.537, 0.658,
                                         0.873, 0.904, 0.632, 0.778))

d_age <- data.frame(time = rep(vTime, each = 5),
                    type = factor(c("T1D 0-4Y", "T1D 5-9Y", "T1D 10-14T", "T2D 5-9Y", "T2D 10-14T"),
                                  levels = c("T1D 0-4Y", "T1D 5-9Y", "T1D 10-14T", "T2D 5-9Y", "T2D 10-14T")),
                    overweight = c(0.102, 0.089, 0.067, 0.667, 0.614,
                                   0.192, 0.078, 0.074, 0.625, 0.620,
                                   0.143, 0.152, 0.129, 0.529, 0.605,
                                   0.339, 0.153, 0.173, 0.667, 0.720,
                                   0.318, 0.245, 0.225, 0.786, 0.662,
                                   0.333, 0.377, 0.314, 0.750, 0.607,
                                   0.250, 0.615, 0.353, 0.667, 0.718,
                                   0.300, 0.652, 0.447, 0.800, 0.696),
                    pooly_controlled = c(0.909, 0.847, 0.927, 0.621, 0.803,
                                         0.734, 0.518, 0.598, 0.318, 0.234,
                                         0.716, 0.654, 0.639, 0.300, 0.226,
                                         0.779, 0.762, 0.726, 0.524, 0.388,
                                         0.783, 0.797, 0.826, 0.526, 0.553,
                                         0.839, 0.840, 0.846, 0.583, 0.648,
                                         0.786, 0.921, 0.914, 0.667, 0.600,
                                         0.830, 0.923, 0.901, 0.714, 0.701))


# Create Plot Function ------------------------------------------------------------
create_plot <- function(data, file_name, y) {
    if (length(unique(data$type)) == 1) {
        data %>% 
            ggplot(aes_string(x = "time", y = y)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(position = position_dodge(width = 1),
                      size = 2.5,
                      aes(label = paste0(round(data[, y], 2) * 100, "%"),
                          y = data[, y] + 0.025)) + 
            scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, 0.2),
                               labels = paste0(seq(0, 1, 0.2) * 100, "%")) +
            labs(x = NULL, y = NULL) +
            theme(legend.position = "none",
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  axis.ticks.x = element_blank())
        ggsave(paste0(file_name, "_y", y, ".png"),
               width = 24, height = 18, units = "cm")
        ggsave(paste0(file_name, "_y", y, ".tiff"),
               width = 24, height = 18, units = "cm",
               device = "tiff")
    } else {
        data %>% 
            ggplot(aes_string(x = "time", y = y)) +
            geom_bar(stat = "identity", position = "dodge", aes(fill = type)) +
            geom_text(position = position_dodge(width = 1),
                      size = 2.5,
                      aes(label = paste0(round(data[, y], 2) * 100, "%"),
                          y = data[, y] + 0.025,
                          group = type)) + 
            scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, 0.2),
                               labels = paste0(seq(0, 1, 0.2) * 100, "%")) +
            scale_fill_grey(start = 0.8, end = 0.2) + 
            labs(x = NULL, y = NULL) +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  axis.ticks.x = element_blank()) +
            guides(fill = guide_legend(nrow = 1))
        ggsave(paste0(file_name, "_y", y, "_xTime.png"),
               width = 24, height = 18, units = "cm")
        ggsave(paste0(file_name, "_y", y, "_xTime.tiff"),
               width = 24, height = 18, units = "cm",
               device = "tiff")
        
        data %>% 
            ggplot(aes_string(x = "type", y = y)) +
            geom_bar(stat = "identity", position = "dodge", aes(fill = time)) +
            geom_text(position = position_dodge(width = 1),
                      size = 2.5,
                      aes(label = paste0(round(data[, y], 2) * 100, "%"),
                          y = data[, y] + 0.025,
                          group = time)) + 
            scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, 0.2),
                               labels = paste0(seq(0, 1, 0.2) * 100, "%")) +
            scale_fill_grey(start = 0.8, end = 0.2) + 
            labs(x = NULL, y = NULL) +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  axis.ticks.x = element_blank()) +
            guides(fill = guide_legend(nrow = 1))
        ggsave(paste0(file_name, "_y", y, "_xType.png"),
               width = 24, height = 18, units = "cm")
        ggsave(paste0(file_name, "_y", y, "_xType.tiff"),
               width = 24, height = 18, units = "cm",
               device = "tiff")
    }
}

for (var_y in c("overweight", "pooly_controlled")) {
    # for (var_file_name in c("Dm", "Sex", "Age")) {
    # create_plot(data = d_type, y = var_y, file_name = file.path(output_path, var_file_name))
    # }
    
    for (var_type in c("T1D", "T2D")) {
        create_plot(data = d_type %>% 
                        filter(type == var_type),
                    y = var_y,
                    file_name = file.path(output_path, var_type))
    }
}

create_plot(data = d_type_2, y = "well_controlled", file_name = file.path(output_path, "Dm"))



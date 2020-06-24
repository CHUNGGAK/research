options(scipen = 999)

library(shiny)
library(tidyverse)
library(DT)
library(CohortMethod)

source("helpers.R")

# Define ui ---------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Estimation Viewer"),
    
    fluidRow(
        column(12,
               textInput("text", "Output path", value = "/home/lee/workspace/respiratory/output/2020-06-11_13:09:22")),
        
        column(12,
               dataTableOutput("summary")),
        
        column(12,
               verbatimTextOutput("selected_row"),
               tabsetPanel(
                   tabPanel("Attrition",
                            plotOutput("attrition_diagram")),
                   tabPanel("Population characteristics",
                            dataTableOutput("table1")),
                   tabPanel("Propensity model",
                            dataTableOutput("propensity_model")),
                   tabPanel("Propensity scores",
                            plotOutput("propensity_score_1"),
                            plotOutput("propensity_score_2")),
                   tabPanel("Covariate balance",
                            plotOutput("balance_scatter_plot"),
                            plotOutput("balance_top_variables")),
                   tabPanel("Follow-up",
                            plotOutput("follow_up")),
                   tabPanel("Time-to-event",
                            plotOutput("time_to_event")))
        )
    )
)


# Define server logic -----------------------------------------------------
server <- function(input, output) {
    prepare_viewer(input$text)
    
    summary <- reactive({
        read_csv(file.path(input$text, "shiny_data/summary.csv")) %>% 
            select(-logRr, -seLogRr) %>% 
            mutate_at(vars(rr, ci95lb, ci95ub, p), funs(round(., 2)))
    })
    
    output$summary <- renderDataTable(summary(),
                                      selection = "single")
    
    result <- reactive({
        read_csv(file.path(input$text, "shiny_data/result.csv"))
    })
    
    output$selected_row <- renderPrint(result()$strataFile[input$summary_rows_selected])
    
    matchedPop <- reactive({
        read_rds(file.path(input$text,
                           result()$strataFile[input$summary_rows_selected]))
    })
    
    output$attrition_diagram <- renderPlot(drawAttritionDiagram(matchedPop()))
    
    balance <- reactive({
        read_csv(list.files(file.path(input$text,
                                      "shiny_data/balance"),
                            str_sub(result()$strataFile[input$summary_rows_selected], 10, -5),
                            full.names = TRUE))
    })
    
    output$table1 <- renderDataTable(createCmTable1(balance()))
    
    ps <- reactive({
        read_rds(file.path(input$text,
                  result()$psFile[input$summary_rows_selected]))
    })

    cm <- reactive({
        loadCohortMethodData(file.path(input$text,
                                       result()$cohortMethodDataFolder[input$summary_rows_selected]))
    })

    output$propensity_model <- renderDataTable(getPsModel(ps(), cm()))
    
    output$propensity_score_1 <- renderPlot(plotPs(ps()))

    output$propensity_score_2 <- renderPlot(plotPs(matchedPop(), ps()))

    output$balance_scatter_plot <- renderPlot(plotCovariateBalanceScatterPlot(balance(),
                                                                              showCovariateCountLabel = TRUE,
                                                                              showMaxLabel = TRUE))

    output$balance_top_variables <- renderPlot(plotCovariateBalanceOfTopVariables(balance()))

    output$follow_up <- renderPlot(plotFollowUpDistribution(matchedPop()))
    
    output$time_to_event <- renderPlot(plotTimeToEvent(cohortMethodData = cm(),
                                                       outcomeId = summary()$outcomeId[input$summary_rows_selected],
                                                       firstExposureOnly = FALSE,
                                                       washoutPeriod = 0,
                                                       riskWindowStart = 3,
                                                       startAnchor = "cohort start",
                                                       riskWindowEnd = 33,
                                                       endAnchor = "cohort start",
                                                       showFittedLines = FALSE,
                                                       periodLength = 1,))
}


# Run the app -------------------------------------------------------------
shinyApp(ui, server)


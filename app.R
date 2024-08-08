library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
    dashboardHeader(title = "Data catalogue wireframe"),
    dashboardSidebar(title = "Filtering",
                     sliderInput("slider", "Date range:", 
                                 min = as.Date("2019-04-01","%Y-%m-%d"),
                                 max = as.Date("2024-08-01","%Y-%m-%d"),
                                 value=c(as.Date("2019-04-01"), as.Date("2024-08-01")),
                                 timeFormat="%Y-%m")
                     
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(DT::DTOutput("totals"), width=12),
            box(plotOutput("monthly_count", height = 250)),
            box(plotOutput("stat_numeric", height = 250))
            
        )
    )
)

server <- function(input, output) { 
    totals <-  tibble(
        concept_id = c(2212648, 2617206, 2212406),
        name = c("Blood count; complete (CBC), automated (Hgb, Hct, RBC, WBC and platelet count) and automated differential WBC count",
                 "Prostate specific antigen test (psa)",
                 "Homocysteine"),
        person_count = c(7080, 960, 10),
        records_per_person = c(4.37, 1.12, 1.06)
    )
    # looks like can pick up rows selected from the datatable: input$totals_rows_selected
    output$totals <- DT::renderDT(counts, selection = list(mode = 'multiple', selected = c(2), target = 'row'))
    
    
    monthly_count <- tibble(
        date = c("2020-01","2020-02", "2020-03", "2020-04"),
        record_count = c(120, 250, 281, 220)
    )
    
    
    output$monthly_count <- renderPlot(
            ggplot(monthly_count, aes(x=date, y=record_count)) +
            geom_bar(stat = "identity") +
            ggtitle("Prostate specific antigen test (psa)") +
            xlab("Month") +
            ylab("Number of records")
    )
   
    summary_stat <- tibble(
        concept = "Prostate specific antigen test (psa)",
        sd = c(0.8280661),
        mean = c(5.843)
    )
    
    output$stat_numeric <- renderPlot({
        ggplot(summary_stat, aes(x=concept)) +
            geom_boxplot(
                aes(lower = mean - sd,
                    upper = mean + sd,
                    middle = mean,
                    ymin = mean - 3*sd,
                    ymax = mean + 3*sd),
        stat="identity")
            
    })
}

shinyApp(ui, server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(dplyr)
library(ggplot2)

ag_data <- read_csv("berries.csv", col_names = TRUE)
ag_data %>% summarize_all(n_distinct) -> aa
bb <- which(aa[1,]==1)
cn <- colnames(ag_data)[bb]
ag_data %<>% select(-all_of(bb))
aa %<>% select(-all_of(bb)) 
ag_data %<>% select(-4)
aa %<>% select(-4) 
kable(head(ag_data)) %>% kable_styling(font_size=12)
berry <- unique(ag_data$Commodity)
nberry <- length(berry)
rberry <- ag_data %>% filter((Commodity=="RASPBERRIES") & (Period=="YEAR"))
rberry %<>% select(-c(Period, Commodity))   
sum(str_detect(rberry$`Data Item`, "^RASPBERRIES")) == length(rberry$`Data Item`)
rberry %<>% separate(`Data Item`, c("Type", "Production"), sep = "-")
rberry %<>% separate(Type, c("Berry", "Type", "Market"), sep = ",")
rberry %<>% separate(Production, c("Production", "Measure", "Average"), sep = ",")
rberry[is.na(rberry)] <- ""
rberry$Market <- ifelse(rberry$Type==" PROCESSING ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" PROCESSING ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""
rberry$Market <- ifelse(rberry$Type==" UTILIZED ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" UTILIZED ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""
rberry$Market <- ifelse(rberry$Type==" FRESH MARKET ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" FRESH MARKET ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""
rberry$Market <- ifelse(rberry$Type==" NOT HARVESTED ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" NOT HARVESTED ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""
rberry$Market <- ifelse(rberry$Type==" NOT SOLD ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" NOT SOLD ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""
rberry %<>% separate(Domain, c("D_left", "D_right"), sep = ", ")
rberry[is.na(rberry)] <- ""
rberry$Chemical_Type <- ifelse(rberry$D_left=="FERTILIZER", rberry$D_left, rberry$D_right)
rberry %<>% separate(`Domain Category`, c("DC_left", "DC_right"), sep = ":")
rberry[is.na(rberry)] <- ""
rberry %<>% rename(Chemicals = DC_right)
rberry %<>% select(-c(D_left, D_right, DC_left, Berry, Average))
rberry$Value <- ifelse(rberry$Value=="(D)", NA, rberry$Value)
rberry[is.na(rberry)] <- ""
rberry$Value <- ifelse(rberry$Value=="(NA)", NA, rberry$Value)
rberry[is.na(rberry)] <- ""
rberry2 <- rberry %>% select(Year, State, Measure, Chemical_Type, Value)
rberry2 %<>% filter(Measure==" MEASURED IN LB")
rberry2$Value %<>% as.numeric()
Chemical_Type <- rberry2$Chemical_Type
Year <- rberry2$Year
State <- rberry2$State
groups <- c("Chemical Type", "Year", "State")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Berries Project"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10)
                ),
            fluidRow(
                selectInput("group", "Grouping Variable", groups)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                plotOutput("distPlot")
            ),
            fluidRow(
                tableOutput("summary")
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- rberry2$Value
        bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab = "Number of LBs", main = "Histogram of LBs")
    })
    
    output$summary <- renderTable({
        if(isTRUE(input$group=="Chemical Type")){
            table <- rberry2 %<>% group_by(Chemical_Type) %>% summarise(mean(Value, na.rm = TRUE))
        }
        if(isTRUE(input$group=="Year")){
            table <- rberry2 %<>% group_by(Year) %>% summarise(mean(Value, na.rm = TRUE))
        }
        if(isTRUE(input$group=="State")){
            table <- rberry2 %<>% group_by(State) %>% summarise(mean(Value, na.rm = TRUE))
        }
        table
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

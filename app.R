#install.packages(c("rsconnect", "shiny", "ggplot2", "scales", "shinythemes", "tidyverse, 
#"shinyWidgets", "ggrepel", "itertools"))

library(rsconnect)
library(shiny)
library(ggplot2)
library(scales)
library(shinythemes)
library(shinyWidgets)
library(ggrepel)
library(itertools)


data <- read.csv("data/finalData.csv", head=T, sep=",")
data$GEO <- factor(data$GEO, levels=c("Canada", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"))
data$Age.group <- factor(data$Age.group, levels = c("Total age group", "0-24 yrs", "25-34 yrs", "35-44 yrs", 
                                                    "45-54 yrs", "55-64 yrs", "65+ yrs", "65-74 yrs", "75+ yrs"))
data$Sex <- factor(data$Sex, levels = c("Both sexes", "Males", "Females"))

ui <- fluidPage(theme=shinytheme("paper"),
  titlePanel("CIS & T1FF income app"),
  sidebarLayout(
    sidebarPanel(width = 3,
      checkboxGroupInput(inputId = "dataSource", 
                         label="Select data source(s):",
                         choices = c("Canadian Income Survey" = "CIS",
                                     "T1 Family File" = "T1FF"),
                         selected = c("CIS", "T1FF")),
      sliderInput(inputId = "refYears",
                  label = "Select a reference period:",
                  min = 1976,
                  max = 2018,
                  value = c(2000,2017),
                  sep = ""),
      selectInput(inputId = "incomeSource",
                  label = "Select an income source:",
                  choices = c("Total income",
                              "Market income",
                              "Employment income",
                              "Wages, salaries and commissions",
                              "Self-employment income",
                              "Government transfers"),
                  selected = "Total income"),
      selectInput(inputId = "stat",
                  label = "Select a statistic:",
                  choices = c(#"Number of persons" = "Number of persons",
                              "Number of persons with income" = "Number with income",
                              "Aggregate income (x1,000)" = "Aggregate income",
                              "Average income",
                              "Median income"),
                  selected = "Number with income"),
      selectInput(inputId = "geo",
                  label = "Geography:",
                  choices = c("Canada" = "Canada", "Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                              "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                              "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                              "British Columbia" = "BC", "Yukon" = "YT", "Northwest Territories" = "NT",
                              "Nunavut" = "NU"),
                  selected = "Canada"),
      fluidRow(
        column(6, selectInput(inputId = "sex",
                              label = "Sex:",
                              choices = c("Both sexes", "Males", "Females"),
                              selected = "Both sexes")),

        column(6, selectInput(inputId = "age",
                              label = "Age group:",
                              choices = c("All age groups" = "Total age group", "0 to 24 years" = "0-24 yrs", 
                                          "25 to 34 years" = "25-34 yrs", "35 to 44 years" = "35-44 yrs", 
                                          "45 to 54 years" = "45-54 yrs", "55 to 64 years" ="55-64 yrs", 
                                          "65 years and over" = "65+ yrs", "65 to 74 years" = "65-74 yrs",
                                          "75 years and over" = "75+ yrs"),
                              selected = "Total age group"))
      )
      #actionButton(inputId = "update", label="Update", icon=icon("sync"))
    ),
    mainPanel(width=9,
              fluidRow(style="display: flex; align-items: flex-end; overflow: visible;",
                       column(8,
                              div(style = "display: inline-block;vertical-align:top;",
                                  dropdownButton(label = "Geography",
                                                 circle = F,
                                                 size = "sm",
                                                 status = "custom",
                                                 checkboxGroupInput("yearsByGeo", 
                                                                    label = NULL, 
                                                                    choices = c("Canada" = "Canada", "Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                                                                "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                                                                "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                                                                "British Columbia" = "BC", "Yukon" = "YT", "Northwest Territories" = "NT",
                                                                                "Nunavut" = "NU")))),
                              div(style = "display: inline-block;vertical-align:top;",
                                  dropdownButton(label = "Sex",
                                                 circle = F,
                                                 size = "sm",
                                                 status = "custom",
                                                 checkboxGroupInput("yearsBySex",
                                                                    label = NULL,
                                                                    choices = c("Both sexes", "Males", "Females")))),
                              div(style = "display: inline-block;vertical-align:top;",
                                  dropdownButton(label = "Age group",
                                                 circle = F,
                                                 size = "sm",
                                                 status = "custom",
                                                 checkboxGroupInput("yearsByAge",
                                                                    label = NULL,
                                                                    choices = c("All age groups" = "Total age group", "0 to 24 years" = "0-24 yrs", 
                                                                                "25 to 34 years" = "25-34 yrs", "35 to 44 years" = "35-44 yrs", 
                                                                                "45 to 54 years" = "45-54 yrs", "55 to 64 years" ="55-64 yrs", 
                                                                                "65 years and over" = "65+ yrs", "65 to 74 years" = "65-74 yrs",
                                                                                "75 years and over" = "75+ yrs")))),
                              tags$style(".btn-custom {-webkit-box-shadow: none; box-shadow: none; position: relative;}"),
                              plotOutput("yearsPlot")),
                       column(4, plotOutput("sexPlot"))
              ),
              fluidRow(
                column(5, plotOutput("agePlot")),
                column(7, plotOutput("provsPlot"))
              ),
              br()
    )
  ),
  wellPanel(tags$small(p("Notes:"),
                       tags$ul(
                         tags$li("The max year selected is the year of primary interest"),
                         tags$li("Note that not all combinations of source of income and statistic are available for all data sources"),
                         tags$li("CIS is available for 1976 - 2018"),
                         tags$li("T1FF is available for 2000 - 2017"),
                         tags$li("The minimum age for income information on the CIS is 16 years"),
                         tags$li("While the CIS presents 2018 constant dollars, the T1FF simply uses dollars"),
                         tags$li("The T1FF includes dependants in the count of individuals and all other statistics"))))
)


server <- function(input, output) {
  
# Plot of statistic over reference period
  output$yearsPlot <- renderPlot({
    if (is.null(input$yearsByGeo)  & is.null(input$yearsByAge) & is.null(input$yearsBySex)) 
      yearsData <- subset(data, Income.source == input$incomeSource &
                            Statistics == input$stat &
                            input$refYears[1] <= Year & 
                            Year <= input$refYears[2] &
                            Data.source %in% input$dataSource &
                            Age.group == input$age &
                            Sex == input$sex &
                            GEO == input$geo)
    else yearsData <- subset(data, Income.source == input$incomeSource &
                            Statistics == input$stat &
                            input$refYears[1] <= Year & 
                            Year <= input$refYears[2] &
                            Data.source %in% input$dataSource &
                            {if (is.null(input$yearsByAge)) Age.group == input$age
                              else Age.group %in% input$yearsByAge} &
                            {if (is.null(input$yearsBySex)) Sex == input$sex
                              else Sex %in% input$yearsBySex} &
                            {if (is.null(input$yearsByGeo)) GEO == input$geo
                              else GEO %in% input$yearsByGeo})
    
    # only show selected variables in the interaction legend
    byVarsBool <- c(!is.null(input$yearsByGeo),
                   !is.null(input$yearsByAge),
                   !is.null(input$yearsBySex),
                   !is.na(input$dataSource[2]))
    
    byVars <- c("GEO", "Age.group", "Sex", "Data.source")
    
    byVars <- byVars[unlist(byVarsBool)]
    
    ggplot(yearsData) + 
      geom_line(aes(x=Year, y=VALUE, colour=interaction(yearsData[,byVars] ,drop=T, sep=", ")), size=1.5) + 
      theme_classic() + scale_y_continuous(labels = comma, limits = c(0,NA)) +
      labs(y=paste(input$stat,input$incomeSource), 
           x="Year", 
           title=paste(input$stat, ",", input$incomeSource, "by year"),
           colour=NULL) 
    
  })
  
# Plot of statistic by geography
  dataProv <- reactive({data$highlight <- ifelse((data$GEO == input$geo), 1, ifelse((input$geo == "Canada"),1,0))
                        return(subset(data,
                                      Age.group == input$age &
                                      Sex == input$sex &
                                      GEO != "Canada" &
                                      Year == input$refYears[2] &
                                      Income.source == input$incomeSource &
                                      Statistics == input$stat &
                                      Data.source %in% input$dataSource))})
  
  output$provsPlot <- renderPlot({
    
    ggplot(dataProv()) + 
      geom_col(aes(x=GEO, y=VALUE, fill=Data.source, alpha = highlight), position="dodge") + 
      theme_classic() + scale_y_continuous(labels = comma) +
      scale_alpha(range = c(max(0.45, min(dataProv()$highlight)),1)) +
      guides(alpha = FALSE) +
      labs(y=paste(input$stat,input$incomeSource), 
           x="Province", 
           title=paste(input$stat, ",", input$incomeSource, "\nby province/territory,",input$refYears[2]),
           fill="Data source")
  })

  
# Plot of statistic by sex
  dataSex <- reactive ({data$highlight <- ifelse((data$Sex == input$sex), 1, ifelse((input$sex == "Both sexes"),1,0))
                        return(subset(data,
                                      GEO == input$geo &
                                        Sex != "Both sexes" &
                                        Age.group == input$age &
                                        Year == input$refYears[2] &
                                        Income.source == input$incomeSource &
                                        Statistics == input$stat &
                                        Data.source %in% input$dataSource))})
  
  output$sexPlot <- renderPlot({
    
    ggplot(dataSex()) + 
      geom_col(aes(x=Sex, y=VALUE, fill=Data.source, alpha = highlight), position="dodge") + 
      theme_classic() + scale_y_continuous(labels = comma) +
      scale_alpha(range = c(max(0.45, min(dataSex()$highlight)),1)) +
      guides(alpha = FALSE) +
      labs(y=paste(input$stat,input$incomeSource), 
           x="Sex", 
           title=paste(input$stat, ",", input$incomeSource, "\nby sex,",input$refYears[2]),
           fill="Data source")
  })
  
# Plot of statistic by age group
  dataAge <- reactive ({data$highlight <- ifelse((data$Age.group == input$age), 1, ifelse((input$age == "Total age group"),1,0))
                        return(subset(data,
                                      GEO == input$geo &
                                       Sex == input$sex &
                                       Age.group != "Total age group" &
                                       Year == input$refYears[2] &
                                       Income.source == input$incomeSource &
                                       Statistics == input$stat &
                                       Data.source %in% input$dataSource))})
  
  output$agePlot <- renderPlot({
    
    ggplot(dataAge()) + 
      geom_col(aes(x=Age.group, y=VALUE, fill=Data.source, alpha = highlight), position="dodge") + 
      theme_classic() + scale_y_continuous(labels = comma) +
      scale_alpha(range = c(max(0.45, min(dataAge()$highlight)),1)) +
      guides(alpha = FALSE) +
      labs(y=paste(input$stat,input$incomeSource), 
           x="Age group", 
           title=paste(input$stat, ",", input$incomeSource, "\nby age group,",input$refYears[2]),
           fill="Data source")
  })
}

shinyApp(ui, server)

library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(marital)
library(ggpubr)
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
data("floridaCountyMarital")

countyNames<-levels(as.factor(floridaCountyMarital@geography$NAME))

codebookColon<-str_locate(countyNames, "County")[,1]

codebook<- data.frame(cbind(str_sub(countyNames, 0, codebookColon - 2),
                            floridaCountyMarital@geography$county))

colnames(codebook)<-c("NAME","county")

# Define UI for application that plots features of movies
ui <- fluidPage(
  theme=shinytheme("flatly"),
  titlePanel("Marital by sex, 2009 - 2016", windowTitle = "Marital by sex"),

  # Sidebar layout with a input and output definitions
  sidebarLayout(

    # Inputs
    sidebarPanel(

      h3("County"),      # Third level header: County

      # Select County for agePlot
      selectInput(inputId = "county",
                  label = "Choose a County:",
                  choices = c(as.character(codebook$NAME)),
                  selected = "Miami-Dade"),

      hr(),


      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = FALSE),

      br(),

      # Built with Shiny by RStudio
      br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         "."),width=3

    ),

    # Output:
    mainPanel(

      tabsetPanel(id = "tabspanel", type = "tabs",
                  tabPanel(title = "Plot",
                           fluidRow(
                             column(6, plotOutput("maleNeverPlot", height = 250)), # notice the ,
                             column(6, plotOutput("femaleNeverPlot", height = 250))
                           ), #end of this fluidRow notice comma needed before the next fluidRow()
                           fluidRow(
                             column(6, plotOutput("maleMarriedPlot", height = 250)), # notice the ,
                             column(6, plotOutput("femaleMarriedPlot", height = 250))
                           ), #end of this fluidRow notice comma needed before the next fluidRow()
                           fluidRow(
                             column(6, plotOutput("maleabsentPlot", height = 250)), # notice the ,
                             column(6, plotOutput("femaleabsentPlot", height = 250))
                           ), #end of this fluidRow notice comma needed before the next fluidRow()
                           fluidRow(
                             column(6, plotOutput("maleotherPlot", height = 250)), # notice the ,
                             column(6, plotOutput("femaleotherPlot", height = 250))
                           ), #end of this fluidRow notice comma needed before the next fluidRow()
                           fluidRow(
                             column(6, plotOutput("maleWidowedPlot", height = 250)), # notice the ,
                             column(6, plotOutput("femaleWidowedPlot", height = 250))
                           ), #end of this fluidRow notice comma needed before the next fluidRow()
                           fluidRow(
                             column(6, plotOutput("maleDivorcedPlot", height = 250)), # notice the ,
                             column(6, plotOutput("femaleDivorcedPlot", height = 250))
                           ) #end of this fluidRow notice comma needed before the next fluidRow()
                  ),
                  tabPanel(title = "Data",
                           br(),
                           DT::dataTableOutput(outputId = "Demotable")),
                  # New tab panel for Codebook
                  tabPanel("Codebook",
                           br(),
                           DT::dataTableOutput("codebook"))

      )
    )
  ))


# Define server function required to create the scatterplot
server <- function(input, output, session) {
  # Create a subset of data filtering for selected County
  CT <- reactive({

    req(input$county) # ensure availablity of value before proceeding
    county <- as.character(codebook$county[codebook$NAME %in% input$county])
    #Pad the county code

    paddedCounty <- str_pad(county, 3, "left", pad = "0")

    recordNumber <- which(floridaCountyMarital@geography$county == paddedCounty)
    #Subset the data by county
    NG<-floridaCountyMarital[recordNumber]
    # Create a subset of data filtering for selected County
    x<-ncol(floridaCountyMarital[recordNumber]@estimate)
    NR<-matrix(floridaCountyMarital[recordNumber]@estimate,x,1)
    colons <- str_locate_all(NG@acs.colnames, ":")[[1]]
    rownames(NR)<-str_sub(NG@acs.colnames, colons[1] +2, -1)
    colnames(NR)<-"Estimate"
    NR
  })



  # Create scatterplot object the plotOutput function is expecting
  output$maleNeverPlot <- renderPlot({
   agePlot(input$county,  "Male", "Never married")
  })
  output$femaleNeverPlot <- renderPlot({
    agePlot(input$county,  "Female", "Never married")
  })
  output$maleMarriedPlot <- renderPlot({
    agePlot(input$county,  "Male", "Married")
  })
  output$femaleMarriedPlot <- renderPlot({
    agePlot(input$county,  "Female", "Married")
  })
  output$maleabsentPlot <- renderPlot({
    agePlot(input$county,  "Male", "Married, spouse absent")
  })
  output$femaleabsentPlot <- renderPlot({
    agePlot(input$county,  "Female", "Married, spouse absent")
  })
  output$maleotherPlot <- renderPlot({
    agePlot(input$county,  "Male", "Married, other")
  })
  output$femaleotherPlot <- renderPlot({
    agePlot(input$county,  "Female", "Married, other")
  })
  output$maleWidowedPlot <- renderPlot({
    agePlot(input$county,  "Male", "Widowed")
  })
  output$femaleWidowedPlot <- renderPlot({
    agePlot(input$county,  "Female", "Widowed")
  })
  output$maleDivorcedPlot <- renderPlot({
    agePlot(input$county,  "Male", "Divorced")
  })
  output$femaleDivorcedPlot <- renderPlot({
    agePlot(input$county,  "Female", "Divorced")
  })

  # Update code below to render data table regardless of current county of input$show_data
  output$Demotable <- DT::renderDataTable({
    DT::datatable(data = CT(),
                  options = list(pageLength = 10),
                  rownames = TRUE)
  })

  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabspanel", target = "Data", select = TRUE)
    } else {
      hideTab(inputId = "tabspanel", target = "Data")
    }
  })

  # Render data table for codebook
  output$codebook <- DT::renderDataTable({
    DT::datatable(data = codebook,
                  options = list(pageLength = 10, lengthMenu = c(10, 25, 40)),
                  rownames = FALSE)
  })

}

# Create Shiny app object
shinyApp(ui = ui, server = server)

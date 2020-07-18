## This Shiny app is intended to help summarise data and provide a customisable graph in the form of data points, means, and SEs
## @RB, 2019

##---- LOAD DATA ----
library(shiny)
library(DT)
library(data.table)
library(tidyverse)
library(plotrix)
library(markdown)
  
##---- UI ----
ui <- navbarPage('Graph Maker',
##PLOT TAB----                 
   tabPanel('Plot',
            sidebarPanel(
            helpText('A semi-customisable graph and summary data from a data file'),
            fileInput("file",
                      label = h4("Select a csv or txt file with your raw data"),
                      multiple=FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            selectInput('xinput',
                        'X-axis data',
                        choices='',
                        selected='',
                        multiple=FALSE),
            selectInput('yinput',
                        'Y-axis data',
                        choices='',
                        selected='',
                        multiple=FALSE),
            selectInput('plotcond',
                        'Conditional',
                        choices='',
                        selected='',
                        multiple=FALSE),
            textInput("xtext", label = h4("X-axis label"), value = "x axis"),
            textInput("ytext", label = h4("Y-axis label"), value = "y axis"),
            textInput("plottitle", label = h4("Plot Title"), value = ""),
            checkboxInput("xlabcheck", label = "X axis labels", value = TRUE),
            checkboxInput("ylabcheck", label = "Y axis labels", value = TRUE),
            checkboxInput("points", label = "Points/Bars", value = TRUE),
            checkboxInput("errors", label = "Error Bars", value = TRUE),
            checkboxInput("jitter", label = "Jitter data points", value = TRUE),
            downloadButton("downloadPlot", "Download"),
            br(),br(),br(),br(),br()),
   mainPanel(
     h2('Graph'),
     plotOutput("plot"))
),
##SUMMARY TAB----
tabPanel('Summary',
         sidebarPanel(
           helpText('Means and SE for the data'),
           selectInput('summvar1',
                       'Variable 1',
                       choices='',
                       selected='',
                       multiple=FALSE),
           selectInput('summvar2',
                       'Variable 2',
                       choices='',
                       selected='',
                       multiple=FALSE),
           selectInput('summvalue',
                       'Value',
                       choices='',
                       selected='',
                       multiple=FALSE),
           radioButtons("summhead", "Show",
                        choices = c(Head = "head",
                                    All = "all"),
                        selected = "head"),
           h5('Click button to download the table currently displayed'),
           downloadButton("downloadSummary", "Download")),
         mainPanel(
     h2('Summary Statistics'),
     tableOutput('summarytable'),
     br(),br(),br(),br(),br())),
##RAW DATA TAB----
tabPanel('Raw Data',
         sidebarPanel(
           helpText('Select the cultivars and conditions you\'d like to view in the table to the right'),
           h3('Selections for Raw Data'),
           radioButtons("rawhead", "Show",
                        choices = c(Head = "head",
                                    All = "all"),
                        selected = "head")),
         mainPanel(
     h2('Raw Data'),
     tableOutput('table'),
     br(),br(),br(),br(),br()
     )),
tags$footer("Created by Richard Browne, La Trobe University, 2019. Last updated 23/02/2020.", align = "right", style = "
  position:fixed;
  min-height: 5vh;
  bottom:0;
  width:100%;
  height:40px;
  color: grey;
  padding: 10px;
  background-color: black;
  z-index: 1000;")
)
   
##---- SERVER ----
server <- function(input, output, session) {

##Load Data File ----
  file_in <- reactive({
    req(input$file)
    data <- fread(input$file$datapath)
    return(data)
  })
  
##Make Raw Datatable ----
  output$table <- renderTable({
    req(file_in)
    data <- file_in()
   if(input$rawhead == "head") {
      return(head(data, 10))
    }
    else {
      return(data)
    }
  })
  
##Summarise Datafile ----
  summary_stats <- reactive({
    req(file_in)
    data <- file_in() %>% 
      group_by(!! rlang::sym(input$summvar1), !! rlang::sym(input$summvar2)) %>%  
    summarise(means = mean(!! rlang::sym(input$summvalue)), se=std.error(!! rlang::sym(input$summvalue))) %>% 
    mutate_if(is.numeric, round, 2)
        })
##Stats for Graph ----
  plot_stats <- reactive({
    req(file_in)
    data <- file_in() %>% 
      group_by(!! rlang::sym(input$xinput), !! rlang::sym(input$plotcond)) %>%  
      summarise(means = mean(!! rlang::sym(input$yinput)), se=std.error(!! rlang::sym(input$yinput))) %>% 
      mutate_if(is.numeric, round, 2)
  })
  
  
##Output Summary Table ----
  output$summarytable <- renderTable({
    req(file_in)
    data <- summary_stats()
    if(input$summhead == "head") {
      return(head(data, 10))
    }
    else {
      return(data)
    }
    return(data)
  })
  
##Update Dropdowns ----
  observeEvent( 
    input$file,{
      updateSelectInput(session,
                        'xinput',
                        choices=colnames(file_in()))
      updateSelectInput(session,
                        'yinput',
                        choices=colnames(file_in()))
      updateSelectInput(session,
                        'plotcond',
                        choices=colnames(file_in()))
      updateSelectInput(session,
                        'summvar1',
                        choices=colnames(file_in()))
      updateSelectInput(session,
                        'summvar2',
                        choices=colnames(file_in()))
      updateSelectInput(session,
                        'summvalue',
                        choices=colnames(file_in()))
        })

##Download Button ----
  output$downloadSummary <- downloadHandler(
    filename = function(){paste('datasummary.csv')},
    content = function(file) {

      write.csv(summary_stats(), file, row.names = FALSE)
    }
  )

##Outut Plot ----
  output$plot <- renderPlot({
    req(plot)
    plotPrint <- plot()
    plotPrint
  })
  
##Make Plot ----  
  plot <- reactive({
  req(input$file)
  p <- ggplot()
  if(input$points == TRUE && input$jitter == TRUE) {
    p <- p + geom_point(data=file_in(), aes(x=!! rlang::sym(input$xinput), y=!! rlang::sym(input$yinput), colour=!! rlang::sym(input$plotcond), alpha=0.75),
                   size = 3,
                   position = position_jitterdodge(dodge.width=0.5, jitter.width=0.25))
        } else {
  if(input$points == TRUE && input$jitter == FALSE) {
            p <- p + geom_point(data=file_in(), aes(x=!! rlang::sym(input$xinput), y=!! rlang::sym(input$yinput), colour=!! rlang::sym(input$plotcond), alpha=0.75),
                                size = 3, position=position_dodge(width=0.5))
        } else {
            p <-  p + geom_col(data=plot_stats(), aes(x=!! rlang::sym(input$xinput),y=means,group=!! rlang::sym(input$plotcond),fill=!! rlang::sym(input$plotcond), alpha=0.75), width=0.25, position=position_dodge(width=0.5))
        }
      }
  if(input$points == TRUE && input$errors == TRUE) {
    p <- p + geom_errorbar(data = plot_stats(), colour = "black",
                           aes(x=!! rlang::sym(input$xinput),
                               ymin=c(means - se), group=!! rlang::sym(input$plotcond),
                               ymax=c(means + se)), width=0.25,
                           position=position_dodge(width=0.5)) +
      geom_point(data=plot_stats(), colour="black",
                 aes(x=!! rlang::sym(input$xinput),y=means,group=!! rlang::sym(input$plotcond)),size=2, position=position_dodge(width=0.5))
    } else {
  if(input$points == FALSE && input$errors == TRUE) {
        p <- p + geom_errorbar(data = plot_stats(), colour = "black",
                               aes(x=!! rlang::sym(input$xinput),
                                   ymin=c(means - se), group=!! rlang::sym(input$plotcond),
                                   ymax=c(means + se)), width=0.25,
                               position=position_dodge(width=0.5))
    } else {
        p
      }
  }  
  p <- p  + theme(plot.title = element_text(hjust = 0.5),
            legend.key = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position="right",
            panel.background = element_blank(),
            text = element_text(size=20),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.line = element_line(colour = "black")) + 
      xlab(input$xtext) + 
      ylab(input$ytext) +
      guides(alpha=FALSE) +
      ggtitle(input$plottitle)
  if(input$xlabcheck == TRUE) {
      p
    } else {
     p <-  p + theme(axis.text.x = element_blank())
    }
  if(input$ylabcheck == TRUE) {
    p
    } else {
      p <- p + theme(axis.text.y = element_blank())
  }
  
  })
  
##Download Plot ----  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste('plot.png')},
    content = function(file) {
      ggsave(file, plot = plot(), device = "png")
    }
  )
}

##---- RUN APP ---- 
shinyApp(ui, server)
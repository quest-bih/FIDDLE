#This shiny app displays the Open Access Journal Whitelist in a
#searchable and filterable format in the browser using the datatable package
#

library(shiny)
library(DT)
library(tidyverse)
library(ggvis)
library("shinythemes")
library(RColorBrewer)
library(lubridate)

source("app_functions.R", encoding = "UTF-8")

null_results_table <- read_delim("data/Table_3.csv", delim = ";")
null_results_table <- as.matrix(null_results_table)
rownames(null_results_table) <- null_results_table[,1]
null_results_table <- null_results_table[,-1]
#null_results_table <- as_tibble(cbind(nms = names(null_results_table), t(null_results_table)))

url <- "https://twitter.com/intent/tweet?text=Where%20and%20how%20to%20publish%20null%20results%3f&url=http://s-quest.bihealth.org:3838/fiddle/"

ui <- navbarPage("FIDDLE", theme = shinytheme("flatly"),
                 tabPanel("Table Select",
                          fluidRow(
                            column(width = 3,
                                   tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 183, width = 280),href="https://www.bihealth.org/de/forschung/quest-center/")
                            ),
                            column(width = 9,
                                   h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                                   h4("Where and how to publish null / neutral results?", align = "center"),
                                   br(),
                                   h5("This tool provides information on different ways of publishing results that are
                                      either considered null results or only comprise a dataset instead of a full study.
                                      Choose a scenario that best reflects your case or the options that are most important
                                      to you. The most suitable options are then highlighted."),
                                   br()
                            )),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("Why are your data in the file drawer?"),
                                     radioButtons('scenario', 'Scenarios',
                                                 c("1. I don't have enough time to prepare a publication",
                                                   "2. My experiment or dataset is incomplete",
                                                   "3. I have data that may be useful to others, but don't have time to analyze everything",
                                                   "4. I have neutral or null results from a small, underpowered study or an exploratory study",
                                                   "5. I have neutral or null results from a large, adequately powered study",
                                                   "6. My study is completed, but the findings aren't novel or exciting",
                                                   "7. I need the research to be published quickly",
                                                   "8. I don't have funding to pay for publication charges",
                                                   "9. None of these describe my situation - show me the table of all options"),
                                                 selected = "9. None of these describe my situation - show me the table of all options"),
                                     helpText('Choose the scenario that best reflects your case.')
                                   ),
                                   tags$a(href=url, "Tweet", class="twitter-share-button"),
                                   includeScript("http://platform.twitter.com/widgets.js")
                            ),
                            column(9,
                                   DT::dataTableOutput("table")
                            )
                          )
                 ),
                 tabPanel("Table Options",
                          fluidRow(
                            column(width = 3,
                                   tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 183, width = 280),href="https://www.bihealth.org/de/forschung/quest-center/")
                            ),
                            column(width = 9,
                                   h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                                   h4("Where and how to publish null / neutral results?", align = "center"),
                                   br(),
                                   h5("This tool provides information on different ways of publishing results that are
                             either considered null results or only comprise a dataset instead of a full study.
                             Choose a scenario that best reflects your case or the options that are most important
                             to you. The most suitable options are then highlighted."),
                                   br()
                            )),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("Options"),
                                     radioButtons('Q1', 'I have a',
                                                  c("dataset only",
                                                    "minor or partial study",
                                                    "full-scale study",
                                                    "rejected manuscript")),
                                     radioButtons('Q2', 'Money I have to cover publication cost (in €)',
                                                 c("0",
                                                   "up to 500",
                                                   "up to 2000"), selected = "up to 2000"),
                                     checkboxGroupInput('Q3', 'I want the outcome to be indexed in',
                                                  c("Pubmed",
                                                    "Google scholar",
                                                    "Google"), selected = "Google"),
                                     radioButtons('Q4', 'I want my outcome to be peer-reviewed',
                                                  c("any",
                                                    "yes",
                                                    "no")),
                                     radioButtons('Q5', 'I want my publication or dataset to appear immediately',
                                                  c("yes",
                                                    "no"), selected = "no"),


                                     helpText('Choose the scenario that best reflects your case.')
                                   ),
                                   tags$a(href=url, "Tweet", class="twitter-share-button"),
                                   includeScript("http://platform.twitter.com/widgets.js")
                            ),
                            column(9,
                                   DT::dataTableOutput("table2")
                            )
                          )
                 )
)


server <- function(input, output) {

  #--------------------------------------------------------------------------------------------------
  # Code for Table
  #--------------------------------------------------------------------------------------------------

  output$table <- DT::renderDataTable({
    DT::datatable(null_results_table, #class = 'cell-border stripe',
                  options = list(
                    pageLength = 14,
                    lengthMenu = list(c(14),
                                      c(14)),
                    ordering=F,
                    searching=F,
                    paging=F,
                    info=F,
                    escape = FALSE),
              escape = FALSE) %>% formatStyle(
      set_chosen_columns(null_results_table, input$scenario),
      color = '#FFFFFF',
      backgroundColor = '#2C3E50'
    )

  })


  output$table2 <- DT::renderDataTable({
    DT::datatable(null_results_table,
                  options = list(
                    pageLength = 14,
                    lengthMenu = list(c(14),
                                      c(14)),
                    ordering=F,
                    searching=F,
                    paging=F,
                    info=F),
      escape = FALSE
    ) %>%   formatStyle(
      set_chosen_columns_2(null_results_table, input$Q1, input$Q2, input$Q3, input$Q4, input$Q5),
      color = '#FFFFFF',
      backgroundColor = '#2C3E50'
    )

  })


}

shinyApp(ui, server)


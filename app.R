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


#color = '#FFFFFF',
#backgroundColor = '#2C3E50'

url <- "https://twitter.com/intent/tweet?text=Where%20and%20how%20to%20publish%20null%20results%3f&url=http://s-quest.bihealth.org:3838/fiddle/"

ui <- navbarPage("FIDDLE", theme = shinytheme("flatly"), id = "navbarTabs",
                 tabPanel("Options", value = "tabOptions",
                          fluidRow(
                            column(width = 3,
                                   tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 183, width = 280),href="https://bihealth.org/quest-center/")
                            ),
                            column(width = 9,
                                   h1(HTML("FIDDLE - <font color=\"#AA1C7D\"><b> Fi</b></font>le
                                           <font color=\"#AA1C7D\"><b> D</b></font>rawer
                                           <font color=\"#AA1C7D\"><b> D</b></font>ata
                                           <font color=\"#AA1C7D\"><b> L</b></font>iberation
                                           <font color=\"#AA1C7D\"><b> E</b></font>ffort"), align = "center"),
                                   h4("Where and how to publish null / neutral results?", align = "center"),
                                   br(),
                                   wellPanel(HTML("<b>This “match-making” tool helps you to identify alternate ways of publishing information from well-designed experiments
                                                  that is often difficult to publish in traditional journals (i.e. null or neutral results, datasets, etc.).
                                                  Choose the criteria that are most relevant to you, or choose the scenario that best reflects your situation.
                                                  The tool will highlight the most suitable options.</b>"),
                                             align = "center"),
                                   #wellPanel(HTML("<font color=\"#FFFFFF\">This tool provides information on different ways of publishing results that are
                                    #       either considered null results or only comprise a dataset instead of a full study. Select options that best
                                    #       reflect your case and corresponding publishing possibilities then appear highlighted.</font>"),
                                    #         align = "center", style = "background: #2C3E50"),
                                   br()
                            )),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("Options"),
                                     helpText('Choose the criteria that are most important to you.'),
                                     radioButtons('Q1', 'I have a',
                                                  c("unanalyzed dataset",
                                                    "stand-alone finding or results from a small study",
                                                    "results from a full-scale study",
                                                    "rejected manuscript")),
                                     radioButtons('Q2', 'Money I have to cover publication cost (in €)',
                                                  c("0",
                                                    "up to 1000",
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

                                     helpText('If these options aren’t helpful, try our list of scenarios instead:'),
                                     actionButton('buttonToScenarios', 'Go to scenarios')
                                   ),
                                   tags$a(href=url, "Tweet", class="twitter-share-button"),
                                   includeScript("http://platform.twitter.com/widgets.js")


                            ),
                            column(9,
                                   DT::dataTableOutput("table2"),

                                   br(),

                                   helpText('This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license,
                                            visit ',a(href = 'https://creativecommons.org/licenses/by-sa/3.0/', 'https://creativecommons.org/licenses/by-sa/3.0/')),
                                   helpText('Bernard, René (concept); Bobrov, Evgeny (concept); Riedel, Nico (concept, technical implementation; Weissgerber, Tracey (concept)'),
                                   helpText('Contact address:'),
                                   helpText('Last update: 25.06.2019')
                            )
                          )
                 ),
                 tabPanel("Scenarios", value = "tabScenarios",
                          fluidRow(
                            column(width = 3,
                                   tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 183, width = 280),href="https://bihealth.org/quest-center/")
                            ),
                            column(width = 9,
                                   h1(HTML("FIDDLE - <font color=\"#AA1C7D\"><b> Fi</b></font>le
                                           <font color=\"#AA1C7D\"><b> D</b></font>rawer
                                           <font color=\"#AA1C7D\"><b> D</b></font>ata
                                           <font color=\"#AA1C7D\"><b> L</b></font>iberation
                                           <font color=\"#AA1C7D\"><b> E</b></font>ffort"), align = "center"),
                                   h4("Where and how to publish null / neutral results?", align = "center"),
                                   br(),
                                   wellPanel(HTML("<b>This “match-making” tool helps you to identify alternate ways of publishing information from well-designed experiments
                                                  that is often difficult to publish in traditional journals (i.e. null or neutral results, datasets, etc.).
                                                  Choose the criteria that are most relevant to you, or choose the scenario that best reflects your situation.
                                                  The tool will highlight the most suitable options.</b>"),
                                             align = "center"),
                                   br()
                            )),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("Why are your data in the file drawer?"),
                                     radioButtons('scenario', 'Scenarios',
                                                 c("I don't have enough time to prepare a publication",
                                                   "My experiment or dataset is incomplete",
                                                   "I have data that may be useful to others, but don't have time to analyze everything",
                                                   "I have neutral or null results from a small, underpowered study or an exploratory study",
                                                   "I have neutral or null results from a large, adequately powered study",
                                                   "My study is completed, but the findings aren't novel or exciting",
                                                   "I need the research to be published quickly",
                                                   "I don't have funding to pay for publication charges",
                                                   "None of these describe my situation - show me the table of all options"),
                                                 selected = "None of these describe my situation - show me the table of all options"),
                                     helpText('Go back to the options version: '),
                                    actionButton('buttonToOptions', 'Back to options')
                                   ),
                                   tags$a(href=url, "Tweet", class="twitter-share-button"),
                                   includeScript("http://platform.twitter.com/widgets.js")
                            ),
                            column(9,
                                   DT::dataTableOutput("table")
                            )
                          )
                 )
)


server <- function(input, output, session) {

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


  #actionButton to switch tabs ()
  observeEvent(input$buttonToScenarios, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabScenarios")
  })

  observeEvent(input$buttonToOptions, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabOptions")
  })

  #write(paste0("App visit at: ", Sys.time()), "/var/log/shiny-server/visitors_fiddle.txt", append = TRUE)
}

shinyApp(ui, server)


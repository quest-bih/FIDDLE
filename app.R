#This is the Shiny app for the FIDDLE tool, which is a “matchmaking” tool
#designed to help researchers to identify the publication format that will
#work best for a particular dataset that may be hard to publish in traditional journals

library(shiny)
library(DT)
library(tidyverse)
library(ggvis)
library("shinythemes")
library(RColorBrewer)
library(lubridate)

source("app_functions.R", encoding = "UTF-8")

null_results_table <- read_delim("data/Table.csv", delim = ";")
null_results_table <- as.matrix(null_results_table)
rownames(null_results_table) <- null_results_table[,1]
null_results_table <- null_results_table[,-1]

ui <- navbarPage("fiddle", theme = shinytheme("flatly"), id = "navbarTabs",
                 tabPanel("Options", value = "tabOptions",
                          fluidRow(
                            column(width = 3,
                                   tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 106, width = 280),href="https://www.bihealth.org/quest-center/")
                            ),
                            column(width = 9,
                                   h1(HTML(" <img src=\"fiddle_logo.png\" width=\"111\" height=\"44\">  - <font color=\"#AA1C7D\"><b> fi</b></font>le
                                           <font color=\"#AA1C7D\"><b> d</b></font>rawer
                                           <font color=\"#AA1C7D\"><b> d</b></font>ata
                                           <font color=\"#AA1C7D\"><b> l</b></font>iberation
                                           <font color=\"#AA1C7D\"><b> e</b></font>ffort"), align = "center"),
                                   h4("Where and how to publish null / neutral results?", align = "center"),
                                   br(),
                                   wellPanel(HTML('<b>This “match-making” tool helps you to identify alternate ways of publishing information from well-designed experiments
                                                  that is often difficult to publish in traditional journals (i.e. null or neutral results, datasets, etc.).
                                                  Choose the criteria that are most relevant to you, or choose the scenario that best reflects your situation.
                                                  The tool will highlight the most suitable options.<br> Check out our explainer video <a href="https://youtu.be/TxUeXUecZIw">here</a>.</b>'),
                                             align = "center"),
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
                                     radioButtons('Q2', 'Money I have to cover publication cost',
                                                  c("0",
                                                    "up to 1000 EUR/1100 US$",
                                                    "up to 2000 EUR/2200 US$"), selected = "up to 2000 EUR/2200 US$"),
                                     checkboxGroupInput('Q3', 'I want the outcome to be indexed in',
                                                        c("Pubmed",
                                                          "Pubmed Central",
                                                          "Web of Science",
                                                          "Scopus",
                                                          "CrossRef",
                                                          "Google scholar"), selected = c()),
                                     radioButtons('Q4', 'I want my outcome to be peer-reviewed',
                                                  c("any",
                                                    "yes",
                                                    "no")),
                                     radioButtons('Q5', 'I want my publication or dataset to appear immediately',
                                                  c("yes",
                                                    "no"), selected = "no"),

                                     helpText('If these options aren’t helpful, try our list of scenarios instead:'),
                                     actionButton('buttonToScenarios', 'Go to scenarios')
                                   )


                            ),
                            column(9,
                                   DT::dataTableOutput("table2"),

                                   br(),

                                   helpText('This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license,
                                            visit ',a(href = 'https://creativecommons.org/licenses/by-sa/3.0/', 'https://creativecommons.org/licenses/by-sa/3.0/')),
                                   helpText('Last update: 25.06.2019')
                            )
                          )
                 ),
                 tabPanel("Scenarios", value = "tabScenarios",
                          fluidRow(
                            column(width = 3,
                                   tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 106, width = 280),href="https://www.bihealth.org/quest-center/")
                            ),
                            column(width = 9,
                                   h1(HTML(" <img src=\"fiddle_logo.png\" width=\"111\" height=\"44\">  - <font color=\"#AA1C7D\"><b> fi</b></font>le
                                           <font color=\"#AA1C7D\"><b> d</b></font>rawer
                                           <font color=\"#AA1C7D\"><b> d</b></font>ata
                                           <font color=\"#AA1C7D\"><b> l</b></font>iberation
                                           <font color=\"#AA1C7D\"><b> e</b></font>ffort"), align = "center"),
                                   h4("Where and how to publish null / neutral results?", align = "center"),
                                   br(),
                                   wellPanel(HTML('<b>This “match-making” tool helps you to identify alternate ways of publishing information from well-designed experiments
                                                  that is often difficult to publish in traditional journals (i.e. null or neutral results, datasets, etc.).
                                                  Choose the criteria that are most relevant to you, or choose the scenario that best reflects your situation.
                                                  The tool will highlight the most suitable options.<br> Check out our explainer video <a href="https://youtu.be/TxUeXUecZIw">here</a>.</b>'),
                                             align = "center"),
                                   br()
                            )),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("Scenarios"),
                                     radioButtons('scenario', 'Why are your data in the file drawer?',
                                                 c("I don't have enough time to prepare a publication",
                                                   "My experiment or dataset is incomplete",
                                                   "I have data that may be useful to others, but am not able to analyze everything",
                                                   "I have neutral or null results from a small, underpowered study or an exploratory study",
                                                   "I have neutral or null results from a large, adequately powered study",
                                                   "My study is completed, but the findings aren't novel or exciting",
                                                   "I need the research to be published quickly",
                                                   "I don't have funding to pay for publication charges",
                                                   "None of these describe my situation - show me the table of all options"),
                                                 selected = "None of these describe my situation - show me the table of all options"),
                                    actionButton('buttonToOptions', 'Back to options')
                                   )
                            ),
                            column(9,
                                   DT::dataTableOutput("table"),

                                   br(),

                                   helpText('This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license,
                                            visit ',a(href = 'https://creativecommons.org/licenses/by-sa/3.0/', 'https://creativecommons.org/licenses/by-sa/3.0/')),
                                   helpText('Last update: 25.06.2019')
                            )
                          )
                 ),
                 tabPanel("About", value = "tabAbout",
                          fluidRow(
                            column(width = 3,
                                   tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 106, width = 280),href="https://www.bihealth.org/quest-center/")
                            ),
                            column(width = 9,
                                   h1(HTML(" <img src=\"fiddle_logo.png\" width=\"111\" height=\"44\">  - <font color=\"#AA1C7D\"><b> fi</b></font>le
                                           <font color=\"#AA1C7D\"><b> d</b></font>rawer
                                           <font color=\"#AA1C7D\"><b> d</b></font>ata
                                           <font color=\"#AA1C7D\"><b> l</b></font>iberation
                                           <font color=\"#AA1C7D\"><b> e</b></font>ffort"), align = "center"),
                                   h4("Where and how to publish null / neutral results?", align = "center"),
                                   br(),
                                   wellPanel(HTML('<b>This “match-making” tool helps you to identify alternate ways of publishing information from well-designed experiments
                                                  that is often difficult to publish in traditional journals (i.e. null or neutral results, datasets, etc.).
                                                  Choose the criteria that are most relevant to you, or choose the scenario that best reflects your situation.
                                                  The tool will highlight the most suitable options.<br> Check out our explainer video <a href="https://youtu.be/TxUeXUecZIw">here</a>.</b>'),
                                             align = "center"),
                                   br()
                            )),
                          fluidRow(
                            column(3,
                                   actionButton('buttonToOptions2', 'Back to options'),
                                   actionButton('buttonToScenarios2', 'Back to scenarios')
                            ),
                            column(9,
                                   h2("About"),
                                   br(),
                                   h4("Contributors"),
                                   helpText('Bernard, René (concept); Bobrov, Evgeny (concept); Riedel, Nico (concept, technical implementation); Weissgerber, Tracey (concept)'),
                                   br(),
                                   h4('Contact address'),
                                   helpText('fiddle@bihealth.de'),
                                   br(),
                                   h4("Publication"),
                                   helpText(a(href = 'https://osf.io/6mcu3/', 'https://osf.io/6mcu3/')),
                                   br(),
                                   h4('Source code'),
                                   helpText(a(href = 'https://github.com/quest-bih/FIDDLE', 'https://github.com/quest-bih/FIDDLE')),
                                   br(),
                                   h4('Research Resource Identifier (RRID) reference'),
                                   helpText('fiddle, RRID:SCR_017327'),
                                   br(),
                                   helpText('This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license,
                                            visit ',a(href = 'https://creativecommons.org/licenses/by-sa/3.0/', 'https://creativecommons.org/licenses/by-sa/3.0/')),
                                   helpText('Last update: 25.06.2019')
                            )
                          )
                 )#,
                 #tabPanel("Tutorial", value = "tabTutorial",
                 #         h2("Tutorial video"),
                 #         wellPanel(HTML("<video controls='controls' height='288' width='400'><source src='test.mp4' type='video/mp4' /></video>"))
                 #)
)


server <- function(input, output, session) {

  #--------------------------------------------------------------------------------------------------
  # Code for Table
  #--------------------------------------------------------------------------------------------------

  #options table
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


  #scenarios table
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


  #actionButton to switch tabs
  observeEvent(input$buttonToScenarios, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabScenarios")
  })

  observeEvent(input$buttonToOptions, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabOptions")
  })

  observeEvent(input$buttonToScenarios2, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabScenarios")
  })

  observeEvent(input$buttonToOptions2, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabOptions")
  })


  #write(paste0("App visit at: ", Sys.time()), "/var/log/shiny-server/visitors_fiddle.txt", append = TRUE)
}

shinyApp(ui, server)


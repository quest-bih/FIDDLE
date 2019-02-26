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

null_results_table <- read_delim("data/Table.csv", delim = ";")
null_results_table <- as.matrix(null_results_table)
rownames(null_results_table) <- null_results_table[,1]
null_results_table <- null_results_table[,-1]
#null_results_table <- as_tibble(cbind(nms = names(null_results_table), t(null_results_table)))

ui <- navbarPage("FIDDLE", theme = shinytheme("flatly"),
                 tabPanel("Table Select",
                          h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                          h4("Where and how to publish null / neutral results?", align = "center"),
                          br(),
                          h5("Some general explanation goes here..."),

                          br(),
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
                                                   "9. None of these describe my situation - show me the table of all options")),
                                     helpText('Some more explanation text...')
                                   )
                            ),
                            column(9,
                                   DT::dataTableOutput("table")
                            )
                          )
                 ),
                 tabPanel("Table Select 2",
                          h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                          h4("Where and how to publish null / neutral results?", align = "center"),
                          br(),
                          h5("Some general explanation goes here..."),

                          br(),
                          fluidRow(
                            column(5,
                                   wellPanel(
                                     h4("Why are your data in the file drawer?"),
                                     radioButtons('scenario_2', 'Scenarios',
                                                  c("1. I don't have enough time to prepare a publication",
                                                    "2. My experiment or dataset is incomplete",
                                                    "3. I have data that may be useful to others, but don't have time to analyze everything",
                                                    "4. I have neutral or null results from a small, underpowered study or an exploratory study",
                                                    "5. I have neutral or null results from a large, adequately powered study",
                                                    "6. My study is completed, but the findings aren't novel or exciting",
                                                    "7. I need the research to be published quickly",
                                                    "8. I don't have funding to pay for publication charges",
                                                    "9. None of these describe my situation - show me the table of all options")),
                                     helpText('Some more explanation text...')
                                   )
                            )),
                          fluidRow(
                            column(12,
                                   DT::dataTableOutput("table3")
                            )
                          )
                 ),
                 tabPanel("Table Select Reversed",
                          h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                          h4("Where and how to publish null / neutral results?", align = "center"),
                          br(),
                          h5("Some general explanation goes here..."),

                          br(),
                          fluidRow(
                            column(5,
                                   wellPanel(
                                     h4("Why are your data in the file drawer?"),
                                     radioButtons('scenario_rev', 'Scenarios',
                                                  c("1. I don't have enough time to prepare a publication",
                                                    "2. My experiment or dataset is incomplete",
                                                    "3. I have data that may be useful to others, but don't have time to analyze everything",
                                                    "4. I have neutral or null results from a small, underpowered study or an exploratory study",
                                                    "5. I have neutral or null results from a large, adequately powered study",
                                                    "6. My study is completed, but the findings aren't novel or exciting",
                                                    "7. I need the research to be published quickly",
                                                    "8. I don't have funding to pay for publication charges",
                                                    "9. None of these describe my situation - show me the table of all options")),
                                     helpText('Some more explanation text...')
                                   )
                            )),
                          fluidRow(
                            column(12,
                                   DT::dataTableOutput("table_rev")
                            )
                          )
                 ),
                 tabPanel("Table Options",
                          h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                          h4("Where and how to publish null / neutral results?", align = "center"),
                          br(),
                          h5("Some general explanation goes here..."),

                          br(),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("Scenarios"),
                                     radioButtons('Q1', 'I have a',
                                                  c("dataset only",
                                                    "partial study",
                                                    "full study",
                                                    "rejected manuscript")),
                                     radioButtons('Q2', 'Money I have to cover publication cost (in €)',
                                                 c("0",
                                                   "up to 500",
                                                   "up to 2000"), selected = "up to 2000"),
                                     radioButtons('Q3', 'I want the outcome to be indexed in',
                                                  c("Pubmed",
                                                    "Google scholar",
                                                    "Google"), selected = "Google"),
                                     radioButtons('Q4', 'I have my outcome to be peer-reviewed',
                                                  c("any",
                                                    "yes",
                                                    "no")),


                                     helpText('Some more explanation text...')
                                   )
                            ),
                            column(9,
                                   DT::dataTableOutput("table2")
                            )
                          )
                 ),
                 tabPanel("Cards Select",
                          h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                          h4("Where and how to publish null / neutral results?", align = "center"),
                          br(),
                          h5("Some general explanation goes here..."),

                          br(),
                          fluidRow(
                            column(5,
                                   wellPanel(
                                     h4("Scenarios"),
                                     radioButtons('scenario_cards', 'Scenarios',
                                                  c("1. I don't have enough time to prepare a publication",
                                                    "2. My experiment or dataset is incomplete",
                                                    "3. I have data that may be useful to others, but don't have time to analyze everything",
                                                    "4. I have neutral or null results from a small, underpowered study or an exploratory study",
                                                    "5. I have neutral or null results from a large, adequately powered study",
                                                    "6. My study is completed, but the findings aren't novel or exciting",
                                                    "7. I need the research to be published quickly",
                                                    "8. I don't have funding to pay for publication charges",
                                                    "9. None of these describe my situation - show me the table of all options")),
                                     helpText('Some more explanation text...')
                                   )
                            )),
                          uiOutput("cards_output")
                 ),
                 tabPanel("Cards Select 3",
                          h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                          h4("Where and how to publish null / neutral results?", align = "center"),
                          br(),
                          h5("Some general explanation goes here..."),

                          br(),
                          fluidRow(
                            column(5,
                                   wellPanel(
                                     h4("Scenarios"),
                                     radioButtons('scenario_cards_3', 'Scenarios',
                                                  c("1. I don't have enough time to prepare a publication",
                                                    "2. My experiment or dataset is incomplete",
                                                    "3. I have data that may be useful to others, but don't have time to analyze everything",
                                                    "4. I have neutral or null results from a small, underpowered study or an exploratory study",
                                                    "5. I have neutral or null results from a large, adequately powered study",
                                                    "6. My study is completed, but the findings aren't novel or exciting",
                                                    "7. I need the research to be published quickly",
                                                    "8. I don't have funding to pay for publication charges",
                                                    "9. None of these describe my situation - show me the table of all options")),
                                     helpText('Some more explanation text...')
                                   )
                            )),
                          uiOutput("cards_output_3")
                 ),
                 tabPanel("Cards Select 2",
                          h1("FIDDLE - Fi*le *D*rawer *D*ata *L*iberation *E*ffort", align = "center"),
                          h4("Where and how to publish null / neutral results?", align = "center"),
                          br(),
                          h5("Some general explanation goes here..."),

                          br(),
                          fluidRow(
                            column(5,
                                   wellPanel(
                                     h4("Scenarios"),
                                     radioButtons('scenario_cards_2', 'Scenarios',
                                                  c("1. I do not have enough time to prepare a publication",
                                                    "2. My experiment or dataset is incomplete",
                                                    "3",
                                                    "4",
                                                    "5",
                                                    "6",
                                                    "7",
                                                    "8",
                                                    "9")),
                                     helpText('Some more explanation text...')
                                   )
                            )),
                          fluidRow(
                            column(2,
                                   conditionalPanel(
                                     condition = "input.scenario_cards_2 == '1. I do not have enough time to prepare a publication'",
                                     wellPanel(
                                       h4("Data Repository only"),
                                     strong("How to"),
                                     p("Organize the data in a logical manner"),
                                     strong("Providers"),
                                     p("Zenodo, FigShare or Dryad; use re3data to search for disciplinary repositories"),
                                     strong("Effort"),
                                     p("low effort"),
                                     strong("Prize"),
                                     p("free of charge"),
                                     strong("Time to publication"),
                                     p("immediate")
                                     )
                                   )),
                            column(2,
                                   conditionalPanel(
                                     condition = "input.scenario_cards_2 == '2. My experiment or dataset is incomplete'",
                                     wellPanel(
                                       h4("Micropublication")
                                     )
                                   )),
                            column(2,
                                   wellPanel(
                                     h4("Preprint publication")
                                   )),
                            column(2,
                                   wellPanel(
                                     h4("Data journals")
                                   )),
                            column(2,
                                   wellPanel(
                                     h4("Publishing platform")
                                   )),
                            column(2,
                                   wellPanel(
                                     h4("Journal dedicated to null results")
                                   )),
                            column(2,
                                   wellPanel(
                                     h4("Journal open to null results")
                                   ))
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
                                      c(14)))
    ) %>%   formatStyle(
      set_chosen_columns(null_results_table, input$scenario),
      color = '#FFFFFF',
      backgroundColor = '#2C3E50'
    )

  })

  output$table3 <- DT::renderDataTable({
    DT::datatable(null_results_table,
                  options = list(
                    pageLength = 14,
                    lengthMenu = list(c(14),
                                      c(14)))
    ) %>%   formatStyle(
      set_chosen_columns(null_results_table, input$scenario_2),
      color = '#FFFFFF',
      backgroundColor = '#2C3E50'
    )

  })

  output$table_rev <- DT::renderDataTable({
    DT::datatable(as_tibble(cbind(nms = names(null_results_table), t(null_results_table))),
                  options = list(
                    pageLength = 14,
                    lengthMenu = list(c(14),
                                      c(14)))
    ) %>%   formatStyle(
      set_chosen_columns(as_tibble(cbind(nms = names(null_results_table), t(null_results_table))), input$scenario_rev),
      color = '#FFFFFF',
      backgroundColor = '#2C3E50'
    )

  })

  output$table2 <- DT::renderDataTable({
    DT::datatable(null_results_table,
                  options = list(
                    pageLength = 14,
                    lengthMenu = list(c(14),
                                      c(14)))
    ) %>%   formatStyle(
      set_chosen_columns_2(null_results_table, input$Q1, input$Q2, input$Q3, input$Q4),
      color = '#FFFFFF',
      backgroundColor = '#2C3E50'
    )

  })

  output$cards_output_2 <- renderUI(
    fluidRow(
      ifelse(1 %in% set_chosen_columns_num(null_results_table, input$scenario_cards), column(2,
             wellPanel(
               h4("Data Repository only")
             )), column(2, wellPanel(h4("NULL")))
             ),
      column(2,
             wellPanel(
               h4("Micropublication")
             )),
      column(2,
             wellPanel(
               h4("Preprint publication")
             )),
      column(2,
             wellPanel(
               h4("Data journals")
             )),
      column(2,
             wellPanel(
               h4("Publishing platform")
             )),
      column(2,
             wellPanel(
               h4("Journal dedicated to null results")
             )),
      column(2,
             wellPanel(
               h4("Journal open to null results")
             ))
    )
  )

  output$cards_output <- renderUI(
    fluidRow(
      column(2,
             wellPanel(style = ifelse(1 %in% set_chosen_columns_num(null_results_table, input$scenario_cards),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Data Repository only"),
                       strong("How to"),
                       p("Organize the data in a logical manner"),
                       strong("Providers"),
                       p("Zenodo, FigShare or Dryad; use re3data to search for disciplinary repositories"),
                       strong("Effort"),
                       p("low effort"),
                       strong("Prize"),
                       p("free of charge"),
                       strong("Time to publication"),
                       p("immediate"),
                       strong("Recognition"),
                       p("citations of the dataset"),
                       strong("Publishing venue can have Impact Factor"),
                       p("no"),
                       strong("Peer-review"),
                       p("no"),
                       strong("Indexing in Pubmed"),
                       p("no"),
                       strong("Findable with Google scholar"),
                       p("no"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p("data a immediately available and archived "),
                       strong("Disadvantages"),
                       p("no scholarly publication")
             )
      ),
      column(2,
             wellPanel(style = ifelse(2 %in% set_chosen_columns_num(null_results_table, input$scenario_cards),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Micropublication"),
                       strong("How to"),
                       p("Manuscript format is informal and up to author; in general like short communication"),
                       strong("Providers"),
                       p("ScienceMatters, BMC Research Notes, microPublication Biology"),
                       strong("Effort"),
                       p("low effort"),
                       strong("Prize"),
                       p("595$ (Science Matters), 0€ (µP Biology)"),
                       strong("Time to publication"),
                       p("typically 1-3 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list (future handling of such articles is open)"),
                       strong("Publishing venue can have Impact Factor"),
                       p("yes"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("no (but might be soon the case for some journals like Science Matters)"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p("Aimed for unpublished observations, negative/neutral results"),
                       strong("Disadvantages"),
                       p("Flexibility in format and length might undercut the credibility of findings")
             )
      ),
      column(2,
             wellPanel(style = ifelse(3 %in% set_chosen_columns_num(null_results_table, input$scenario_cards),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Preprint publication"),

                       strong("How to"),
                       p("1. Prepare manuscript in normal manner as you would submit it for journal publication
                 2. Choose a fitting preprint server and submit under a fitting discipline category"),
                       strong("Providers"),
                       p("biorxiv,  ASAPBio, osf.io "),
                       strong("Effort"),
                       p("medium effort"),
                       strong("Costs"),
                       p("free of charge"),
                       strong("Time to publication"),
                       p("immediate"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list (not universally accepted at this point)"),
                       strong("Publishing venue can have Impact Factor"),
                       p("no"),
                       strong("Peer-review"),
                       p("post-publication review possible"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p("research is better described and accessible than a data deposit only;
                 preprints are licensed, findable, sharable, commentable, and citable;
                 preprint can be submitted to peer review later; free of charge"),
                       strong("Disadvantages"),
                       p(""),
                       strong("Best practice example"),
                       p('finding which confirm the hypothesis, but need to be published quickly;
                 can be used if there is a reason to fear scooping; also a way to receive feedback
                 prior to a regular paper submission')
             )
      ),
      column(2,
             wellPanel(style = ifelse(4 %in% set_chosen_columns_num(null_results_table, input$scenario_cards),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Data journals"),
                       strong("How to"),
                       p('1. Prepare manuscript in normal manner, except for a discussion section
                 2. Proceed with data similarly to "data repository" section'),
                       strong("Providers"),
                       p("Scientific Data, Data, Data in Brief; many disciplinary journals
                 (e.g. Genomics Data, GIgaScience, Biomedical Data Journal)"),
                       strong("Effort"),
                       p("some effort to prepare manuscript/data"),
                       strong("Costs"),
                       p("Scientific Data ca. 1500€ (1305€ + VAT)"),
                       strong("Time to publication"),
                       p("typically 1-4 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list"),
                       strong("Publishing venue can have Impact Factor"),
                       p("yes"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p(""),
                       strong("Disadvantages"),
                       p(""),
                       strong("Best practice example"),
                       p('datasets for which analysis is not possible (anymore), or was not planned right from the start')
             )
      ),
      column(2,
             wellPanel(style = ifelse(5 %in% set_chosen_columns_num(null_results_table, input$scenario_cards),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Publishing platform"),
                       strong("How to"),
                       p('Prepare manuscript in normal manner'),
                       strong("Providers"),
                       p("F1000Research"),
                       strong("Effort"),
                       p("some effort to prepare manuscript/data"),
                       strong("Costs"),
                       p("150-1000$ (F1000); article processing charges are nearly universal"),
                       strong("Time to publication"),
                       p("typically 1-4 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list (not universally accepted at this point)"),
                       strong("Publishing venue can have Impact Factor"),
                       p("no"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p(""),
                       strong("Disadvantages"),
                       p("Since plattforms are not journals - they have no Journal Impact Factor"),
                       strong("Best practice example"),
                       p('finding which confirm the hypothesis, but need to be published quickly; can be used if there is a reason to fear scooping')
             )
      ),
      column(2,
             wellPanel(style = ifelse(6 %in% set_chosen_columns_num(null_results_table, input$scenario_cards),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Journal dedicated to null results"),
                       strong("How to"),
                       p('Prepare manuscript in normal manner'),
                       strong("Providers"),
                       p("Clinical Neurophysiology Practice, Journal of Negative and No Positive Results, The All Results Journal"),
                       strong("Effort"),
                       p("some effort to prepare manuscript/data"),
                       strong("Costs"),
                       p("1500€ (Clin Neurophys Practice)"),
                       strong("Time to publication"),
                       p("typically 1-6 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list"),
                       strong("Publishing venue can have Impact Factor"),
                       p("yes"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p(""),
                       strong("Disadvantages"),
                       p(""),
                       strong("Best practice example"),
                       p('')
             )
      ),
      column(2,
             wellPanel(style = ifelse(7 %in% set_chosen_columns_num(null_results_table, input$scenario_cards),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Journal open to null results"),
                       strong("How to"),
                       p('1. check Journal website /editorial criteria whether negative results are acceptable
                 2. Prepare manuscript in normal manner'),
                       strong("Providers"),
                       p("PeerJ, PLoS One; multiple BMC journals and many other disciplinary journals"),
                       strong("Effort"),
                       p("some effort to prepare manuscript/data"),
                       strong("Costs"),
                       p("1595$ (PLoS One); article processing charges are nearly universal"),
                       strong("Time to publication"),
                       p("typically 1-6 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list"),
                       strong("Publishing venue can have Impact Factor"),
                       p("yes"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p(""),
                       strong("Disadvantages"),
                       p(""),
                       strong("Best practice example"),
                       p('mixed finding, with some being positive, and some not')
             )
      )

    )
  )

  card_head <- function(heading, card_num)
  {
    return(wellPanel(style = ifelse(card_num %in% set_chosen_columns_num(null_results_table, input$scenario_cards_3),
                                    "background-color: #2C3E50; color: white",
                                    "background-color: #ECF0F1; color: black"),
                     h4(heading)))
  }

  card <- function(heading, main_text, card_num)
  {
    return(wellPanel(style = ifelse(card_num %in% set_chosen_columns_num(null_results_table, input$scenario_cards_3),
                                    "background-color: #2C3E50; color: white",
                                    "background-color: #ECF0F1; color: black"),
                     strong(heading),
                     p(main_text)))
  }

  output$cards_output_3 <- renderUI(
    fluidRow(
      column(2,
             card_head("Data Repository only", 1),
             card("How to",
                  "Organize the data in a logical manner", 1),
             card("Providers",
                  "Zenodo, FigShare or Dryad; use re3data to search for disciplinary repositories", 1),
             card("Effort",
                  "low effort", 1),
             card("Prize",
                  "free of charge", 1),
             card("Time to publication",
                  "immediate", 1),
             card("Recognition",
                  "citations of the dataset", 1),
             card("Publishing venue can have Impact Factor",
                  "no", 1),
             card("Peer-review",
                  "no", 1),
             card("Indexing in Pubmed",
                  "no", 1),
             card("Findable with Google scholar",
                  "no", 1),
             card("DOI",
                  "yes", 1),
             card("Advantages",
                  "data a immediately available and archived", 1),
             card("Disadvantages",
                  "no scholarly publication", 1)
      ),
    column(2,
             wellPanel(style = ifelse(2 %in% set_chosen_columns_num(null_results_table, input$scenario_cards_3),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Micropublication"),
                       strong("How to"),
                       p("Manuscript format is informal and up to author; in general like short communication"),
                       strong("Providers"),
                       p("ScienceMatters, BMC Research Notes, microPublication Biology"),
                       strong("Effort"),
                       p("low effort"),
                       strong("Prize"),
                       p("595$ (Science Matters), 0€ (µP Biology)"),
                       strong("Time to publication"),
                       p("typically 1-3 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list (future handling of such articles is open)"),
                       strong("Publishing venue can have Impact Factor"),
                       p("yes"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("no (but might be soon the case for some journals like Science Matters)"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p("Aimed for unpublished observations, negative/neutral results"),
                       strong("Disadvantages"),
                       p("Flexibility in format and length might undercut the credibility of findings")
             )
      ),
      column(2,
             wellPanel(style = ifelse(3 %in% set_chosen_columns_num(null_results_table, input$scenario_cards_3),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Preprint publication"),

                       strong("How to"),
                       p("1. Prepare manuscript in normal manner as you would submit it for journal publication
                         2. Choose a fitting preprint server and submit under a fitting discipline category"),
                       strong("Providers"),
                       p("biorxiv,  ASAPBio, osf.io "),
                       strong("Effort"),
                       p("medium effort"),
                       strong("Costs"),
                       p("free of charge"),
                       strong("Time to publication"),
                       p("immediate"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list (not universally accepted at this point)"),
                       strong("Publishing venue can have Impact Factor"),
                       p("no"),
                       strong("Peer-review"),
                       p("post-publication review possible"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p("research is better described and accessible than a data deposit only;
                         preprints are licensed, findable, sharable, commentable, and citable;
                         preprint can be submitted to peer review later; free of charge"),
                       strong("Disadvantages"),
                       p(""),
                       strong("Best practice example"),
                       p('finding which confirm the hypothesis, but need to be published quickly;
                         can be used if there is a reason to fear scooping; also a way to receive feedback
                         prior to a regular paper submission')
             )
      ),
      column(2,
             wellPanel(style = ifelse(4 %in% set_chosen_columns_num(null_results_table, input$scenario_cards_3),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Data journals"),
                       strong("How to"),
                       p('1. Prepare manuscript in normal manner, except for a discussion section
                         2. Proceed with data similarly to "data repository" section'),
                       strong("Providers"),
                       p("Scientific Data, Data, Data in Brief; many disciplinary journals
                         (e.g. Genomics Data, GIgaScience, Biomedical Data Journal)"),
                       strong("Effort"),
                       p("some effort to prepare manuscript/data"),
                       strong("Costs"),
                       p("Scientific Data ca. 1500€ (1305€ + VAT)"),
                       strong("Time to publication"),
                       p("typically 1-4 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list"),
                       strong("Publishing venue can have Impact Factor"),
                       p("yes"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p(""),
                       strong("Disadvantages"),
                       p(""),
                       strong("Best practice example"),
                       p('datasets for which analysis is not possible (anymore), or was not planned right from the start')
             )
      ),
      column(2,
             wellPanel(style = ifelse(5 %in% set_chosen_columns_num(null_results_table, input$scenario_cards_3),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Publishing platform"),
                       strong("How to"),
                       p('Prepare manuscript in normal manner'),
                       strong("Providers"),
                       p("F1000Research"),
                       strong("Effort"),
                       p("some effort to prepare manuscript/data"),
                       strong("Costs"),
                       p("150-1000$ (F1000); article processing charges are nearly universal"),
                       strong("Time to publication"),
                       p("typically 1-4 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list (not universally accepted at this point)"),
                       strong("Publishing venue can have Impact Factor"),
                       p("no"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p(""),
                       strong("Disadvantages"),
                       p("Since plattforms are not journals - they have no Journal Impact Factor"),
                       strong("Best practice example"),
                       p('finding which confirm the hypothesis, but need to be published quickly; can be used if there is a reason to fear scooping')
             )
      ),
      column(2,
             wellPanel(style = ifelse(6 %in% set_chosen_columns_num(null_results_table, input$scenario_cards_3),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Journal dedicated to null results"),
                       strong("How to"),
                       p('Prepare manuscript in normal manner'),
                       strong("Providers"),
                       p("Clinical Neurophysiology Practice, Journal of Negative and No Positive Results, The All Results Journal"),
                       strong("Effort"),
                       p("some effort to prepare manuscript/data"),
                       strong("Costs"),
                       p("1500€ (Clin Neurophys Practice)"),
                       strong("Time to publication"),
                       p("typically 1-6 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list"),
                       strong("Publishing venue can have Impact Factor"),
                       p("yes"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p(""),
                       strong("Disadvantages"),
                       p(""),
                       strong("Best practice example"),
                       p('')
             )
      ),
      column(2,
             wellPanel(style = ifelse(7 %in% set_chosen_columns_num(null_results_table, input$scenario_cards_3),
                                      "background-color: #2C3E50; color: white",
                                      "background-color: #ECF0F1; color: black"),
                       h4("Journal open to null results"),
                       strong("How to"),
                       p('1. check Journal website /editorial criteria whether negative results are acceptable
                         2. Prepare manuscript in normal manner'),
                       strong("Providers"),
                       p("PeerJ, PLoS One; multiple BMC journals and many other disciplinary journals"),
                       strong("Effort"),
                       p("some effort to prepare manuscript/data"),
                       strong("Costs"),
                       p("1595$ (PLoS One); article processing charges are nearly universal"),
                       strong("Time to publication"),
                       p("typically 1-6 months"),
                       strong("Recognition"),
                       p("citations of article  + article in publication list"),
                       strong("Publishing venue can have Impact Factor"),
                       p("yes"),
                       strong("Peer-review"),
                       p("yes"),
                       strong("Indexing in Pubmed"),
                       p("yes"),
                       strong("Findable with Google scholar"),
                       p("yes"),
                       strong("DOI"),
                       p("yes"),
                       strong("Advantages"),
                       p(""),
                       strong("Disadvantages"),
                       p(""),
                       strong("Best practice example"),
                       p('mixed finding, with some being positive, and some not')
             )
      )

    )
  )

}

shinyApp(ui, server)


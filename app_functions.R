#functions used to select the right publication venues for the given options/scenarios

#set up table with right highlighting for the chosen options
set_chosen_columns <- function(table, option)
{
  tab_col <- colnames(table)

  col_chosen = NULL
  switch(option,
         "I don't have enough time to prepare a publication" = {col_chosen = c(tab_col[1], tab_col[2])},
         "My experiment or dataset is incomplete" = {col_chosen = c(tab_col[1], tab_col[2])},
         "I have data that may be useful to others, but am not able to analyze everything" = {col_chosen = c(tab_col[1], tab_col[2], tab_col[4])},
         "I have neutral or null results from a small, underpowered study or an exploratory study" = {col_chosen = c(tab_col[2], tab_col[3])},
         "I have neutral or null results from a large, adequately powered study" = {col_chosen = c(tab_col[5], tab_col[6])},
         "My study is completed, but the findings aren't novel or exciting" = {col_chosen = c(tab_col[1], tab_col[3], tab_col[4], tab_col[5], tab_col[6])},
         "I need the research to be published quickly" = {col_chosen = c(tab_col[1], tab_col[3])},
         "I don't have funding to pay for publication charges" = {col_chosen = c(tab_col[1], tab_col[3])},
         "None of these describe my situation - show me the table of all options"  = {col_chosen = NULL}
  )

  return(col_chosen)
}

set_chosen_columns_num <- function(table, option)
{
  tab_col <- colnames(table)

  col_chosen = NULL
  switch(option,
         "I don't have enough time to prepare a publication" = {col_chosen = c(1, 2)},
         "My experiment or dataset is incomplete" = {col_chosen = 2},
         "I have data that may be useful to others, but am not able to analyze everything" = {col_chosen = c(1, 2, 4)},
         "I have neutral or null results from a small, underpowered study or an exploratory study" = {col_chosen = c(2, 3)},
         "I have neutral or null results from a large, adequately powered study" = {col_chosen = c(5, 6)},
         "My study is completed, but the findings aren't novel or exciting" = {col_chosen = c(1, 3, 4, 5, 6)},
         "I need the research to be published quickly" = {col_chosen = c(1, 3)},
         "I don't have funding to pay for publication charges" = {col_chosen = c(1, 2, 3)},
         "None of these describe my situation - show me the table of all options"  = {col_chosen = NULL}
  )

  return(col_chosen)
}

#map chosen options to publication venues
set_chosen_columns_2 <- function(table, Q1, Q2, Q3, Q4, Q5)
{
  tab_col <- colnames(table)

  Q1_sel = NULL
  switch(Q1,
         "unanalyzed dataset" = {Q1_sel = c(1, 4)},
         "stand-alone finding or results from a small study" = {Q1_sel = c(2)},
         "results from a full-scale study" = {Q1_sel = c(3, 5, 6)},
         "rejected manuscript" = {Q1_sel = c(2, 3, 5, 6)}
  )

  Q2_sel = NULL
  switch(Q2,
         "0" = {Q2_sel = c(1, 3)},
         "up to 1000" = {Q2_sel = c(1, 2, 3, 5)},
         "up to 2000" = {Q2_sel = c(1, 2, 3, 4, 5, 6)}
  )

  Q3_sel = NULL
  if("Pubmed" %in% Q3) {
    Q3_sel = c(3, 4, 5, 6)
  } else if("Google scholar" %in% Q3) {
    Q3_sel = c(2, 3, 4, 5, 6)
  } else {
    Q3_sel = c(1, 2, 3, 4, 5, 6)
  }

  Q4_sel = NULL
  switch(Q4,
         "any" = {Q4_sel = c(1, 2, 3, 4, 5, 6)},
         "yes" = {Q4_sel = c(2, 4, 5, 6)},
         "no" = {Q4_sel = c(1, 3)}
  )

  Q5_sel = NULL
  switch(Q5,
         "yes" = {Q5_sel = c(1, 3, 5)},
         "no" = {Q5_sel = c(1, 2, 3, 4, 5, 6)}
  )

  sel_combined <- Q1_sel %>%
    intersect(Q2_sel) %>%
    intersect(Q3_sel) %>%
    intersect(Q4_sel) %>%
    intersect(Q5_sel)

  col_chosen = map_chr(sel_combined, function(x) tab_col[x])
  #col_chosen = c(tab_col[1], tab_col[2], tab_col[3])

  return(col_chosen)
}

#sets highlighting colors for cards
set_card_col <- function(table, option, card_num)
{
  chosen_col = set_chosen_columns_num(table, option)

  if(card_num %in% chosen_col) {
    card_col = "background-color: #2C3E50; color: white"
  } else {
    card_col = "background-color: #ECF0F1; color: black"
  }

    return(card_col)
}





#set up table with right highlighting for the chosen options
set_chosen_columns <- function(table, option)
{
  tab_col <- colnames(table)

  col_chosen = NULL
  switch(option,
         "1. I don't have enough time to prepare a publication" = {col_chosen = c(tab_col[1], tab_col[2])},
         "2. My experiment or dataset is incomplete" = {col_chosen = tab_col[2]},
         "3. I have data that may be useful to others, but don't have time to analyze everything" = {col_chosen = c(tab_col[1], tab_col[2], tab_col[4])},
         "4. I have neutral or null results from a small, underpowered study or an exploratory study" = {col_chosen = c(tab_col[2], tab_col[3])},
         "5. I have neutral or null results from a large, adequately powered study" = {col_chosen = c(tab_col[5], tab_col[6], tab_col[7])},
         "6. My study is completed, but the findings aren't novel or exciting" = {col_chosen = c(tab_col[1], tab_col[3], tab_col[4], tab_col[5])},
         "7. I need the research to be published quickly" = {col_chosen = c(tab_col[1], tab_col[3])},
         "8. I don't have funding to pay for publication charges" = {col_chosen = c(tab_col[1], tab_col[2], tab_col[3])},
         "9. None of these describe my situation - show me the table of all options"  = {col_chosen = NULL}
  )

  return(col_chosen)
}

set_chosen_columns_num <- function(table, option)
{
  tab_col <- colnames(table)

  col_chosen = NULL
  switch(option,
         "1. I don't have enough time to prepare a publication" = {col_chosen = c(1, 2)},
         "2. My experiment or dataset is incomplete" = {col_chosen = 2},
         "3. I have data that may be useful to others, but don't have time to analyze everything" = {col_chosen = c(1, 2, 4)},
         "4. I have neutral or null results from a small, underpowered study or an exploratory study" = {col_chosen = c(2, 3)},
         "5. I have neutral or null results from a large, adequately powered study" = {col_chosen = c(5, 6, 7)},
         "6. My study is completed, but the findings aren't novel or exciting" = {col_chosen = c(1, 3, 4, 5)},
         "7. I need the research to be published quickly" = {col_chosen = c(1, 3)},
         "8. I don't have funding to pay for publication charges" = {col_chosen = c(1, 2, 3)},
         "9. None of these describe my situation - show me the table of all options"  = {col_chosen = NULL}
  )

  return(col_chosen)
}

set_chosen_columns_2 <- function(table, Q1, Q2, Q3, Q4)
{
  tab_col <- colnames(table)

  Q1_sel = NULL
  switch(Q1,
         "dataset only" = {Q1_sel = c(1, 4)},
         "partial study" = {Q1_sel = c(2)},
         "full study" = {Q1_sel = c(3, 5, 6, 7)},
         "rejected manuscript" = {Q1_sel = c(2, 3, 5, 6, 7)}
  )

  Q2_sel = NULL
  switch(Q2,
         "0" = {Q2_sel = c(1, 2, 3)},
         "up to 500" = {Q2_sel = c(1, 2, 3, 5)},
         "up to 2000" = {Q2_sel = c(1, 2, 3, 4, 5, 6, 7)}
  )

  Q3_sel = NULL
  switch(Q3,
         "Pubmed" = {Q3_sel = c(3, 4, 5, 6, 7)},
         "Google scholar" = {Q3_sel = c(2, 3, 4, 5, 6, 7)},
         "Google" = {Q3_sel = c(1, 2, 3, 4, 5, 6, 7)}
  )

  Q4_sel = NULL
  switch(Q4,
         "any" = {Q4_sel = c(1, 2, 3, 4, 5, 6, 7)},
         "yes" = {Q4_sel = c(2, 4, 5, 6, 7)},
         "no" = {Q4_sel = c(1, 3)}
  )

  sel_combined <- Q1_sel %>% intersect(Q2_sel) %>% intersect(Q3_sel) %>% intersect(Q4_sel)

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




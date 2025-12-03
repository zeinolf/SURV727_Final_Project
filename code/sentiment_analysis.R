#i dont use all of these but for ease of running i've just copy/pasted
library(tidyverse)
library(dplyr)
library(purrr)
library(xml2)
library(httr)
library(httr2)
library(readr)
library(stringr)
library(huggingfaceR)
library(DBI)
library(RSQLite)
library(scales)
library(ggplot2)
library(fastDummies)


#setting up the huggingface model for sentiment analysis
Sys.setenv(TOKENIZERS_PARALLELISM = "false")
emotion_distilroBERTa <- hf_load_pipeline(
  model_id = "j-hartmann/emotion-english-distilroberta-base",
  task     = "text-classification"
)

#setting up a function to run the sentiment analysis
score_emotions_one <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) {
    return(tibble(label = NA_character_, score = NA_real_))
  }
  txt_use <- substr(txt, 1, 1000)
  out <- emotion_distilroBERTa(txt_use, return_all_scores = TRUE)
  
  
  if (is.list(out) && length(out) > 0 && !is.null(out[[1]]$label)) {
    out_list <- out
  } else if (is.list(out) && length(out) > 0 && is.list(out[[1]]) && !is.null(out[[1]][[1]]$label)) {
    out_list <- out[[1]]
  } else {
    return(tibble(label = NA_character_, score = NA_real_))
  }
  
  labels <- sapply(out_list, `[[`, "label")
  scores <- sapply(out_list, `[[`, "score")
  
  tibble(
    label = labels,
    score = scores
  )
}

#reading in nigeria news data
con <- dbConnect(SQLite(), "./data/nigeria_news.db")
article_data <- dbReadTable(con, "nigeria_news")
dbDisconnect(con)

#adding doc_id
article_data_id <- article_data |>
  mutate(doc_id = row_number())

#running sentiment analysis
emotion_long <- map2_dfr(
  article_data_id$text,
  article_data_id$doc_id,
  ~ {df <- score_emotions_one(.x)
  df$doc_id <- .y
  df
  }
) |>
  relocate(doc_id, label, score)|>
  left_join(
    article_data_id |> 
      select(doc_id, pub_date_raw, source, title), by = "doc_id"
  )


con <- dbConnect(SQLite(), "./data/emotion_long.db")
dbWriteTable(con, "emotion_long", emotion_long, overwrite = TRUE)
dbDisconnect(con)
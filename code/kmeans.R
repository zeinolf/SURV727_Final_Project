#i dont use all of these but for ease of running i've just copy/pasted
library(rvest)
library(tidyverse)
library(dplyr)
library(purrr)
library(xml2)
library(httr)
library(httr2)
library(acled.api)
library(readr)
library(stringr)
library(huggingfaceR)
library(DBI)
library(RSQLite)
library(scales)
library(ggplot2)
library(fastDummies)

#connecting to acled data db
con <- dbConnect(SQLite(), "./data/ng_acled.db")
ng_acled <- dbReadTable(con, "ng_acled")
dbDisconnect(con)

#doing some filtering
test<- ng_acled %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  select(disorder_type, event_type, fatalities) %>%
  mutate(
    fatalities = ifelse(is.na(fatalities), 0, fatalities),
    disorder_type = as.numeric(as.factor(disorder_type)),
    event_type = as.numeric(as.factor(event_type))
  )|>
  scale()

#elbow plot
fviz_nbclust(test, kmeans, method = "wss")

#kmeans using k=3
set.seed(100)
k_final <- kmeans(test, centers = 3, nstart = 25)

#adding clusters back to original data
acled_clustered <- ng_acled |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  mutate(
    cluster   = factor(k_final$cluster),
    disorder_type = as.factor(disorder_type),
    event_type    = as.factor(event_type),
    fatalities    = ifelse(is.na(fatalities), 0, fatalities),
    actor1 = str_to_lower(actor1),
    actor2 = str_to_lower(actor2),
    boko_flag = case_when(
      str_detect(actor1, "boko haram") | str_detect(actor2, "boko haram") ~ "Boko Haram",
      TRUE ~ "Other"
    )
  )

#saving to db
con <- dbConnect(SQLite(), "./data/acled_clustered.db")
dbWriteTable(con, "acled_clustered", acled_clustered, overwrite = TRUE)
dbDisconnect(con)

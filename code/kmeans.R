library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(factoextra)

#connecting to acled data db
con <- dbConnect(SQLite(), "./data/ng_acled.db")
ng_acled <- dbReadTable(con, "ng_acled")
dbDisconnect(con)


#filtering data to sub set
acled_clean <- ng_acled %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(
    fatalities    = ifelse(is.na(fatalities), 0, fatalities),
    disorder_type = as.factor(disorder_type),
    event_type    = as.factor(event_type)
  )

#building a matrix to do k-means
X_cat <- model.matrix(~ disorder_type + event_type - 1, data = acled_clean)
X <- cbind(fatalities = acled_clean$fatalities, X_cat)
#scaling
X_scaled <- scale(X)

#elbow plot
fviz_nbclust(X_scaled, kmeans, method = "wss")

#chose 7, running k-means
set.seed(100)
k_final <- kmeans(X_scaled, centers = 7, nstart = 25)

#adding clusters back to data
acled_clustered <- acled_clean %>%
  mutate(
    cluster    = factor(k_final$cluster),
    actor1     = str_to_lower(actor1),
    actor2     = str_to_lower(actor2),
    boko_flag  = case_when(
      str_detect(actor1, "boko haram") | str_detect(actor2, "boko haram") ~ "Boko Haram",
      TRUE ~ "Other"
    )
  )

#saving to db
con <- dbConnect(SQLite(), "./data/acled_clustered.db")
dbWriteTable(con, "acled_clustered", acled_clustered, overwrite = TRUE)
dbDisconnect(con)
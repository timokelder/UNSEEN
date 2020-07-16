## code to prepare `DATASET` dataset goes here

#The data is loaded from the ESOWC workflow. This open a dataframe with date as character. We convert this to Date format.
EOBS_UK_weighted_df <- read.csv("../UNSEEN-open/Data/EOBS_UK_weighted_upscaled.csv", stringsAsFactors=FALSE)
SEAS5_UK_weighted_df <- read.csv("../UNSEEN-open//Data/SEAS5_UK_weighted_masked.csv", stringsAsFactors=FALSE)

EOBS_UK_weighted_df$time <- lubridate::ymd(EOBS_UK_weighted_df$time)
str(EOBS_UK_weighted_df)

SEAS5_UK_weighted_df$time <- lubridate::ym(SEAS5_UK_weighted_df$time)
str(SEAS5_UK_weighted_df)

usethis::use_data(DATASET, overwrite = TRUE)

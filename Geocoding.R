install.packages("tidygeocoder")
install.packages("sf")
install.packages("spdep")

library(tidygeocoder)
library(sf)
library(tidyverse)
library(dplyr)
library(spdep)

## working directory
setwd("C:/Users/sydne/OneDrive/Desktop/School/DAT 490/Data/")

## data
counties = st_read("tl_2023_us_county.shp")
treatment = read.csv("Treatment_Facilities.csv")

## remove treatment centers without addresses
treatment <- treatment %>%
  mutate(across(c(Street1, City, State, Zip),
                ~ ifelse(str_trim(.x) %in% c("", "- - -", "--", "---"), NA, .x)))
treatment <- treatment %>%
  filter(!is.na(Street1), !is.na(City), !is.na(State), !is.na(Zip),
         Street1 != "", City != "", State != "", Zip != "")

## filter out na
treatment = treatment %>%
  filter(
    !is.na(Street1),
    !is.na(City),
    !is.na(State),
    !is.na(Zip))

## full address for geocoding
treatment = treatment %>%
  mutate(full_address = paste(Street1, City, State, Zip, sep = ", "))

## batch for geocoding
batch_size = 5000
batches = split(treatment, ceiling(seq_len(nrow(treatment)) / batch_size))
geocoded = list()

## geocode using census 
for (i in seq_along(batches)) {
  cat("Processing batch", i, "of", length(batches), "\n")
  
  batch = batches[[i]]
  
  result = tryCatch({
    geocode(.tbl = batch,
            address = full_address,
            method = "census",
            lat = latitude,
            long = longitude,
            verbose = TRUE)
  }, error = function(e) {
    message("Batch ", i, " failed: ", e$message)
    return(NULL)
  })
  
  if (!is.null(result)) {
    geocoded[[i]] <- result
  }
}

## merge into new df
treatment_geocoded = bind_rows(geocoded)

## save geocoded df 
write.csv(treatment_geocoded, "treatment_geocoded.csv")
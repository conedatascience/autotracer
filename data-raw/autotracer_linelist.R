# Purpose: Generate Simulated Data to Mimic EMR Record Data
# packages ----------------------------------------------------------------

library(wakefield)
library(dplyr)

# setup -------------------------------------------------------------------
set.seed(336)

# generate fake data ------------------------------------------------------
# Using Wakefield, generate fake patient population
df <- r_data_frame(
  n = 10500,
  x = rnorm,
  y = rnorm,
  race,
  id,
  age,
  sex,
  language
) %>%
  mutate(Age10 = case_when(Age < 10~"0-9",
                           Age < 20 ~"10-19",
                           Age < 30 ~"20-29",
                           Age < 40 ~"30-39",
                           Age < 50 ~"40-49",
                           Age < 60 ~"50-59",
                           Age < 70 ~"60-69",
                           Age < 80 ~"70-79",
                           TRUE ~"80+"
  )) %>%
  mutate(Age10 = factor(Age10, c("0-9", "10-19", "20-29", "30-39",
                                 "40-49", "50-59", "60-69", "70-79", "80+")))

# Add generic test dates
df <- dplyr::rename(df, patient_id = ID)
df$date <- sample(seq.Date(as.Date("2020-06-01"),
                           as.Date("2020-07-01"), by = 1),
                  size = 10500, replace = T)

# write output ------------------------------------------------------------
autotracer_linelist <- df

usethis::use_data(autotracer_linelist, overwrite = TRUE)

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
)

# Add generic test dates
df <- dplyr::rename(df, patient_id = ID)
df$date <- sample(seq.Date(as.Date("2020-06-01"),
                           as.Date("2020-07-01"), by = 1),
                  size = 10500, replace = T)

# write output ------------------------------------------------------------
autotracer_linelist <- df

usethis::use_data(autotracer_linelist, overwrite = TRUE)

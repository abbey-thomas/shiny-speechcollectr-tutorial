# Load necessary packages----
# Note: download the latest version of speechcollectr using the following command:
# install.packages("devtools")
devtools::install_github("abbey-thomas/speechcollectr")
library(dplyr)
library(tidyr)
library(tuneR)
library(shiny)
library(speechcollectr)

# Set working directory----
setwd("perc-exp")

# Make the necessary sub-directories----
# BUT ONLY RUN THIS CHUNK ONCE!!!!
# dir.create("www")
# dir.create("www/outputs")

# Demographic survey prep----
# Get the basic demographic survey data frame from speechcollectr
data("demographics")
# If desired, view this table of survey questions using the following:
# View(demographics)

# Create another vector with the same names as the column in the demographics file
# To ask participants a question about their native language
# Note that since this is a free-form text response (as denoted by `type = 'textInput'`),
# We do not need to add an answer options (hence options = NA),
# but we still need to include the options element in the vector
# to ensure it joins properly to the table of other questions in "demographics"
demog_new <- data.frame(id = c("nat_lang", "nat_lang2"),
                        priority = c("required", "optional"),
                        label = c("Is English your native language?",
                                  "What is your native language?"),
                        type = c("radioButtons", "textInput"),
                        options = c("yes,no", NA),
                        trigger_id = c(NA, "nat_lang"),
                        trigger_value = c(NA, "no"))

# Bind the native language to the rest of the demographic survey
demographics <- bind_rows(demographics, demog_new)

# Save the complete survey
write.csv(demographics, "www/demographics.csv", row.names = FALSE)


# Check the survey to make sure everything is formatted properly
feedback <- surveyPrep(questionFile = "www/demographics.csv",
                       notListedLab = "Not listed:")

# Perception Experiment prep----

## Perception stimuli----
wwwPrep("perc_stim", is_dir = TRUE)

## Get the duration of each audio recording----
file_dur <- lapply(list.files(path = "www/perc_stim", full.names = TRUE), function(i){
  w <- readWave(i, header = TRUE)
  dur <- ceiling((w$samples/w$sample.rate)*1000)
  return(dur)
})

## Make a data frame for storing filenames and durations----
stim_df <- data.frame(filename = list.files(path = "www/perc_stim")) %>%
  mutate(emotion = ifelse(grepl("\\dn", filename), "neutral",
                          ifelse(grepl("\\ds", filename), "sad",
                                 ifelse(grepl("\\dh", filename), "happy",
                                        "angry"))),
         duration = unlist(file_dur)) %>%
  slice(c(rep(1, times = 2), 2:nrow(.)))

## Save the stimulus data frame to a file----
write.csv(stim_df, "www/stimuli.csv", row.names = FALSE)




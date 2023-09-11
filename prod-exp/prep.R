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
setwd("prod-exp")

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

# Production Experiment prep----

## Build stimulus set----
# Emotionally neutral words used by Kim & Sumner (2017) go in one column,
# Repeated four times, one set per block
# Each block is assigned an emotion that we want the participants to use when producing the words
# Each emotion will be associated with a simple line drawing of an emoji expressing that emotion
# Icon names correspond to those in the "Font Awesome" library (http://fontawesome.io)
stimuli <- data.frame(word = rep(c("adequate", "multiply", "compose",
                                   "pending", "specialist"), 4),
                      emotion = rep(c("NEUTRAL", "HAPPY",
                                      "SAD", "ANGRY"), each = 5)) %>%
  mutate(icon = ifelse(emotion == "NEUTRAL", "meh",
                       ifelse(emotion == "ANGRY", "angry",
                              ifelse(emotion == "SAD", "sad-tear",
                                     "smile"))))

# Save the set to a file----
write.csv(stimuli, "www/stimuli.csv", row.names = FALSE)

# Get the recording JS files into the www directory----
wwwPrep(recordJS = TRUE)



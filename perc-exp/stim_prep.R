library(dplyr)
library(tidyr)
library(tuneR)
library(shiny)
library(speechcollectr)

# Set working directory----
setwd("perc-exp")

## Get the duration of each audio recording----
file_dur <- lapply(list.files(path = "perc_stim", full.names = TRUE), function(i){
  w <- readWave(i, header = TRUE)
  dur <- ceiling((w$samples/w$sample.rate)*1000)
  return(dur)
})

## Make a data frame for storing filenames and durations----
stim_df <- data.frame(filename = list.files(path = "perc_stim")) %>%
  mutate(emotion = ifelse(grepl("\\dn", filename), "neutral",
                          ifelse(grepl("\\ds", filename), "sad",
                                 ifelse(grepl("\\dh", filename), "happy",
                                        "angry"))),
         duration = unlist(file_dur)) %>%
  slice(c(rep(1, times = 2), 2:nrow(.)))

## write it to a csv file
write.csv(stim_df, "stimuli.csv")

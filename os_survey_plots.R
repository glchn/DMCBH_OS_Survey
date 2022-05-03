# assumes you downloaded your data from the qualtrics results tables in .csv file...

# variables you need to update before running script ---------------------------------
subdir <- "/22s_co-op/os_qualtrics_survey_20220502"
q55_csv <- "q55_table.csv"
q52_csv <- "q52_table.csv"
q56_csv <- "q56_table.csv"

#set up directory!--------------------------------------------------------------------
data_dir <- paste0( Sys.getenv("HOME"), subdir)
print(data_dir)
getwd()
setwd(data_dir)
getwd()

# load packages ---------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)

# plop in q55 -----------------------------------------------------------------------
q55 <- read.csv(q55_csv)
colnames(q55)[3] <- "Response rate (%)"
q55_cleaned <- q55[,-1]
q55_cleaned <- q55_cleaned[-c(6), ]
q55_cleaned[q55_cleaned == "Totally Diagree"] <- "Totally Disagree" # fixing a typo = =

# make plot for q55, uses counts (not percentages)
png('q55_counts.png', width = 700,height = 700)
# there has to be a smarter way of doing this... couldn't figure this out 
# (see previous commits): https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
q55_cleaned$Answer <- factor(q55_cleaned$Answer, levels=c("Totally Agree", "Agree", "Neutral", "Disagree", "Totally Disagree"))
  ggplot(q55_cleaned, aes(x=Answer, y=Count)) +
    ggtitle("Q55 - DMCBH becoming an Open Science Institute would bring value to the DMCBH community") + 
    geom_bar(stat = "identity") +
    xlab("Response") +
    ylab("Count")
dev.off()

# plot q52 and 56 ---------------------------------------------------------------------
# Q52 - Based on the information available to you and your current understanding of what an Open Science Institute is,
# should we position the DMCBH as an Open Science Institute?
# Q56 - Do you think open science in especially important in neuroscience as compared to other fields?
q52 <- read.csv(q52_csv)
colnames(q52)[3] <- "Response rate (%)"
q52_cleaned <- q52[,-1]
q52_cleaned <- q52_cleaned[-c(4), ]
q52_cleaned$Answer<-gsub("\\.","",as.character(q52_cleaned$Answer)) # note that . by itself just means any character

png('q52_counts.png', width = 700,height = 700)
q52_cleaned$Answer <- factor(q52_cleaned$Answer, levels=c("Yes", "No", "I don't know yet"))
ggplot(q52_cleaned, aes(x=Answer, y=Count)) +
  ggtitle("Q52 - Based on the information available to you and your current understanding of what an Open Science Institute 
          is, should we position the DMCBH as an Open Science Institute?") + 
  geom_bar(stat = "identity") +
  xlab("Response") +
  ylab("Count")
dev.off()

q56 <- read.csv(q56_csv)
colnames(q56)[3] <- "Response rate (%)"
q56_cleaned <- q56[,-1]
q56_cleaned <- q56_cleaned[-c(3), ]
png('q56_counts.png', width = 700,height = 700)
q56_cleaned$Answer <- factor(q56_cleaned$Answer, levels=c("Yes", "No"))
ggplot(q56_cleaned, aes(x=Answer, y=Count)) +
  ggtitle("Q56 - Do you think open science in especially important in neuroscience as compared to other fields?") + 
  geom_bar(stat = "identity") +
  xlab("Response") +
  ylab("Count")
dev.off()

# plot 57,58 --------------------------------------------------------------------------


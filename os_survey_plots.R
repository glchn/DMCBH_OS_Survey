# assumes you downloaded your data from the qualtrics results tables in .csv file...
# TODO: absolute size for bars

# variables you need to update before running script ---------------------------------
subdir <- "/Github/DMCBH_OS_Survey/data"
q55_csv <- "q55_table.csv"
q52_csv <- "q52_table.csv"
q56_csv <- "q56_table.csv"
q57_csv <- "q57_table.csv"
q58_csv <- "q58_table.csv"
text_size = 15
subtitle_size = 15
title_size = 17
bar_width = 0.5

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
library(scales)
library(gridExtra)

# plop in q55 -----------------------------------------------------------------------
q55 <- read.csv(q55_csv)
colnames(q55)[3] <- "Response rate (%)"
q55_cleaned <- q55[,-1]
q55_cleaned <- q55_cleaned[-c(6), ]
q55_cleaned[q55_cleaned == "Totally Diagree"] <- "Totally Disagree" # fixing a typo = =

# make plot for q55, uses counts (not percentages)
png('q55_plot.png', width = 900,height = 700)
# (see previous commits): https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
q55_cleaned$Answer <- factor(q55_cleaned$Answer, levels=c("Totally Agree", "Agree", "Neutral", "Disagree", "Totally Disagree"))
  ggplot(q55_cleaned, aes(x=Answer, y=Count)) +
    ggtitle("Q55 - DMCBH becoming an OSI would bring value to the DMCBH community.") + 
    geom_bar(stat = "identity", fill = "#6ca8d9", width = 0.7) +
    xlab("") +
    ylab("Number of Responses") +
    theme(title=element_text(size=title_size, face="bold"),
          axis.title=element_text(size=subtitle_size, face="bold"),
          axis.text=element_text(size=text_size, face="bold"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
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

png('q52_plot.png', width = 1000,height = 700)
q52_cleaned$Answer <- factor(q52_cleaned$Answer, levels=c("Yes", "No", "I don't know yet"))
ggplot(q52_cleaned, aes(x=Answer, y=Count)) +
  ggtitle("Q52 - Based on the information available to you and your current understanding of what an OSI 
          is, should we position the DMCBH as an OSI?") + 
  geom_bar(stat = "identity", fill = "#6ca8d9", width = 0.385) +
  xlab("") +
  ylab("Number of Responses") +
  theme(title=element_text(size=title_size, face="bold"),
        axis.title=element_text(size=subtitle_size,face="bold"),
        axis.text=element_text(size=text_size, face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
dev.off()

q56 <- read.csv(q56_csv)
colnames(q56)[3] <- "Response rate (%)"
q56_cleaned <- q56[,-1]
q56_cleaned <- q56_cleaned[-c(3), ]
png('q56_plot.png', width = 920,height = 700)
q56_cleaned$Answer <- factor(q56_cleaned$Answer, levels=c("Yes", "No"))
ggplot(q56_cleaned, aes(x=Answer, y=Count)) +
  ggtitle("Q56 - Do you think OS is especially important in neuroscience as compared to other fields?") + 
  geom_bar(stat = "identity", fill = "#6ca8d9", width = 0.29) +
  xlab("") +
  ylab("Number of Responses") +
  theme(title=element_text(size=title_size, face="bold"),
        axis.title=element_text(size=subtitle_size,face="bold"),
        axis.text=element_text(size=text_size, face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
dev.off()

# plot 57,58 --------------------------------------------------------------------------
# Q57 - Which options make open science especially important in neuroscience?
# Q58 - Which of the following options limit the importance of open science in neuroscience?

q57 <- read.csv(q57_csv)
q57_cleaned <- q57[,-1]
q57_cleaned <- q57_cleaned[-c(5), ]
q57_cleaned$Prev <- c("OS especially important in neuroscience as compared to other fields")
q57_cleaned$Match <- 1:nrow(q57_cleaned)
q57_cleaned$Answer<-gsub("Other","Other (For)",as.character(q57_cleaned$Answer))


q58 <- read.csv(q58_csv)
q58_cleaned <- q58[,-1]
q58_cleaned <- q58_cleaned[-c(5), ]
q58_cleaned$Prev <- c("OS not especially important in neuroscience as compared to other fields")
q58_cleaned$Match <- 1:nrow(q58_cleaned) 
q58_cleaned$Answer<-gsub("Other","Other (Against)",as.character(q58_cleaned$Answer))

total <- rbind(q57_cleaned, q58_cleaned)
total$Answer[total$Answer=="High burden of disease for brain illness, disorders, and injuries."]<-"High burden of disease"
total$Answer[total$Answer=="The enormous complexity of the brain is a global rather than local scientific challenge."]<-"Complexity of the brain"
total$Answer[total$Answer=="Open Science increases respect for different cultural views on and meanings of brain and mind"]<-"Respect for cultural diversity"
total$Answer[total$Answer=="There are other health research related priorities."]<-"Plethora of health priorities"
total$Answer[total$Answer=="All biomedical research areas would benefit from open science."]<-"OS benefits all disciplines"
total$Answer[total$Answer=="Science benefits everyone regardless of cultural views on and meanings of brain and mind."]<-"Benefits are independent of culture"
total <- total[-c(4,8),]

# attempting grouped... not working :-(
png('q5758_plot.png', width = 1100,height = 700)
total$Answer <- factor(total$Answer, levels=c("High burden of disease",
                                              "Complexity of the brain",
                                              "Respect for cultural diversity",
                                              "Plethora of health priorities",
                                              "OS benefits all disciplines",
                                              "Benefits are independent of culture"))
ggplot(total, aes(x=Answer, y=Count, fill=str_wrap(Prev, 30))) +
  labs(fill="Answer to Q56") +
  ggtitle("Q57/58 - Opinions on the importance of OS in neuroscience") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_bar(position="dodge", stat="identity", width = 0.68) +
  xlab("") +
  ylab("Number of Responses") +
  scale_fill_manual(values=c("#6ca8d9",
                             "#f58070")) +
  scale_x_discrete(labels = wrap_format(35)) +
  theme(legend.key.height=unit(2, "cm"),
        legend.text=element_text(size=text_size),
        legend.title=element_text(size=subtitle_size, face="bold"),
        title=element_text(size=title_size, face="bold"),
        axis.title=element_text(size=subtitle_size,face="bold"),
        axis.text=element_text(size=text_size, face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.85, 0.85))
dev.off()


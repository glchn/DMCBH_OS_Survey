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
library(forcats)

# plop in q55 -----------------------------------------------------------------------
q55 <- read.csv(q55_csv)
colnames(q55)[3] <- "Response rate (%)" # fixing wonky column name
# remove totals row
q55_cleaned <- q55[,-1]
rownames(q55_cleaned) <- q55_cleaned[,1]
row.names.remove <- c("Total")
q55_cleaned <- q55_cleaned[!(row.names(q55_cleaned) %in% row.names.remove), ]

# test
# data <- data.frame(
#   name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
#   val=sample(seq(1,10), 8 )
# )
# p <- data %>%
#   mutate(name = fct_relevel(name, 
#                             "north", "north-east", "east", 
#                             "south-east", "south", "south-west", 
#                             "west", "north-west")) %>%
#   ggplot( aes(x=name, y=val)) +
#   geom_bar(stat="identity") +
#   xlab("")

# make plot for q55, uses counts (not percentages)
png('q55_counts.png', width = 700,height = 700)
# p <- q55_cleaned %>%
#   mutate(Answer = fct_relevel(Answer, 
#                             "Totally agree", "Agree", "Neutral", 
#                             "Disagree", "Totally Disagree")) %>%
  ggplot(q55_cleaned, aes(x=Answer, y=Count)) +
    ggtitle("Q55 - DMCBH becoming an Open Science Institute would bring value to the DMCBH community") + 
    geom_bar(stat = "identity") +
    xlab("Response") +
    ylab("Count")
dev.off()

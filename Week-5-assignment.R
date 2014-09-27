# Matthew Miesle
# CUNY SPS
# IS 607 SECT 01
# Week 5 Assignment
# Due 9/30/2014 EOD

# Issued Problem:
# In a recent mythical poll in Scotland, voters were asked if they preferred 
# Cullen skink over Partan bree. Here are the results of the poll, with results
# broken down by city and by age group.

# 1. Write down 3 questions that you might want to answer based on this data.
# 2. Create an R data frame with 2 observations to store this data in its 
# current “messy” state. Use whatever method you want to re-create and/or 
# load the data.
# 3. Use the functionality in the tidyr package to convert the data frame to
# be “tidy data.”
# 4. Use the functionality in the plyr package to answer the questions that
# you asked in step 1.
# 5. Having gone through the process, would you ask different questions and/or
# change the way that you structured your data frame?

#### Question 1 ####
# 3 questions:
#     1: What is the population's preference as a whole?
#     2: What do 16-24 year olds prefer?
#     3: How does preference differ for all of Edinburgh to all of Glasgow?

#### Question 2 ####
# Create R data frame
library(dplyr)
scotland.soup <- tbl_df(read.table(text = "
    Yes 80100   143000  99400   150400
    No  53900   214800  43000   207000
", col.names = c("Vote", "Edinburgh_16to24", "Edinburgh_25plus", "Glasgow_16to24", "Glasgow_25plus")))

#### Question 3 ####
# convert to tidy data
library(tidyr)
library(stringr)
tidy.soup <- scotland.soup %>%
    gather(LocationAge, NumVotes, Edinburgh_16to24:Glasgow_25plus) %>%
    mutate(Location = Reduce(rbind, str_split(string = LocationAge, pattern = "_"))[, 1]) %>%
    mutate(Age = Reduce(rbind, str_split(string = LocationAge, pattern = "_"))[, 2]) %>%
    mutate(Vote = paste0(Vote, "Vote")) %>%
    spread(Vote, NumVotes) %>%
    arrange(Location, Age)

tidy.soup <- tidy.soup[, -1]
#     Location    Age NoVote YesVote
# 1 Edinburgh 16to24  53900   80100
# 2 Edinburgh 25plus 214800  143000
# 3   Glasgow 16to24  43000   99400
# 4   Glasgow 25plus 207000  150400


#### Question 4 ####
# Use plyr package to answer questions from #1
detach("package:tidyr")
detach("package:dplyr")
library(plyr)
# warnings received from RStudio about loading plyr after dplyr
# might want to load plyr first then dplyr at the beginning as suggested by the warning
# but also get warnings about doing it in opposite order

#     1: What is the population's preference as a whole?
total.votes <- c(NoVote = sum(tidy.soup$NoVote), YesVote = sum(tidy.soup$YesVote))
# NoVote    YesVote 
# 518700    472900 
# There are more "No" vote than "Yes" votes.  This means the population
# does not prefer Cullen skink over Partan bree

#     2: What do 16-24 year olds prefer?
# plyr attempt:
mysum <- function(x) colSums(x[, c("NoVote", "YesVote")])
age.votes <- ddply(tidy.soup[, -1], .variable = "Age", .fun = mysum)
#       Age NoVote YesVote
# 1 16to24  96900  179500
# 2 25plus 421800  293400

# There are more YesVotes for the 16-24 year olds, so the younger group
# prefers Cullen skink over Partan bree

age.pct.votes <- cbind(Age = data.frame(age.votes[, 1], stringsAsFactors = FALSE), 100 * age.votes[, 2:3] / rowSums(age.votes[, 2:3]))
#       Age   NoVote  YesVote
# 1 16to24 35.05789 64.94211
# 2 25plus 58.97651 41.02349
# http://stackoverflow.com/questions/25089665/error-only-defined-on-a-data-frame-with-all-numeric-variables-with-ddply-on-lar
# used to see that must create my own function to get sums of each column
# The Age column has a class() of factor. Tried to change this 
# by changing stringsAsFactors = FALSE, but this was not successful

#     3: How does preference differ for all of Edinburgh to all of Glasgow?
# plyr attempt:
location.votes <- ddply(tidy.soup[, -2], .variable = "Location", .fun = mysum)
#     Location NoVote YesVote
# 1 Edinburgh 268700  223100
# 2   Glasgow 250000  249800

location.pct.votes <- cbind(Location = data.frame(location.votes[, 1], stringsAsFactors = FALSE), 100 * location.votes[, 2:3] / rowSums(location.votes[, 2:3]))
# In both locations there is not a preference to Cullen skink over Partan bree,
# but the vote is much closer in Glasgow
#   Location   NoVote  YesVote
# 1 Edinburgh 54.63603 45.36397
# 2   Glasgow 50.02001 49.97999
# The Location column has a class() of factor. Tried to change this
# by changing stringsAsFactors = FALSE, but this was not successful

#### Question 5 ####
# Having gone through the process, would you ask different questions and/or
# change the way that you structured your data frame?

# I think the way tidy.soup is structured works well for the computations
# in Question 4, so I wouldn't change it.
# I'm not sure what other question I could ask.  With this small data set
# It's pretty easy to find the answers quickly and easily and I only see
# 2 ways to group this set - Location and Age.  Maybe I need to work on my
# creativity for seeing what can be learned from data

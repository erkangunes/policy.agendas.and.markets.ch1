#----------------------------------------------------#
#-----Policy Agendas and Markets - Policy Data ------#
#----------------------------------------------------#


#This script extracts CAP(Comparative Agendas Project) major topic and subtopic 
#information from congressional hearing level data, and calculates monthly
#policy agenda characteristics including topic diversity and attention volatility


#### Packages and working directory ####

if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(zoo)) install.packages("zoo")
if(!require(lessR)) install.packages("lessR")
if(!require(TSstudio)) install.packages("TSstudio")

library(rstudioapi)
library(zoo)
library(lessR)
library(TSstudio)

this_files_path <- getActiveDocumentContext()$path
setwd(dirname(this_files_path))


#### Loading the raw data file ####
hearings.data <- na.omit(read.csv2("data/congressional_hearings.csv", sep = ","))
head(hearings.data, 10)

#### Creating the monthly majortopic frequency dataset #### 

#Each column corresponds to a policy issue topic (e.g. International Affairs). 
#Each row corresponds to monthly issue topic frequency vector.

issues <- c(to("policy", 23))
topics.monthly <- data.frame(year = rep(c(min(hearings.data$year):max(hearings.data$year)), each = 12), 
                             month = rep(seq(1:12), times = (max(hearings.data$year) - min(hearings.data$year) +1)), 
                             month.year = NA)

topics.monthly$month.year <- as.yearmon(paste(topics.monthly$year, topics.monthly$month, sep = "-"))
topics.monthly[issues] <- 0

#### Filling the matrix with monthly topic frequencies  ####

for (i in 1:nrow(hearings.data)) {
    topic.number <- hearings.data$majortopic[i]
    month.year.raw <- as.yearmon(paste(hearings.data$year[i], hearings.data$month[i], sep = "-"))
    topic.freq <- topics.monthly[which(topics.monthly$month.year == month.year.raw),(topic.number + 3)] 
    topics.monthly[which(topics.monthly$month.year == month.year.raw),(topic.number + 3)] <- topic.freq + 1   
}

head(topics.monthly, 10)

#### Calculating attention allocation characteristics ####

#Issue fractions

topics.monthly$total.attention <- rowSums(topics.monthly[,issues])
fraction.columns <- c( to("p", 23))
fraction <- function(a){issue.fracs <- a/topics.monthly$total.attention
return(issue.fracs)}
topics.monthly[fraction.columns] <- apply(topics.monthly[,issues], MARGIN = 2, FUN = fraction)


#Information entropy to measure attention diversity across topics

for (i in 1:nrow(topics.monthly)) {
    topics.monthly$entropy[i] <- -(sum(log(topics.monthly[i,fraction.columns]^topics.monthly[i,fraction.columns]))/log(21))   
}

#Herfindahl index - an alternative to information entropy measure of attention diversity

for(i in 1:nrow(topics.monthly)) { 
    h <- sum(topics.monthly[i,fraction.columns]^2)
    topics.monthly$herfindahl[i] <- (h - (1/21))/(1-(1/21))
}

#Entropy vs. Herfindahl plot

topics.monthly$date <- as.Date(topics.monthly$month.year)
ts_plot(topics.monthly[, c("date", "entropy", "herfindahl")])

#Entropy moving average

topics.monthly$entropy.ma3 <- NA
topics.monthly$entropy.ma6 <- NA
for (i in 3:nrow(topics.monthly)){
    topics.monthly$entropy.ma3[i] <- mean(topics.monthly$entropy[i-2:1])
    if(i < 6){
        next
    }
    topics.monthly$entropy.ma6[i] <- mean(topics.monthly$entropy[i-5:1])
}


#### Economic Policy Uncertainty (EPU) Index Data ####


epu.data <- read.csv2("data/US_Policy_Uncertainty_Data.csv", sep = ",")
head(epu.data, 10)

colnames(epu.data)[c(1,2)] <- c("year", "month")

topics.monthly <- merge(topics.monthly, epu.data[,c(1:3)], by = c("year", "month"))
topics.monthly$epu <- as.numeric(as.character(topics.monthly$Baseline_overall_index))
topics.monthly <- topics.monthly[,- which(colnames(topics.monthly) == "Baseline_overall_index")]

topics.monthly <- topics.monthly[order(topics.monthly$year, topics.monthly$month),]


#### Export the dataset ####

write.csv(topics.monthly, file = "topics.monthly.csv")


#Create the data frame that will store policy and stock price data


#aam = attention and markets
aam.data <- data.frame(
    year = topics.monthly$year, 
    month = topics.monthly$month, 
    entropy = topics.monthly$entropy,
    entropy.ma3 = topics.monthly$entropy.ma3,
    entropy.ma6 = topics.monthly$entropy.ma6,
    total.attention = topics.monthly$total.attention, 
    herfindahl = topics.monthly$herfindahl,
    epu = topics.monthly$epu
    )

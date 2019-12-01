# Beer Data Science Project
# by Devi Chitra Lakshmanan

# Turn off scientific notations
options(scipen = 999)

# import libraries
library(readr)
library(scales)
library(data.table)
library(dplyr)  
library(stringr)
library(ggplot2)
library(reshape2)
library(Amelia)
library(Hmisc)
library(corrgram)
library(corrplot)
library(qdap)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(quanteda)
library(SnowballC)
library(RColorBrewer)

# import Data
BeerData <- fread(file.choose())
BeerData <- as.data.table(BeerData)

# Examine the structure of the data
str(BeerData)

# conver Unix time stamp to Date
BeerData$reviewidate <- as.Date(as.POSIXct(BeerData$review_time, origin="1970-01-01"))

# Get to know more about the data
dim(BeerData)
#Number of unique Breweries
length(unique(BeerData$beer_brewerId))
# Number of unique Beer Styles and IDS
length(unique(BeerData$beer_beerId))
length(unique(BeerData$beer_style))

# Number of missing vaues in each variable
table(is.na(BeerData))
# Missing values by column
sapply(BeerData, function(x) sum(is.na(x)))

# Graph Missing values
missmap(BeerData, main = "Missing Values Vs NonMissing Values")


# mISSING VALUES
library(VIM)
 mice_plot <- aggr(BeerData, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(BeerData), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

#Question #1
# subset the data
BeerData_ABV <- na.omit(BeerData)
BeerData_ABV <- as.data.table(BeerData_ABV)

# histogram with added parameters
# histogram with added parameters
ggplot(BeerData_ABV, aes(x=beer_ABV)) + 
  geom_histogram(color="blue", fill="yellow")+
   scale_x_continuous(breaks = seq(0, 80, by = 5)) + labs(x = "Beer Alcohol by Volume")+
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"))

BeerData_ABV_Group_2 <- BeerData_ABV[, c("beer_ABV", "beer_brewerId")]
BeerData_ABV_Group_2 <- as.data.table(BeerData_ABV_Group_2)
BeerData_ABV_Group_sorted = BeerData_ABV_Group_2[,.SD[order(beer_ABV,decreasing = TRUE),][1:3],by = "beer_brewerId"]
BeerData_ABV_Group_Agg <- BeerData_ABV_Group_sorted %>%
  group_by(beer_brewerId) %>%
  summarise(meanABV = mean(beer_ABV))
BeerData_ABV_Group_Agg <- as.data.table(BeerData_ABV_Group_Agg)
BeerData_ABV_Group_StrongBeers <- BeerData_ABV_Group_Agg[order(-meanABV),]

# subset the top 3 Brewers
StrongBeerBrewer <- head(BeerData_ABV_Group_StrongBeers, n = 3)


#Question #2

# subset the BeerData using Keys
setkey(BeerData, review_appearance,review_palette,review_overall,review_taste,review_aroma)
BeerData_year <- BeerData[.(5,5,5,5,5)]
BeerData_year$Year <- year(BeerData_year$reviewidate)
BeerData_year <- BeerData_year[, .(beer_name, Year)]

# count of high rating by year
BeerData_year <- BeerData_year %>%
  group_by(Year) %>%
  summarise(n = n())
print(BeerData_year)

# total Reviews by year
BeerData$Year <- year(BeerData$reviewidate)
BeerData_allYear <- BeerData %>%
  group_by(Year) %>%
  summarise(n = n())
print(BeerData_allYear)
setnames(BeerData_allYear, 'n','TotalReviews')
# Merge with BeerData_year
BeerData_year_Final <- left_join(BeerData_year, BeerData_allYear, by = "Year")
print(BeerData_year_Final)
BeerData_year_Final$PositivePercent <- round((BeerData_year_Final$n/BeerData_year_Final$TotalReviews),2)
BeerData_year_Final$PositivePercent <- percent(BeerData_year_Final$PositivePercent)
ggplot(data=BeerData_year_Final, aes(x=Year, y=PositivePercent)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  scale_x_continuous(breaks = seq(2000, 2012, by = 2)) + labs(x = "Year", y = "Percentage of High Ratings")+
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"))


# Question #3

Ratingscolumn <- c("review_appearance","review_palette","review_aroma","review_overall","review_taste")
BeerData_Ratings <- BeerData[, Ratingscolumn, with = FALSE]
BeerData_Ratings <- as.data.frame(BeerData_Ratings)
# correlation Matrix
CorrelationMatrix <- cor(BeerData_Ratings)
CorrelationMatrix <- round(CorrelationMatrix,2)

#corrleation plot using corrgram
library(corrgram)
corrgram(BeerData_Ratings, order=TRUE, lower.panel = panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrlation between Review Ratings")


# Question #4

# create ABV group
ABVGroup <- function(beer_ABV) {
  if (beer_ABV <= 10) {return('<10')}
  else if (beer_ABV > 10 & beer_ABV <= 20 ) {return('10-20')}
  else if (beer_ABV >20 & beer_ABV <= 30) {return ('20-30')}
  else if (beer_ABV > 30 & beer_ABV <= 40) {return ('30-40')}
  else {return('>40')}
}
  
BeerData_ABV$ABVGroup <- sapply(BeerData_ABV$beer_ABV, ABVGroup)  
BeerData_ABV$ABVGroup <- as.factor(BeerData_ABV$ABVGroup) 

# count of reviews by ABV group
BeerData_ABV_Group <- BeerData_ABV[,c("ABVGroup","review_overall")]

BeerData_ABV_Group <- BeerData_ABV_Group %>%
  group_by(ABVGroup) %>%
  summarise(n = n())
print(BeerData_ABV_Group)

#Ggplot of observations by ABV Group
ggplot(data=BeerData_ABV_Group, aes(x=ABVGroup, y=n)) +
  geom_bar(stat="identity",color="blue", fill="green")  + labs(x = "ABV Group", y = "Number of Observations")+
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "blue"))

# Subset the data to ABV less than or equal to 20
BeerData_Best <- BeerData[beer_ABV <= 20, ]
BeerData_Best <- na.omit(BeerData_Best)
BeerData_Best_ProfileName <- BeerData_Best[, c("beer_name","review_profileName")]

BeerData_Best_ProfileName <- BeerData_Best_ProfileName %>%
  group_by(beer_name) %>%
  summarise(n = n())

BeerData_Best_ProfileName <- as.data.table(BeerData_Best_ProfileName)
BeerData_Best_ProfileName <- BeerData_Best_ProfileName[order(-n),]
BeerData_Best_ProfileName_Top100 <- head(BeerData_Best_ProfileName, 100)
BeerData_Best_ProfileName_Top100$ReviewRating <- "Top"
BeerData_BestBeers <- left_join(BeerData_Best, BeerData_Best_ProfileName_Top100, by = "beer_name")
BeerData_BestBeers <- na.omit(BeerData_BestBeers)
BeerData_BestBeers_Agg <- BeerData_BestBeers %>%
  group_by(beer_name) %>%
  summarise(MeanReview = mean(review_overall))
BeerData_BestBeers_Agg <- BeerData_BestBeers_Agg[order(-BeerData_BestBeers_Agg$MeanReview),]
TopThreeBeers <- head(BeerData_BestBeers_Agg,3)
TopThreeBeers$BeerRating <- "Top 3"

# Question #5
BeerData_beerStyle <-  left_join(BeerData, TopThreeBeers, by = "beer_name")

# Subset the data to include only the Beer styles with top beer names
BeerData_beerStyle <- na.omit(BeerData_beerStyle)

# find the work frquencies in the  top Beer names
myCorpus <- Corpus(VectorSource(BeerData_beerStyle$review_text))
cleanset <- tm_map(myCorpus, removeWords, stopwords("english"))
#cleanset <- tm_map(cleanset, removeWords, c("also", "the","can", "it","but", "with", "again" ))
cleanset <- tm_map(cleanset, tolower)
cleanset <- tm_map(cleanset, removePunctuation)
cleanset <- tm_map(cleanset, removeNumbers)
cleanset <- tm_map(cleanset, removeWords, c("also", "the","can", "but", "hop", "beer","alcohol","pour"))
cleanset <- tm_map(cleanset, stemDocument)

# viewing the corpus content
cleanset[[8]][1]
tdm_1 <- TermDocumentMatrix(cleanset)
m <- as.matrix(tdm_1)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
# generate wordcloud
set.seed(1234)

TopWords <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# plot word frequencies
TopWordsGraph <- barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

# Subset the  data to Low ratigs and check the word frequencies
BeerData_LowReviews <- BeerData_Best[BeerData_Best$review_overall == 1, ]

# Rerpeat the steps to get the most frequently used words in beers withh low ratings.
myCorpus_L <- Corpus(VectorSource(BeerData_LowReviews$review_text))
cleanset_L <- tm_map(myCorpus_L, removeWords, stopwords("english"))
#cleanset <- tm_map(cleanset, removeWords, c("also", "the","can", "it","but", "with", "again" ))
cleanset_L <- tm_map(cleanset_L, tolower)
cleanset_L <- tm_map(cleanset_L, removePunctuation)
cleanset_L <- tm_map(cleanset_L, removeNumbers)
cleanset_L <- tm_map(cleanset_L, removeWords, c("also", "the","can", "but", "hop", "beer","alcohol","pour"))
cleanset_L <- tm_map(cleanset_L, stemDocument)

# viewing the corpus content
cleanset_L[[8]][1]
tdm_L <- TermDocumentMatrix(cleanset_L)
m_L <- as.matrix(tdm_L)
v_L <- sort(rowSums(m_L),decreasing=TRUE)
d_L <- data.frame(word = names(v_L),freq=v_L)
head(d_L, 10)
# generate wordcloud
set.seed(1234)
TopWords_L <- wordcloud(words = d_L$word, freq = d$freq, min.freq = 1,
                      max.words=200, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))

# plot word frequencies
TopWordsGraph_L <- barplot(d_L[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
                         col ="lightblue", main ="Most frequent words",
                         ylab = "Word frequencies")


# Best Beer Styles based on Best Review Ratings
BeerData_beerStyle <- as.data.table(BeerData_beerStyle)
BeerData_beerStyle_Agg <- BeerData_beerStyle %>%
  group_by(beer_style) %>%
  summarise(MeanReview = mean(review_overall))
BeerData_beerStyle_Agg <- BeerData_beerStyle_Agg[order(-BeerData_beerStyle_Agg$MeanReview),]




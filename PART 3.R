#import libraries
library(dplyr)
library(readr)
library(psych)

#import scraped csv files
p_90 <- read.csv(file.choose())
p_2012 <- read.csv(file.choose())
p_2016 <- read.csv(file.choose())
p_currenthits <- read.csv(file.choose())
p_indiapop <- read.csv(file.choose())
p_top100 <- read.csv(file.choose())
P_drake <- read.csv(file.choose())
p_ed <- read.csv(file.choose())
p_calvin <- read.csv(file.choose())

p_2012$X<-NULL
p_90$X<-NULL
p_currenthits$X<-NULL
p_indiapop$X<-NULL
p_top100$X<-NULL


#create a master testing data and delete duplicate rows
master <- rbind(p_90, p_2012, p_2016, p_currenthits, p_indiapop, p_top100)
test_data <- master %>% 
  distinct(name, .keep_all = TRUE)

#remove X column from the testing dataset, check structure
test_data$X <- NULL
str(test_data)
#change length from miliseconds to seconds
test_data$length <- ceiling(test_data$length/1000)

#use calvin harris' data as first training dataset, clean
train_data1 <- p_calvin
train_data1$X <- NULL
test_data1 <- test_data

#remove uninteresting columns 
train_data1 <- train_data1 %>% select (-c(name, album, artist, release_date, length))
test_data1 <- test_data1 %>% select (-c(name, album, artist, release_date, length))

combine <- rbind(test_data1, train_data1)
combine$danceability.1<-NULL

#Plot histogram of the interesting features to aid feature selection
test_data1$hist <- 'test'
train_data1$hist <- 'train'

############### Drake similarity ################################################

train_data = read.csv(file.choose())
test_data = read.csv(file.choose())
test_data$X <- NULL
test_data$album <-NULL
test_data$artist <- NULL
#Plot histogram of the interesting features to aid feature selection
test_data$hist <- 'test'
train_data$hist <- 'train'
combine_dance1 <- rbind(test_data, train_data)
library(ggplot2)
ggplot(combine_dance1, aes(danceability, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(combine_dance1, aes(acousticness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(combine_dance1, aes(energy, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(combine_dance1, aes(instrumentalness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(combine_dance1, aes(liveness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(combine_dance1, aes(loudness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(combine_dance1, aes(speechiness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(combine_dance1, aes(tempo, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

################## train_data hist #######################
library(ggplot2)
ggplot(train_data, aes(danceability, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(train_data, aes(acousticness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(train_data, aes(energy, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(train_data, aes(instrumentalness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(train_data, aes(liveness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(train_data, aes(loudness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(train_data, aes(speechiness, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(train_data, aes(tempo, fill = hist)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

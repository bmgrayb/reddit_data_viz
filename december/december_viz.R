set.seed(10)
library(plotly)
library(tidyverse)
library(tidyr)
library(forecast)

#read in lake data
lake_data<-read.csv(file="data/raw_lake_mendota_freeze.csv", header = TRUE,sep = ",")

##### NOW WE NEED TO CLEANSE THE DATA
#remove the last row because it is the current year
n<-dim(lake_data)[1]
lake_data<-lake_data[1:(n-1),]

#add missing years
lake_data[lake_data == "\""] <- "0000-00"
missing_years <- which(lake_data[,"WINTER"] == "0000-00")
lake_data[missing_years,"WINTER"] <- lake_data[missing_years-1,"WINTER"]


#create date objects from the winter, closed, and opened columns
lake_data <- lake_data %>% separate(.,col="WINTER",into = c("START_YEAR","END_YEAR"),sep = "-", remove = FALSE)
lake_data$END_YEAR <- apply(lake_data, 1, function(x) paste(substring(x[2],1,2), x[3],sep = ""))
wrong_years<-which(lake_data[,"END_YEAR"] < lake_data[,"START_YEAR"])
lake_data[wrong_years,"END_YEAR"] <- as.character(as.integer(lake_data[wrong_years,"END_YEAR"]) + 100)
lake_data$START_DATE <- as.Date(paste(lake_data$CLOSED,lake_data$START_YEAR), format = "%d %b %Y")
lake_data$END_DATE <- as.Date(paste(lake_data$OPENED,lake_data$END_YEAR), format = "%d %b %Y")
#calculate the number of days between due to some missing data
lake_data$NUM_DAYS <- lake_data$END_DATE - lake_data$START_DATE

#some years are still wrong so lets recalculate
wrong_years2<-which(lake_data[,"NUM_DAYS"] > 365)
lake_data[wrong_years2,"START_YEAR"] <- as.character(as.integer(lake_data[wrong_years2,"START_YEAR"]) + 1)
lake_data$START_DATE <- as.Date(paste(lake_data$CLOSED,lake_data$START_YEAR), format = "%d %b %Y")
lake_data$END_DATE <- as.Date(paste(lake_data$OPENED,lake_data$END_YEAR), format = "%d %b %Y")
lake_data$NUM_DAYS <- as.integer(lake_data$END_DATE - lake_data$START_DATE)

#data set to plot, removing days where the lake thawed and refroze
cols_to_keep <- c("START_DATE","END_DATE","NUM_DAYS")
lake_data_final <- lake_data[lake_data$NUM_DAYS > 25,cols_to_keep]

#histogram of number of days closed
h <- ggplot(data=lake_data_final, aes(lake_data_final$NUM_DAYS)) + 
  geom_histogram(breaks=seq(20, 180, by = 10), 
                 col="red", 
                 aes(fill=..count..))+
  labs(title="Histogram of Lake Mendota Closures", x = "Days Closed", y = "Occurences" )

ggsave("img/histogram.png", plot = h, device = png(),
       scale = 1, dpi = 300)


## ggplot trend line
t <- ggplot(lake_data_final, aes(x = END_DATE, y = NUM_DAYS)) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  labs(title="Number of Days Closed,
Lake Mendota", x = "Year", y = "Days Closed" )
ggplotly()

ggsave("img/trend.png", plot = t, device = png(),
       scale = 1, dpi = 300)

#forecast plot
fit <- ets(lake_data_final$NUM_DAYS)
fc <- forecast(lake_data_final$NUM_DAYS, model=fit)
plot(fc)

png(filename="img/forecast.png")
plot(fc)
dev.off()

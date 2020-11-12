#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("reshape2")

library(dplyr)
library(ggplot2)
library(reshape2)

library(readr)
NewTownData <- read_csv("NewTownData.csv", 
                        col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                         `Total Case Count` = col_integer(), 
                                         `Case Count last 14 days` = col_integer(), 
                                         `Total Tests last 14 days` = col_integer(), 
                                         `Total positive tests last 14 days` = col_integer()))

NatickPopulation <- 36050  #estimate of Natick's population as of 8/1/20
perFactor <- 100000  #most COVID data reported per 100k people

#use following lines of code after importing csv of other town data - file should be named "NewTownData"
#NewTownData$Date <- as.Date(NewTownData$Date, "%y-%m-%d")
NewTownData <- NewTownData %>%   #calculate new confirmed case rate per 100k people
  mutate('New Conf per day per 100k' = `Total positive tests last 14 days`/14/Population*perFactor)
otherTownDat <- rbind(NewTownData, otherTownDat)  #add new area data to beginning of otherTownData date set

previousDays2Plot <- 45  #plot data of last 45 calendar days
thresholds <- rbind( c("green",1), c("yellow",4), c("red",8) )  #these thresholds were set in August by the Commonwealth of MA; they may not correlate to closing of schools
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  #color palette for plotting

label1 = subset(otherTownDat, Town=="Natick" & Date==max(otherTownDat$Date))  #label for 14-day Natick data
#label2 = subset(dat, Date==max(dat$Date))  #label for 3/4-day Natick data

#create new plot with updated data
dat %>% ggplot(aes(x=Date, color=Town)) +
#  geom_line(aes(y=New_Conf_per_day_per_100k), size = 1.5) +
#  geom_point(aes(y=New_Conf_per_day_per_100k), size = 3) +
  scale_x_date(date_labels = "%b %d", limit=c(as.Date(max(dat$Date,otherTownDat$Date)-previousDays2Plot),as.Date(max(dat$Date,otherTownDat$Date)))) +
  coord_cartesian(ylim = c(0, 30)) +
  #geom_rect(aes(xmin=as.Date(-Inf), ymin=10, xmax=as.Date(Inf), ymax=Inf,alpha=0.01,fill="red" )) +
  ylab("Cases per 100,000 residents") +
  xlab("Date") +
#  ggtitle(label=paste("Cases per day per 100k residents 14-day average \nlast",previousDays2Plot,"days from",max(dat$Date,otherTownDat$Date)),
#          subtitle="Thick line is 3-4 day smoothed average for Natick") +
  ggtitle(label=paste("Positive tests per day per 100k residents 14-day average \nlast",previousDays2Plot,"days from",max(dat$Date,otherTownDat$Date))) +
  geom_hline(yintercept=as.numeric(10), linetype="solid", color = "red", size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[3,2]), linetype="dashed", color = thresholds[3,1], size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[2,2]), linetype="dashed", color = thresholds[2,1], size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[1,2]), linetype="dashed", color = thresholds[1,1], size=1) +
  theme_bw() +
  geom_line(data = otherTownDat, aes(y=`New Conf per day per 100k`)) +
  geom_point(data = otherTownDat, aes(y=`New Conf per day per 100k`)) +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
#  geom_text(data=label2,aes(Date,New_Conf_per_day_per_100k,label=round(New_Conf_per_day_per_100k, digits=1)),nudge_y = -0.3) +
  geom_text(data=label1,aes(x=Date,y=`New Conf per day per 100k`,label=round(`New Conf per day per 100k`, digits=1)),nudge_y = +0.4)


localTowns <- c("Natick","Framingham","Wayland","Wellesley","Dover","Sherborn","Weston")
otherTownDat2 <- otherTownDat %>%
  filter(Town %in% localTowns) %>%
  group_by(Date) %>%
  mutate(equal_wtd_rate = mean(`New Conf per day per 100k`), pop_wtd_rate = sum(`Total positive tests last 14 days`)/14/sum(Population)*perFactor) %>%
  select(Date, equal_wtd_rate, pop_wtd_rate) %>%
  unique()
melt(otherTownDat2, id=1) %>%
  ggplot(aes(x=Date,y=value,color=variable)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%b %d", limit=c(as.Date(max(otherTownDat2$Date)-previousDays2Plot),as.Date(max(otherTownDat2$Date)))) +
  coord_cartesian(ylim = c(0, 20)) +
  ylab("Cases per 100,000 residents") +
  xlab("Date") +
  ggtitle(label=paste("Positive tests per day per 100k residents\n14-day average \nlast",previousDays2Plot,"days from",max(dat$Date,otherTownDat$Date))) +
  geom_hline(yintercept=as.numeric(10), linetype="solid", color = "red", size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[3,2]), linetype="dashed", color = thresholds[3,1], size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[2,2]), linetype="dashed", color = thresholds[2,1], size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[1,2]), linetype="dashed", color = thresholds[1,1], size=1) +
  theme_bw() +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(Date,value,label=round(value, digits=1)),nudge_y = +0.2)


localTowns <- c("Natick","Wayland","Wellesley","Dover","Sherborn","Weston")
otherTownDat2 <- otherTownDat %>%
  filter(Town %in% localTowns) %>%
  group_by(Date) %>%
  mutate(equal_wtd_rate = mean(`New Conf per day per 100k`), pop_wtd_rate = sum(`Total positive tests last 14 days`)/14/sum(Population)*perFactor) %>%
  select(Date, equal_wtd_rate, pop_wtd_rate) %>%
  unique()
melt(otherTownDat2, id=1) %>%
  ggplot(aes(x=Date,y=value,color=variable)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%b %d", limit=c(as.Date(max(otherTownDat2$Date)-previousDays2Plot),as.Date(max(otherTownDat2$Date)))) +
  coord_cartesian(ylim = c(0, 12)) +
  ylab("Cases per 100,000 residents") +
  xlab("Date") +
  ggtitle(label=paste("Positive tests per day per 100k residents\n14-day average \nlast",previousDays2Plot,"days from",max(dat$Date,otherTownDat$Date),"\n Ex Framingham")) +
  geom_hline(yintercept=as.numeric(10), linetype="solid", color = "red", size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[3,2]), linetype="dashed", color = thresholds[3,1], size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[2,2]), linetype="dashed", color = thresholds[2,1], size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[1,2]), linetype="dashed", color = thresholds[1,1], size=1) +
  theme_bw() +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(Date,value,label=round(value, digits=1)),nudge_y = +0.2)


write.csv(otherTownDat,"NatickArea14dayData.csv")  #export area data as csv file
#write.csv(dat,"NatickTwiceWeeklyData.csv")  #export Natick data as csv file


#Discontinued use of adjacent towns list below (without Weston) on 11/12/20
#weighted plots based on adjacent towns to Natick, not including Weston
localTowns <- c("Natick","Framingham","Wayland","Wellesley","Dover","Sherborn")
otherTownDat2 <- otherTownDat %>%
  filter(Town %in% localTowns) %>%
  group_by(Date) %>%
  mutate(equal_wtd_rate = mean(`New Conf per day per 100k`), pop_wtd_rate = sum(`Total positive tests last 14 days`)/14/sum(Population)*perFactor) %>%
  select(Date, equal_wtd_rate, pop_wtd_rate) %>%
  unique()
melt(otherTownDat2, id=1) %>%
  ggplot(aes(x=Date,y=value,color=variable)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%b %d", limit=c(as.Date(max(otherTownDat2$Date)-previousDays2Plot),as.Date(max(otherTownDat2$Date)))) +
  coord_cartesian(ylim = c(0, 20)) +
  ylab("Cases per 100,000 residents") +
  xlab("Date") +
  ggtitle(label=paste("Positive tests per day per 100k residents\n14-day average \nlast",previousDays2Plot,"days from",max(dat$Date,otherTownDat$Date))) +
  geom_hline(yintercept=as.numeric(10), linetype="solid", color = "red", size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[3,2]), linetype="dashed", color = thresholds[3,1], size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[2,2]), linetype="dashed", color = thresholds[2,1], size=1) +
  #geom_hline(yintercept=as.numeric(thresholds[1,2]), linetype="dashed", color = thresholds[1,1], size=1) +
  theme_bw() +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(Date,value,label=round(value, digits=1)),nudge_y = +0.2)




#Natick no longer updating twice-weekly data as of 9/17/20
#use the following lines of code to add Natick's twice-weekly data
newData <- data.frame(Town = "Natick", 
                      Date = as.Date("09/17/2020", "%m/%d/%Y"), 
                      Day_Difference = as.integer(3), 
                      Confirmed_Difference = as.integer(2), 
                      Current = as.integer(7), 
                      Total_Confirmed = as.integer(470), 
                      #Total_Probable = as.integer(237), 
                      Total_Probable = NA, 
                      #Total_Probable_Confirmed = as.integer(690), 
                      Total_Probable_Confirmed = NA, 
                      #New_Probable_Confirmed = as.integer(11), 
                      New_Probable_Confirmed = NA, 
                      Removed_Probable_Confirmed = NA,
                      New_Prob_Conf_per_day_per_100k = NA, 
                      New_Conf_per_day_per_100k = NA, 
                      Current_per_100k = NA,
                      avg7dayPer100k = NA)
dat <- rbind(newData, dat)  #add new Natick data to beginning of full data set

#calculate new rates per 100k based on newly-added data
dat <- dat %>% mutate('New_Prob_Conf_per_day_per_100k' = New_Probable_Confirmed/Day_Difference/NatickPopulation*perFactor,
                      'New_Conf_per_day_per_100k' = Confirmed_Difference/Day_Difference/NatickPopulation*perFactor,
                      'Current_per_100k' = Current/NatickPopulation*perFactor)

#create 7-day daily average of newly confirmed cases, assuming "dat" data frame lists data in chronological order
avg7dayPer100k <- (diff(dat$Total_Confirmed, lag=2)*-1) / as.numeric(diff.Date(dat$Date, lag=2)*-1) / NatickPopulation * perFactor
avg7dayPer100k <- c(avg7dayPer100k,NA,NA)  ##diff functions above creates a vector with length 2 less than "dat" data frame
dat$avg7dayPer100k <- avg7dayPer100k   ##rewrite 7-day average data in "dat" data frame

#7-day Natick plot
dat %>% ggplot(aes(x=Date)) +
  geom_line(aes(y=avg7dayPer100k), size = 1) +
  geom_point(aes(y=avg7dayPer100k), size = 2) +
  scale_x_date(date_labels = "%b %d", limit=c(as.Date(max(dat$Date)-previousDays2Plot),as.Date(max(dat$Date)))) +
  coord_cartesian(ylim = c(0, 12)) +
  ylab("Cases per 100,000 residents") +
  xlab("Date") +
  ggtitle(label=paste("Cases per day per 100k residents\n7-day average \nlast",previousDays2Plot,"days from",max(dat$Date))) +
  geom_hline(yintercept=as.numeric(thresholds[3,2]), linetype="dashed", color = thresholds[3,1], size=1) +
  geom_hline(yintercept=as.numeric(thresholds[2,2]), linetype="dashed", color = thresholds[2,1], size=1) +
  geom_hline(yintercept=as.numeric(thresholds[1,2]), linetype="dashed", color = thresholds[1,1], size=1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(Date,avg7dayPer100k,label=round(avg7dayPer100k, digits=1)),nudge_y = +0.5) #+
  #geom_smooth(aes(Date,avg7dayPer100k),se=FALSE)


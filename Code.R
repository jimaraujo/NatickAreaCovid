install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

#use the following lines of code to add Natick's twice-weekly data
newData <- data.frame(Town = "Natick", 
                      Date = as.Date("08/24/2020", "%m/%d/%Y"), 
                      Day_Difference = as.integer(4), 
                      Current = as.integer(3), 
                      Total_Confirmed = as.integer(453), 
                      Total_Probable = as.integer(237), 
                      Total_Probable_Confirmed = as.integer(690), 
                      Confirmed_Difference = as.integer(3), 
                      New_Probable_Confirmed = as.integer(11), 
                      Removed_Probable_Confirmed = NA,
                      New_Prob_Conf_per_day_per_100k = NA, 
                      New_Conf_per_day_per_100k = NA, 
                      Current_per_100k = NA )
dat <- rbind(newData, dat)  #add new Natick data to beginning of full data set

NatickPopulation <- 36050  #estimate of Natick's population as of 8/1/20
perFactor <- 100000  #most COVID data reported per 100k people

#calculate new rates per 100k based on newly-added data
dat <- dat %>% mutate('New_Prob_Conf_per_day_per_100k' = New_Probable_Confirmed/Day_Difference/NatickPopulation*perFactor,
                      'New_Conf_per_day_per_100k' = Confirmed_Difference/Day_Difference/NatickPopulation*perFactor,
                      'Current_per_100k' = Current/NatickPopulation*perFactor)


#use following lines of code after importing csv of other town data - file should be named "NewTownData"
NewTownData$Date <- as.Date(NewTownData$Date, "%y-%m-%d")
NewTownData <- NewTownData %>%   #calculate new confirmed case rate per 100k people
  mutate('New Conf per day per 100k' = `Total positive tests last 14 days`/14/Population*perFactor)
otherTownDat <- rbind(NewTownData, otherTownDat)  #add new area data to beginning of otherTownData date set


previousDays2Plot <- 45  #plot data of last 45 calendar days
thresholds <- rbind( c("green",1), c("yellow",4), c("red",8) )  #these thresholds were set in August by the Commonwealth of MA; they may not correlate to closing of schools
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  #color palette for plotting

#create new plot with updated data
dat %>% ggplot(aes(x=Date, color=Town)) +
  geom_line(aes(y=New_Conf_per_day_per_100k), size = 1.5) +
  geom_point(aes(y=New_Conf_per_day_per_100k), size = 3) +
  scale_x_date(date_labels = "%b %d", limit=c(as.Date(max(dat$Date,otherTownDat$Date)-previousDays2Plot),as.Date(max(dat$Date,otherTownDat$Date)))) +
  coord_cartesian(ylim = c(0, 10)) +
  ylab("Cases per 100,000 residents") +
  xlab("Date") +
  ggtitle(label=paste("Cases per day per 100k residents 14-day average \nlast",previousDays2Plot,"days from",max(dat$Date,otherTownDat$Date)),
          subtitle="Thick line is 3-4 day smoothed average for Natick") +
  geom_hline(yintercept=as.numeric(thresholds[3,2]), linetype="dashed", color = thresholds[3,1], size=1) +
  geom_hline(yintercept=as.numeric(thresholds[2,2]), linetype="dashed", color = thresholds[2,1], size=1) +
  geom_hline(yintercept=as.numeric(thresholds[1,2]), linetype="dashed", color = thresholds[1,1], size=1) +
  theme_bw() +
  geom_line(data = otherTownDat, aes(y=`New Conf per day per 100k`)) +
  geom_point(data = otherTownDat, aes(y=`New Conf per day per 100k`)) +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


write.csv(dat,"NatickTwiceWeeklyData.csv")  #export Natick data as csv file
write.csv(otherTownDat,"NatickArea14dayData.csv")  #export area data as csv file

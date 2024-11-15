#data imports
library(readxl)
library(RCurl)
#load the data sheets
data <- read.csv("https://raw.githubusercontent.com/Apfia-658/Pyrosome-data/main/Consolidated%20Data.csv")
bongo_data <- read.csv("https://raw.githubusercontent.com/Apfia-658/Pyrosome-data/main/Bongo%20Tow%20Data.csv")
jul_aug_data <- read.csv("https://raw.githubusercontent.com/Apfia-658/Pyrosome-data/main/Jul%20-%20Aug%202023%20Data.csv")
#plotting imports
library(ggplot2)
#data transformation imports
library(dplyr)
#map imports
library(ggmap)
library(plotly)
#color palette
library("viridis")

#data transformation
data$C.T <- factor(data$C.T, levels = c("3", "CCT", "2", "1", "AT"))
data$Lat <- as.numeric(data$Lat)
data$Long <- as.numeric(data$Long)
data$Length..cm. <- as.numeric(data$Length..cm.)
data$Total.pigs.g.WW <- as.numeric(data$Total.pigs.g.WW)
data$Total.pigs.per.colony <- as.numeric(data$Total.pigs.per.colony)
data$Day.Night <- as.factor(data$Day.Night)

#jul-aug data transformation
jul_aug_data$C.T <- factor(jul_aug_data$C.T)
jul_aug_data$Total.pigs.g.WW <- as.numeric(jul_aug_data$Total.pigs.g.WW)
jul_aug_data$Length..cm.<- as.numeric(jul_aug_data$Length..cm.)
jul_aug_data$Day.Night <- as.factor(jul_aug_data$Day.Night)
jul_aug_data$Total.pigs.per.colony <- as.numeric(jul_aug_data$Total.pigs.per.colony)


##### LENGTH #####
#creates box plot
lenBoxPlot <- ggplot(data, aes(x = C.T , y = Length..cm., fill = C.T )) + 
  geom_boxplot() + scale_fill_viridis(discrete = TRUE)
#adds points on top
lenBoxPlot + geom_jitter(position=position_jitter(0.3), shape = 1, alpha = 0.5) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  xlab("Cycle/Transect") + ylab("Length (cm)")


##### TOTAL PIGMENT PER GRAM WET WEIGHT #####
#creates box plot
pigsBoxPlot <- ggplot(data, aes(x = C.T , y = Total.pigs.g.WW, fill = C.T )) + 
  geom_boxplot() +  geom_jitter(position = position_jitter(0.3), shape = 1, alpha = 0.5)
#adds points on top of box plot
pigsBoxPlot + theme(legend.position="right") + scale_fill_viridis(discrete = TRUE) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Cycle/Transect") + ylab("Total Pigment per gram Wet Weight")


##### TOTAL PIGMENT PER COLONY #####
#creates box plot
totalpigsBoxPlot <- ggplot(data, aes(x = C.T, y = Total.pigs.per.colony, fill = C.T)) + 
  geom_boxplot() +  geom_jitter(position = position_jitter(0.3), shape = 1, alpha = 0.5)
#adds points on top of box plot
totalpigsBoxPlot + theme(legend.position="right") + scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  xlab("Cycle/Transect") + ylab("Total Pigment per Colony")


##### LENGTH BY DAY AND NIGHT #####
dflen <- subset(data, C.T != "CCT" | C.T != "AT",) #creates data frame without transects
dflen$C.T <- factor(dflen$C.T, levels = c("1","2", "3")) #separates data into cycles
#creates box plot
dayNightBoxPlotLen <- ggplot(data = subset(dflen, !is.na(C.T)), 
                       aes(x = Day.Night, y = Length..cm., fill = Day.Night)) + 
  geom_boxplot()
#adds points on top of box plot
dayNightBoxPlotLen + 
  geom_jitter(position = position_jitter(0.3), shape = 1, alpha = 0.5) +
  facet_wrap(~ `C/T`, ncol = 3) + scale_fill_manual(values=c("#f2ad00", "#46acc8")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


##### TOTAL PIGMENT PER COLONY BY DAY AND NIGHT #####
#creates box plot
dayNightBoxPlotRt <- ggplot(data = subset(dflen, !is.na(C.T)), 
                       aes(x = Day.Night, y = Total.pigs.g.WW, fill = Day.Night)) + 
  geom_boxplot()
#adds points on top of box plot
dayNightBoxPlotRt + 
  geom_jitter(position = position_jitter(0.3), shape = 1, alpha = 0.5) +
  facet_wrap(~ `C/T`, ncol = 3) + scale_fill_manual(values=c("#f2ad00", "#46acc8")) + 
  ylab("Total Pigment per gram Wet Weight") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


##### LENGTH MAP ##### (just cool looking)
#longitude data transformation
Longdf <- -1 * as.numeric(data$Long)
#loads California map
cali_map <- map_data("state", region = "california")
#plotly tool tip, creates information of pyrosome on hover
data <- data %>%
  mutate(pyrosome_info=paste(
    "Info ", Sample.Information, "\n",
    "Pyrosome ", Pyrosome, "\n",
    "Length: ", Length..cm., sep="")
  )
#plots map
length_map <- ggplot() + geom_polygon(data = cali_map, aes(x = long, y = lat, group = group), fill = "darkgrey") +
  coord_fixed(1.3, xlim = c(-131.5, -117), ylim = c(33,38)) +
  geom_point(data = data, aes(x = Longdf, y = Lat, size = Length..cm., 
                              color = Length..cm., text = pyrosome_info), alpha = 0.2) +
  scale_size_continuous(range=c(0.1,10)) + 
  scale_color_viridis(option="magma", trans="log") +
  theme(legend.position = "right")
#adds tooltip text
length_map <- ggplotly(p, tooltip="text")
length_map


##### CRUISE MAP #####
#longitude and latitude data transformation
blongdf <- -1 * as.numeric(bongo_data$long)
blatdf <- as.numeric(bongo_data$lat)
bongo_data$Cycle <- factor(bongo_data$Cycle,levels = c("3", "CCT", "2", "1", "AT"))
#loads terrain map
cruiseMap <- get_stamenmap(bbox = c(left = -132, bottom = 33, right = -117, top = 40), 
                       maptype = "terrain", zoom = 4)
#plots cruise locations
ggmap(cruiseMap) + geom_point(data=bongo_data, aes(x=blongdf, y=blatdf, fill = `Cycle`), size = 5, shape = 21, alpha = 0.9) + 
  scale_fill_viridis(discrete = TRUE)  + xlab("longitude") + ylab("latitude") + 
  theme(axis.text.x=element_text(size=12)) + theme(axis.text.y=element_text(size=12))


##### ANOVAS #####
#LENGTH BY CYCLE/TRANSECT
#one way anova
one.way.len <- aov(data$Length..cm.~ data$C.T , data = data)
summary(one.way.len) #Cycle 3 shorter than all
#tukey plot
tukey.plot.aov.len <- one.way.len
tukey.plot.test.len <-TukeyHSD(tukey.plot.aov.len)
plot(tukey.plot.test.len, las = 1)
tukey.plot.test.len

#GUT PIGMENT PER GRAM WET WEIGHT
#one way anova
one.way.gut <- aov(data$Total.pigs.g.WW~ data$C.T , data = data)
summary(one.way.gut) #Cycle 1 > Cycle 2, Cycle 3 < all other cycles and transects
#tukey plot
tukey.plot.aov.gut<- one.way.gut
tukey.plot.test.gut<-TukeyHSD(tukey.plot.aov.gut)
plot(tukey.plot.test.gut, las = 1)
tukey.plot.test.gut

#JULY-AUG DATA (is it still significant?)
#one way anova
one.way.gut.aug <- aov(jul_aug_data$Total.pigs.g.WW ~ jul_aug_data$C.T, 
                       data = jul_aug_data)
summary(one.way.gut.aug) #Cycle 1 > Cycle 2 still
#tukey plot
tukey.plot.aov.gut.aug <- one.way.gut.aug
tukey.plot.test.gut.aug <-TukeyHSD(tukey.plot.aov.gut.aug)
plot(tukey.plot.test.gut.aug, las = 1)
tukey.plot.test.gut.aug

#TOTAL GUT PIGMENT PER PYROSOME
#one way anova
one.way.tot <- aov(data$Total.pigs.per.colony ~ data$C.T, data = data)
summary(one.way.tot)
#tukey plot
tukey.plot.aov.tot <- one.way.tot
tukey.plot.test.tot<-TukeyHSD(tukey.plot.aov.tot)
plot(tukey.plot.test.tot, las = 1)
tukey.plot.test.tot

#LENGTH BY DAY AND NIGHT
#two way anova
two.way.len <- aov(data$Length..cm.~ data$C.T:data$Day.Night, data = data)
summary(two.way.len) #Cycle 2 Night > Cycle 2 Days
#tukey plot
tukey.plot.aov.two.len<- two.way.len
tukey.plot.test.two.len<-TukeyHSD(tukey.plot.aov.two.len)
plot(tukey.plot.test.two.len, las = 1)
tukey.plot.test.two.len

#GUT PIGMENT PER WET WEIGHT BY DAY AND NIGHT
#two way anova
two.way.pig <- aov(data$Total.pigs.g.WW~ data$C.T:data$Day.Night, data = data)
summary(two.way.pig) #Cycle 1 Night > Cycle 1 Day
#tukey plot
tukey.plot.aov.two.pig<- two.way.pig 
tukey.plot.test.two.pig<-TukeyHSD(tukey.plot.aov.two.pig)
plot(tukey.plot.test.two.pig, las = 1)
tukey.plot.test.two.pig


data <- read.csv("Batting.csv")
head(data , 7)
str(data)
#call the head of the first 5 rows of the AB (at bats) column
head(data$AB , 5)
head(data$X2B , 5)

data$batting.avg <- data$H / data$AB
tail(data$batting.avg , 5)
data$OBP <- (data$H + data$BB + data$HBP) / (data$AB + data$BB + data$HBP + data$SF)
data$X1B <- (data$H - data$X2B - data$X3B - data$HR)
data$slug <- ((1 * data$X1B) + (2 * data$X2B) + (data$X3B * 3) + (data$HR * 4)) / (data$AB)
str(data)

salaries <- read.csv("Salaries.csv")

summary(data)
#see that we have data all the way back from 1871...too much 

data <- subset(data , yearID >= 1985)
summary(data)
#now just the data from 1985 and more recent

#now merging the dataframes together 
combo.data <- merge(data , salaries , by = c("playerID" , "yearID"))
head(combo.data)

lost.players <- subset(combo.data , playerID %in% c('giambja01','damonjo01','saenzol01'))
lost.players <- subset(lost.players , yearID == 2001)
lost.players <- select(lost.players , playerID,H,X2B,X3B,HR,OBP,slug,batting.avg,AB)
#grabs only the certain columns we want 

#now we are going to look for replacement and undervalued players with these conditions:
#1469 AB , average OBP of 0.364 and salary 15 mil
sum(lost.players$AB)
mean(lost.players$OBP)

combo.data <- subset(combo.data , yearID == 2001)
combo.data

ggplot(combo.data , aes(x = OBP , y = salary)) + geom_point()
combo.data <- subset(combo.data , salary < 8000000 & OBP > 0 & AB >= 450)

options <- head(arrange(combo.data , desc(OBP)),10)
options <- select(options , AB , salary , OBP)
options
#can find 3 replacements from this data now 







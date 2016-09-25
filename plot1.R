# Assignment Week 1
#setwd("E:/Biblioteca/Exploratory Coursera/Week1")
# Plot 1
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

d <- read.csv(file = "household_power_consumption.txt", sep=";")
d <- transform(d, Date = as.Date(Date,format="%d/%m/%Y"))
d <- rbind( d[(d$Date == "2007-02-01"),],d[(d$Date == "2007-02-02"),])

dkw <-as.data.frame.numeric(  d[as.character(d$Global_active_power) != "?" ,"Global_active_power"] )
names(dkw)[1] = "kw"

dkw <- cbind(dkw, as.numeric.factor(dkw$kw) )
names(dkw)[2] = "num_kw"

hist(dkw$num_kw, 
    col = "red", 
    main="Global Active Power",
    ylab="Frequency", 
    xlab = "Global Active Power (kilowatts)")

dev.copy(png, file="plot1.png",width=480, height=480)
dev.off()
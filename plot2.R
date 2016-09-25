# Assignment Week 1
# setwd("E:/Biblioteca/Exploratory Coursera/Week1")
# Plot 2
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

d <- read.csv(file = "household_power_consumption.txt", sep=";")
d <- transform(d, Date = as.Date(Date,format="%d/%m/%Y"))
d <- rbind( d[(d$Date == "2007-02-01"),],d[(d$Date == "2007-02-02"),])


d <- transform(d, DateTime = strftime(paste(Date, Time) ,format="%Y-%m-%d %T"))

d <- transform(d, Time = as.character.factor(Time))

d <- transform(d, Time = strftime(paste(Date,Time,sep=" "),format="%D%T", tz=""))

dkw <-as.data.frame(  d[ as.character(d$Global_active_power) != "?" ,
                         c("DateTime","Global_active_power") ] )
names(dkw)[2] = "kw"

dkw <- cbind(dkw, as.numeric.factor(dkw$kw) )
names(dkw)[3] = "num_kw"

dkw$dat <- as.POSIXct(dkw$DateTime)

plot(dkw$dat , 
    dkw$num_kw,
    type = "l", 
    main= "",
    xlab= "", 
    ylab = "Global Active Power (kilowatts)")

dev.copy(png, file="plot2.png",width=480, height=480)
dev.off()
# Assignment Week 1
# setwd("E:/Biblioteca/Exploratory Coursera/Week1")
# Plot 3

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
d <- read.csv(file = "household_power_consumption.txt", sep=";")
d <- transform(d, Date = as.Date(Date,format="%d/%m/%Y"))

#Aplica o filtro de data
d <- rbind( d[(d$Date == "2007-02-01"),],d[(d$Date == "2007-02-02"),])

d <- transform(d, DateTime = strftime(paste(Date, Time) ,format="%Y-%m-%d %T"))

d <- transform(d, Time = as.character.factor(Time))

d <- transform(d, Time = strftime(paste(Date,Time,sep=" "),format="%D%T", tz=""))

d <- cbind(d, Dat <- as.POSIXct(d$DateTime))
names(d)[length(d)] <- "Dat"

dkw <-as.data.frame(  d[ as.character(d$Global_active_power) != "?" ,
                         c("Dat","Sub_metering_1","Sub_metering_2","Sub_metering_3") ] )

dkw <- cbind(dkw, 
             as.numeric.factor(dkw$Sub_metering_1), 
             as.numeric.factor(dkw$Sub_metering_2), 
             as.numeric(dkw$Sub_metering_3))

names(dkw)[ (length(dkw)-2) : length(dkw)] <- c("Sub_metering_1_n","Sub_metering_2_n","Sub_metering_3_n")


plot(dkw$Dat , 
     dkw$Sub_metering_1_n,
     type = "l", 
     main= "",
     xlab= "", 
     ylab = "Energy sub metering")

lines(dkw$Dat , 
      dkw$Sub_metering_2_n,
      col="red")


lines(dkw$Dat , 
      dkw$Sub_metering_3_n,
      col="blue")

legend("topright", 
       c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       lty=c(1,1,1),
       col=c("black","blue","red"),
       cex = 0.5
       )


dev.copy(png, file="plot3.png",width=480, height=480)
dev.off()
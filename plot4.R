# Assignment Week 1
# setwd("E:/Biblioteca/Exploratory Coursera/Week1")
# Plot 4

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
                         c("Dat","Global_active_power","Global_reactive_power","Voltage","Sub_metering_1","Sub_metering_2","Sub_metering_3") ] )

dkw <- transform(dkw, 
                 Global_active_power = as.numeric.factor(dkw$Global_active_power), 
                 Global_reactive_power = as.numeric.factor(dkw$Global_reactive_power), 
                 Voltage = as.numeric.factor(Voltage), 
                 Sub_metering_1 = as.numeric.factor(dkw$Sub_metering_1), 
                 Sub_metering_2 = as.numeric.factor(dkw$Sub_metering_2), 
                 Sub_metering_3 = as.numeric(dkw$Sub_metering_3))

par(mfrow=c(2,2))
par(mar=c(4, 4, 1, 2) + 0.1)


#Plot 1

plot(dkw$Dat , 
     dkw$Global_active_power,
     type = "l", 
     main= "",
     xlab= "", 
     ylab = "Global Active Power")

# Plot 2

plot(dkw$Dat , 
     dkw$Voltage,
     type = "l", 
     main= "",
     xlab= "datetime", 
     ylab = "Voltage")

# Plot 3

plot(dkw$Dat,
     dkw$Sub_metering_1,
     type = "l", 
     main= "",
     xlab= "", 
     ylab = "Energy sub metering")

lines(dkw$Dat , 
      dkw$Sub_metering_2,
      col="red")


lines(dkw$Dat , 
      dkw$Sub_metering_3,
      col="blue")

legend("topright", 
       c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       lty=c(1,1,1),
       col=c("black","blue","red"),
       cex = 0.50
)

# Plot 4
plot(dkw$Dat , 
     dkw$Global_reactive_power,
     type = "l", 
     main= "",
     xlab= "datetime", 
     ylab = "Global_reactive_power")



dev.copy(png, file="plot4.png",width=480, height=480)
dev.off()
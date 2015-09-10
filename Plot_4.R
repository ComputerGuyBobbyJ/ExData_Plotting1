# class.coursera.org/exdata-032
#   Exploratory Data Analysis
# Class Project 1
#   R script to read txt data file, extract data for period 1/2/2007 to 2/2/2007
#   Prepare data frame containing selected data
#   Generate exploratory data plots
# Plot 4
#   Containing four sub-plots

fileName <-"household_power_consumption.txt"  #name of file to read
dateList <-  "1/2/2007;2/2/2007"              #list of dates to be plotted

print("Plot 4 - reading file and creating data frame")
lineCount  <- 0 #count all lines read from file
rowCount   <- 0 #count number of rows added to data frame
con <- file(fileName,"r")
while(length(line <- readLines(con,1)) > 0){  #read all the lines in the data file
    lineCount <- lineCount + 1                #count each line in file
    lineData  <- unlist(strsplit(line,";"))
    if (lineCount == 1)
    {
        headerLen  <- length(lineData)
        df <- as.data.frame(setNames(replicate(headerLen,character(1), simplify = F), lineData), stringsAsFactors = FALSE)      
    } else {
        dateField  <- lineData[1]             #get first field which is date
        if (length(grep(dateField, dateList)) > 0) #check to see if dataField in dateList
        {
            df <- rbind(df, lineData)         #add row to data frame
            rowCount <- rowCount + 1          #count number of rows added to data frame
        }
    }
}
close(con)

#
print("formatting data frame")
df <- df[df$Date != character(1),]
#coerce data columns to numeric
for (i in 3:ncol(df)){
    df[,i] <- as.numeric(df[,i])
}
#add DateTime column to data frame, class = POSITIX
DateTime <- strptime(paste(df$Date,df$Time,sep = " "), "%d/%m/%Y %H:%M:%S")
df <- cbind(DateTime,df)

#Plot 4 - mar 4,4,2,1 ; oma 0 0 2 0
print("plotting data")
par(mfrow = c(2, 2), mar = c(4, 4, 1, 1), oma = c(0, 0, 3, 0))

#Sub Plot 1
plot(df$DateTime,df$Global_active_power, type="o", pch="", xlab = "", ylab="Global Active Power")

#Sub Plot 2
plot(df$DateTime,df$Voltage, type="o", pch="", xlab = "datetime", ylab="Voltage")

#Sub Plot 3
with(df, plot(DateTime,Sub_metering_1, type = "n", xlab="", ylab="Energy sub metering"))
with(df, points(DateTime,Sub_metering_1, type="o", pch = "", col="black"))
with(df, points(DateTime,Sub_metering_2, type="o", pch = "", col="red"))
with(df, points(DateTime,Sub_metering_3, type="o", pch = "", col="blue"))
legend("topright",lty=1, col=c("black","red","blue"),legend=c("SubMtr1", "SubMtr2", "SubMtr3"))

#Sub Plot 4
plot(df$DateTime,df$Global_reactive_power, type="o", pch="", xlab = "datetime", ylab="Global_reactive_power")

#copy to png file
dev.copy(png, filename = "plot4.png", width = 480, height = 480)
dev.off()
print("plot finished")
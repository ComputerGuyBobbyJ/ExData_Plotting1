# class.coursera.org/exdata-032
#   Exploratory Data Analysis
# Class Project 1
#   R script to read txt data file, extract data for period 1/2/2007 to 2/2/2007
#   Prepare data frame containing selected data
#   Generate exploratory data plots
# Plot 2
#   Plot Global Active Power verses DataTime

fileName <-"household_power_consumption.txt"  #name of file to read
dateList <-"1/2/2007;2/2/2007"                #list of dates to be plotted

print("Plot 2 - reading file and creating data frame")
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
        if (length(grep(dateField, dateList)) > 0) #check to see if dateField in list of dates
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

#Plot 2
print("plotting data")
plot(df$DateTime,df$Global_active_power, type="o", pch="", xlab = "", ylab="Global Active Power (kilowatts)")
dev.copy(png, filename = "plot2.png", width = 480, height = 480)
dev.off()
print("plot finished")
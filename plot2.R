# class.coursera.org/exdata-032
#   Exploratory Data Analysis
# Class Project 1
#   R script to read txt data file, located in R working directory 
#   Extract data for period 1/2/2007 to 2/2/2007
#   Prepare data frame containing selected data
#   Generate exploratory data plot and store PNG copy in working directory
# Plot 2
#   Plot Global Active Power verses DataTime

fileName <- "household_power_consumption.txt" #name of file to load
dateList <- "1/2/2007;2/2/2007"               #list of dates to be plotted, day/month/year format

print("Plot 2 - reading file and creating data frame")
lineCount  <- 0 #count of all lines read from file
rowCount   <- 0 #count of number of rows added to data frame

#read all the lines in the data file
con <- file(fileName,"r")
while(length(line <- readLines(con,1)) > 0){
    lineCount <- lineCount + 1                #count each line in file
    lineData  <- unlist(strsplit(line,";"))   #split line into individual data fields
    if (lineCount == 1)
    {
        headerLen  <- length(lineData)        #process first line containing column names to create data frame "df"
        df <- as.data.frame(setNames(replicate(headerLen,character(1), simplify = F), lineData), stringsAsFactors = FALSE)      
    } else {
        dateField  <- lineData[1]             #for all other lines get first data field which is date string
        if (length(grep(dateField, dateList)) > 0) #check to see if dateField in dateList
        {
            df <- rbind(df, lineData)         #add row to data frame, if line date in list string
            rowCount <- rowCount + 1          #count number of rows added to data frame
        }
    }
}
close(con)

#
print("Plot 2 - formatting data frame")
df <- df[df$Date != character(1),]
#coerce data columns to numeric
for (i in 3:ncol(df)){
    df[,i] <- as.numeric(df[,i])
}
#add DateTime column to data frame, class = POSITIX
DateTime <- strptime(paste(df$Date,df$Time,sep = " "), "%d/%m/%Y %H:%M:%S")
df <- cbind(DateTime,df)

#Plot 2
print("Plot 2 - plotting data")
plot(df$DateTime,df$Global_active_power, type="o", pch="", xlab = "", ylab="Global Active Power (kilowatts)")
dev.copy(png, filename = "plot2.png", width = 480, height = 480)
dev.off()
print("Plot 2 - plot finished")
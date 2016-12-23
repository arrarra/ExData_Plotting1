# We use data.table, because it is much faster in reading the input
library(data.table)

readData <- function () {
        datasetURL <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
        zipFile <- 'exdata_data_household_power_consumption.zip'
        datasetFile <- 'household_power_consumption.txt'
        # Download and unzip the dataset if it is not already here.
        if (!file.exists(datasetFile)) {
                download.file(datasetURL, zipFile, method='curl')
                unzip(zipFile)
        }
        stopifnot(file.exists(datasetFile))
        # Read and return the full dataset as a data.table
        allData <- fread(datasetFile, sep = ';', header = TRUE, na.string = '?',
                         stringsAsFactors = FALSE, verbose = TRUE)
        # Select data from days Feb. 1st and 2nd, year 2007. 
        data <- allData[Date == '1/2/2007' | Date == '2/2/2007',]
        # Add a new field `datetime`. It combines fields `Date` and `Time`.
        data[,datetime:= mapply(function (date, time) strptime(paste(date, time), '%d/%m/%Y %H:%M:%S'),
                                Date, Time)]
        data
}

# This function makes `plot1.png`.
plot1 <- function () {
        data <- readData()
        png('plot1.png', bg='transparent', width=480, height=480)
        hist(data$Global_active_power,
             col='red',
             xlab='Global Active Power (kilowatts)',
             xaxp=c(0,6,3),
             main='Global Active Power')
        dev.off()
        data
}

plot1()

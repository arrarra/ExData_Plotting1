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
        data[,datetime:= strptime(paste(data$Date, data$Time), '%d/%m/%Y %H:%M:%S')]
        data
}

# This function makes `plot3.png`.
plot3 <- function () {
        data <- readData()
        png('plot3.png', bg='transparent', width=480, height=480)
        
        # Make a plot with no data
        plot(data$Sub_metering_1 ~ data$datetime, data,
             type = 'n',
             ylab ='Energy sub metering',
             xlab =NA)
        # Draw the X axis tickmarks
        # Add a legend
        legend('topright',
               lty=1, # We want lines in the legend
               legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
               col=c('black', 'red', 'blue'))
        # Draw the data
        with(data, {
                lines(datetime, Sub_metering_1, col='black')
                lines(datetime, Sub_metering_2, col='red')
                lines(datetime, Sub_metering_3, col='blue')
        })
        dev.off()
        data
}

plot3()
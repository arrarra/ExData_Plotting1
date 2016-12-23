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

# This function makes `plot4.png`.
plot4 <- function () {
        data <- readData()
        png('plot4.png', bg='transparent', width=480, height=480)
        par(mfrow = c(2,2))
        # 1. sub plot
        plot(data$Global_active_power ~ data$datetime, data, type='l',
             ylab = 'Global Active Power',
             xaxt = 'n',
             xlab=NA)
        datetimeRange <- range(data$datetime)
        axis(1, at = c(datetimeRange[1], 0.5*(datetimeRange[1]+datetimeRange[2]), datetimeRange[2]),
             labels = c('Thu', 'Fri', 'Sat'))
        # 2. sub plot
        plot(data$Voltage ~ data$datetime, data, type='l',
             ylab = 'Voltage',
             xaxt = 'n',
             xlab='datetime')
        axis(1, at = c(datetimeRange[1], 0.5*(datetimeRange[1]+datetimeRange[2]), datetimeRange[2]),
             labels = c('Thu', 'Fri', 'Sat'))
        # 3. sub plot
        # Make a plot with no data
        plot(data$Sub_metering_1 ~ data$datetime, data,
             type = 'n',
             xaxt = 'n',
             ylab ='Energy sub metering',
             xlab =NA)
        # Draw the X axis tickmarks
        axis(1,
             at = c(datetimeRange[1], 0.5*(datetimeRange[1]+datetimeRange[2]), datetimeRange[2]),
             labels = c('Thu', 'Fri', 'Sat'))
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
        # 4. sub plot
        plot(data$Global_reactive_power ~ data$datetime, data, type='l',
             ylab = 'Global Reactive Power',
             xaxt = 'n',
             xlab='datetime')
        axis(1, at = c(datetimeRange[1], 0.5*(datetimeRange[1]+datetimeRange[2]), datetimeRange[2]),
             labels = c('Thu', 'Fri', 'Sat'))
        dev.off()
        data
}

plot4()
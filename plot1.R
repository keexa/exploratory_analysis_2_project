# 1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008

# Loading data
rm(list = ls())

setwd("/Users/bonifazi/Desktop/exploratory2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


new_data <- subset(NEI, select=c(year, Emissions))

# Summing by year
total_emissions <- as.matrix(aggregate(new_data$Emissions, by=list(new_data$year), sum))
colnames(total_emissions) <- c("year", "Emissions")

# Drawing the plot
png("./plot1.png", width = 480, height = 480)

par(mar = c(7,15,4,4) + 0.1, mgp=c(5,1,0))
mp <- barplot(total_emissions[,2],
              col=rainbow(4),
              cex.axis=0.5,
              axes = FALSE,
              main ="Plot 1 - Total emissions from PM2.5",
              xlab = "Year", ylab = "PM 2.5 emission (tons)")
axis(1, at=mp, labels=total_emissions[,1])

max_unit <- signif(max(total_emissions[,2]), 1)
options(scipen=999)
scales <- seq(from=0, to=max_unit, by=max_unit / 7)
axis(2, at=scales, labels=scales, las=1)

# Saving to png file
dev.off() 
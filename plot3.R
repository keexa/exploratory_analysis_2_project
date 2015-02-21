# 3 - Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#  which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#  Which have seen increases in emissions from 1999–2008? 
#  Use the ggplot2 plotting system to make a plot answer this question.
rm(list = ls())

library("ggplot2")

# Loading data
setwd("/Users/bonifazi/Desktop/exploratory2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

new_data <- subset(NEI[NEI$fips=="24510",], select=c(year, type, Emissions))

# Summing by year
total_emissions <- aggregate(new_data$Emissions, by=list(new_data$year, new_data$type), sum)
colnames(total_emissions) <- c("year", "type", "Emissions")

# Drawing the plot
png("./plot3.png", width = 480, height = 480)

d <- ggplot(data = total_emissions, aes(x= year, y=Emissions, color=type))
d <- d + geom_path(size=1)
d <- d + geom_point(size=5)
d <- d + stat_smooth(method=lm, se=FALSE, size=1, linetype=3)
d <- d + xlab("Year")
d <- d + ylab("PM 2.5 emissions (tons)")
d <- d + ggtitle("Plot 3 - Emissions in Baltimore City by type") + 
  theme(plot.title = element_text(face="bold"))
print(d)

# Saving to png file
dev.off() 
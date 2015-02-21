# 4 - Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
rm(list = ls())

library("stringr")
library("ggplot2")

# Loading data

setwd("/Users/bonifazi/Desktop/exploratory2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

temp_data <- subset(NEI, select=c(SCC, year, Emissions))

# We search for "Coal" and "Comb" at the same time in the short name
matches <- grep("(?=.*Coal)(?=.*Comb)", 
                SCC$Short.Name, value=FALSE, ignore.case=FALSE, perl=TRUE)

temp_scc_data <- SCC[matches, ]

# We merge the two tables
new_data <- merge(temp_data, temp_scc_data, by = c("SCC", "SCC"))

# Summing by year
total_emissions <- aggregate(new_data$Emissions, by=list(new_data$year), sum)
colnames(total_emissions) <- c("year", "Emissions")

# Drawing the plot
png("./plot4.png", width = 480, height = 480)
d <- ggplot(data = total_emissions, aes(x= year, y=Emissions))
d <- d + geom_path(size=1)
d <- d + geom_point(size=5)
d <- d + stat_smooth(method=lm, se=FALSE, size=1)
d <- d + xlab("Year")
d <- d + ylab("PM 2.5 emissions (tons)")
d <- d + ggtitle("Plot 4 - Coal combustion-related sources") + theme(plot.title = element_text(face="bold"))
print(d)

# Saving to png file
dev.off() 
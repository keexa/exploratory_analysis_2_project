# 6 - Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

rm(list = ls())

library("stringr")
library("ggplot2")

# Loading data

setwd("/Users/bonifazi/Desktop/exploratory2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

temp_data <- subset(NEI[NEI$fips=="24510"| NEI$fips == "06037",], select=c(SCC, fips, year, Emissions))

matches <- grep("(?=.*On-Road)(?=.*Vehicles)", 
                SCC$EI.Sector, value=FALSE, ignore.case=FALSE, perl=TRUE)

# Here we adjust the SCC column as sometimes the values contain some extra characters at the end
temp_scc_data <- SCC[matches,]

new_data <- merge(temp_data, temp_scc_data, by = c("SCC", "SCC"))

# Summing by year
total_emissions <- aggregate(new_data$Emissions, by=list(new_data$fips, new_data$year), sum)
colnames(total_emissions) <- c("fips", "year", "Emissions")
total_emissions$City <- ""
total_emissions[total_emissions$fips=="24510",]$City <- "Baltimore City"
total_emissions[total_emissions$fips=="06037",]$City <- "Los Angeles"
# Drawing the plot
lm(Emissions ~ year, City=="Baltimore City", data=total_emissions)
lm(Emissions ~ year, City=="Los Angeles", data=total_emissions)

png("./plot6.png", width = 480, height = 480)
d <- ggplot(data = total_emissions, aes(x= year, y=Emissions,
                                        group=City,
                                        shape=City,
                                        colour=City))
d <- d + geom_path(size=1)
d <- d + geom_point(size=5)
d <- d + stat_smooth(method=lm, se=FALSE, size=1, linetype=3)
d <- d + xlab("Year")
d <- d + ylab("PM 2.5 emissions (tons)")
d <- d + ggtitle("Plot 6 - Emissions from motor vehicle sources") + theme(plot.title = element_text(face="bold"))
print(d)

# Saving to png file
dev.off() 
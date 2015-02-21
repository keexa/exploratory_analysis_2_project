# 5 - How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
rm(list = ls())

library("stringr")
library("ggplot2")

# Loading data

setwd("/Users/bonifazi/Desktop/exploratory2/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

temp_data <- subset(NEI[NEI$fips=="24510",], select=c(SCC, year, Emissions))

matches <- grep("(?=.*On-Road)(?=.*Vehicles)", 
                SCC$EI.Sector, value=FALSE, ignore.case=FALSE, perl=TRUE)

# Here we adjust the SCC column as sometimes the values contain some extra characters at the end
temp_scc_data <- SCC[matches,]

new_data <- merge(temp_data, temp_scc_data, by = c("SCC", "SCC"))

# Summing by year
total_emissions <- aggregate(new_data$Emissions, by=list(new_data$year), sum)
colnames(total_emissions) <- c("year", "Emissions")

# Drawing the plot
png("./plot5.png", width = 480, height = 480)
d <- ggplot(data = total_emissions, aes(x= year, y=Emissions))
d <- d + geom_path(size=1)
d <- d + geom_point(size=5)
d <- d + stat_smooth(method=lm, se=FALSE, size=1)
d <- d + xlab("Year")
d <- d + ylab("PM 2.5 emissions (tons)")
d <- d + ggtitle("Plot 5 - Motor vehicle sources in Baltimore") + theme(plot.title = element_text(face="bold"))
print(d)

# Saving to png file
dev.off() 
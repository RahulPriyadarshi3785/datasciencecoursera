#Copy and paste the R code file for the plot uploaded in the previous #question.
#plot5.R
# download data
if (!file.exists("FNEI_data.zip"))
{
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "FNEI_data.zip")
}

files <- unzip("FNEI_data.zip")

# read rds files
SCC <- readRDS(files[1])
NEI <- readRDS(files[2])

library(dplyr)
library(ggplot2)

# filter for Baltimore City
totalEmissionsPerYearBaltimoreCitySCC <- NEI %>%
filter(fips == "24510")

# merge data sets on SCC id and select year, emissions and type
totalEmissionsPerYearBaltimoreCityMerged <- totalEmissionsPerYearBaltimoreCitySCC %>%
inner_join(SCC, by = "SCC") %>% select(year, Emissions, type)

# summarize on type
totalEmissionsPerYearBaltimoreCitySCCType <- totalEmissionsPerYearBaltimoreCityMerged %>%
group_by(type, year) %>%
summarize_if(is.numeric, sum) %>%
ungroup()

totalEmissionsPerYearBaltimoreCitySCCType$type <- factor(totalEmissionsPerYearBaltimoreCitySCCType$type)

# fit linear model to draw the trend
trends <- with(totalEmissionsPerYearBaltimoreCitySCCType,
by(totalEmissionsPerYearBaltimoreCitySCCType, type,
function(x)
{
lm(x[[3]] ~ x[[2]])
}))

# plot year on x and total emissions of Baltimore City on y + lm of data
ggplot(totalEmissionsPerYearBaltimoreCitySCCType, aes(year, Emissions, color = type)) +
geom_line() +
geom_smooth(method = 'lm', se = FALSE)
ylab("emissions in Baltimore City (tons)")
ggsave("plot3.png")


=======================================================================

# download data
if (!file.exists("FNEI_data.zip"))
{
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "FNEI_data.zip")
}

files <- unzip("FNEI_data.zip")

# read rds files
SCC <- readRDS(files[1])
NEI <- readRDS(files[2])

library(dplyr)
library(ggplot2)

# merge data sets on SCC id and select year, emissions and el.sector
# then filter by coal sector, group by year and summarize emissions
emissionByCoalSectorAndYear <- NEI %>%
inner_join(SCC, by = "SCC") %>%
select(year, Emissions, EI.Sector) %>%
filter(grepl("Coal", EI.Sector)) %>%
group_by(year) %>%
summarize_if(is.numeric, sum) %>%
ungroup()

# plot year on x and total emissions of coal combustion-related sources on y
ggplot(emissionByCoalSectorAndYear, aes(year, Emissions)) +
geom_line() +
ylab("emissions (tons)")
ggsave("plot4.png")

=======================================================================

# download data
if (!file.exists("FNEI_data.zip"))
{
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "FNEI_data.zip")
}

files <- unzip("FNEI_data.zip")

# read rds files
SCC <- readRDS(files[1])
NEI <- readRDS(files[2])

library(dplyr)
library(ggplot2)

# filter by Baltimore City fips
# merge data sets on SCC id and select year, emissions and el.sector
# then filter by mobile vehicles, group by year and summarize emissions
emissionByMotorVehicleSectorAndYearBaltimoreCity <- NEI %>%
filter(fips == "24510") %>%
inner_join(SCC, by = "SCC") %>%
select(year, Emissions, EI.Sector) %>%
filter(grepl("Mobile - On-Road", EI.Sector)) %>%
group_by(year) %>%
summarize_if(is.numeric, sum) %>%
ungroup()

# plot year on x and total emissions of coal combustion-related sources on y
ggplot(emissionByMotorVehicleSectorAndYearBaltimoreCity, aes(year, Emissions)) +
geom_line() +
ylab("emissions in Baltimore City (tons)")
ggsave("plot5.png")

=======================================================================


# download data
if (!file.exists("FNEI_data.zip"))
{
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "FNEI_data.zip")
}

files <- unzip("FNEI_data.zip")

# read rds files
NEI <- readRDS(files[2])

library(dplyr)

# filter for Baltimore City, select year and emissions, group them by year and summarize them
totalEmissionsPerYearBaltimoreCity <- NEI %>%
filter(fips == "24510") %>%
select(year, Emissions) %>%
group_by(year) %>%
summarize_if(is.numeric, sum) %>%
ungroup()

# fit linear model to draw the trend
trend <- lm(totalEmissionsPerYearBaltimoreCity$Emissions ~ totalEmissionsPerYearBaltimoreCity$year)

# plot year on x and total emissions of Baltimore City on y + lm of data
png("plot2.png")
plot(totalEmissionsPerYearBaltimoreCity$year, totalEmissionsPerYearBaltimoreCity$Emissions, type = "l", xlab = "year", ylab = "emissions in Baltimore City (tons)")
abline(trend, lwd = 2)
dev.off()

=======================================================================


# download data
if (!file.exists("FNEI_data.zip"))
{
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "FNEI_data.zip")
}

# unzip the files
files <- unzip("FNEI_data.zip")

# read rds file NEI
NEI <- readRDS(files[2])

library(dplyr)

# select year and emissions, group them by year and summarize them
totalEmissionsPerYear <- NEI %>%
select(year, Emissions) %>%
group_by(year) %>%
summarize_if(is.numeric, sum) %>%
ungroup()

# fit linear model to draw the trend
trend <- lm(totalEmissionsPerYear$Emissions ~ totalEmissionsPerYear$year)

# plot year on x and total emissions on y + lm of data
png("plot1.png")
plot(totalEmissionsPerYear$year, totalEmissionsPerYear$Emissions, type = "l", xlab = "year", ylab = "emissions (tons)")
abline(trend, lwd = 2)
dev.off

=======================================================================
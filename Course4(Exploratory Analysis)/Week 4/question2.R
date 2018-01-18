question2 <- function(pathdir = "C:/Users/hp/datasciencecoursera/Course4(Exploratory Analysis)/Week 4")
{
  setwd(pathdir)
  dir()
  library(RColorBrewer)
  ## PM2.5 emissions data for 1999, 2002, 2005, and 2008
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  NEI_Baltimore <- subset(NEI, fips == "24510")
  ##head(NEI_Baltimore)
  ##head(NEI_Baltimore)
  ##names(NEI_Baltimore)
  ##names(SCC)
  ##str(NEI_Baltimore)
  ##str(SCC)
  ##table(NEI_Baltimore$year)
  x <- NEI_Baltimore$Emissions
  class(x)
  mean(is.na(x))
  summary(x)
  ## plot.new()
  ##dev.off() ## Reset par function
  png("question2.png", width=480, height=480)
  boxplot(log10(Emissions) ~ year, NEI_Baltimore, xlab = "year",
          ylab = "PM25(in tons)",
          col = brewer.pal(4,"BuGn"), col.axis = "blue", col.lab = "red")
  title(main = "Total PM2.5 by year in Baltimore city", col.main = "green")
  dev.off()
}
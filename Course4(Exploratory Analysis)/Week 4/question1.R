question1 <- function(pathdir = "C:/Users/hp/datasciencecoursera/Course4(Exploratory Analysis)/Week 4")
{
  setwd(pathdir)
  dir()
  ## PM2.5 emissions data for 1999, 2002, 2005, and 2008
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  ##head(NEI)
  ##head(NEI)
  ##names(NEI)
  ##names(SCC)
  ##str(NEI)
  ##str(SCC)
  ##table(NEI$year)
  x <- NEI$Emissions
  class(x)
  mean(is.na(x))
  summary(x)
  ##boxplot(Emissions ~ year, NEI)
  plot.new()
  dev.off() ## Reset par function
  library(RColorBrewer)
  png("question1.png", width=480, height=480)
  boxplot(log10(Emissions) ~ year, NEI, xlab = "year", ylab = "PM25(in tons)",
          col = brewer.pal(4, "BuGn"), col.axis = "blue", col.lab = "red")
  title(main = "Total PM2.5 by year", col.main = "green")
  dev.off()
  }
question3 <- function(pathdir = "C:/Users/hp/datasciencecoursera/Course4(Exploratory Analysis)/Week 4")
{
  setwd(pathdir)
  dir()
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
  ##NEI_Baltimore$type1 <- as.factor(NEI_Baltimore$type)
  ##qplot(type, log10(Emissions), data = NEI_Baltimore, geom = "boxplot", facets = .~year)
  png("question3.png", width=1000, height=480)
  g <- ggplot(NEI_Baltimore, aes(type, log10(Emissions), fill = year))
  g + geom_boxplot(outlier.color = "red") + facet_grid(.~year) +
    labs(x = "Types of Sources", y = "Emissions PM25") +
    ggtitle("                                                                    As Per Year PM25 change")+
    theme(plot.title = element_text(color = "green",size = 14, face = "bold.italic"),
          axis.title = element_text(color = "red", size = 14, face = "bold"),
          axis.text = element_text(color = "blue"))
  dev.off()
}
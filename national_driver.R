library(ggplot2)
library(plyr)
library(readr)
library(rgeos)
library(rgdal)
library(maptools)
library(maps)
library(RColorBrewer)
library(scales)
library(extrafont)

# this is a combined driver to make multiple plots of state primaries and general election results

# choosing the state to analyze
stateAbbreviation <- "MI"

# Loading the data
generalResults <- read_csv("input/general_results.csv")
primaryResults <- read_csv("input/primary_results.csv")

# loading and parsing the shape files for each of the counties
counties <- readOGR(dsn="input/county_shapefiles", layer="cb_2014_us_county_500k")
counties@data$id <- rownames(counties@data)
counties.points <- fortify(counties, region="id")
counties.df <- join(counties.points, counties@data, by="id")
counties.df$fips <- as.integer(paste0(counties.df$STATEFP, counties.df$COUNTYFP))
data(state.fips)

# creating county info datataframe
stateFips <- state.fips$fips[state.fips$abb==stateAbbreviation]
state <- primaryResults[primaryResults$state_abbreviation==stateAbbreviation,]$state[[1]]
stateCounties <- counties.df[counties.df$STATEFP==sprintf("%02d", stateFips),]

# adding general election info
totalResults <- merge(stateCounties, generalResults[generalResults$state_abbreviation==stateAbbreviation,], by="fips")
totalResults <- totalResults[order(totalResults$order),]
totalResults$vote_diff <- totalResults$votes_dem_2016 - totalResults$votes_gop_2016

# adding primary information
primaryResults$class <- "primary"
party <- "Democrat"
totalResults <- merge(totalResults, primaryResults[primaryResults$state_abbreviation==stateAbbreviation & primaryResults$party==party,], by="fips")
totalResults <- totalResults[order(totalResults$order),]

totalResults$trumpvdem <- totalResults$fraction_votes - totalResults$Trump



#HillResults <- statePrimaryResults[statePrimaryResults$candidate=="Hillary Clinton",]
#BernResults <- statePrimaryResults[statePrimaryResults$candidate=="Bernie Sanders",]


#HillResults$percent_diff <- HillResults$fraction_votes - BernResults$fraction_votes
#HillResults$percent_diff <- HillResults$fraction_votes - BernResults$fraction_votes


# Here is a fancy color palette inspired by http://www.colbyimaging.com/wiki/statistics/color-bars
#cool = rainbow(10, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
#warm = rainbow(10, start=rgb2hsv(col2rgb('#ff8888'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
#cols<-c("#34e1fb", "#8dbafb","#b9a1fb","#fb93fc")
cols<-c("#7f0000","#ffffff", "#0000b2")

mypalette <- colorRampPalette(cols)(255)

p <- ggplot(totalResults) + 
  aes(long,lat,group=group,fill=trumpvdem) + 
  geom_polygon() +
  geom_path(color="white") +
  facet_wrap(~candidate) +
  coord_equal() +
  scale_fill_gradientn(name="Dem Win By",
                       colours=mypalette,
                       limits=c(-.5,.4),
                       labels=percent) + 
  theme_light(base_size=16) +
  theme(strip.text.x = element_text(size=14, colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ggtitle(paste("2016", state, "Comparative Performance V Trump"))
ggsave(paste0(state, "_General", ".pdf"), p, height=4, width=7, units="in")

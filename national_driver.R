library(ggplot2)
library(plyr)
library(readr)
library(rgeos)
library(rgdal)
library(maptools)
library(maps)
library(RColorBrewer)
library(scales)

# this is a combined driver to make multiple plots of state primaries and general election results

# choosing the state to analyze
stateAbbreviation <- "GA"

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
totalResults$class <- "general"
totalResults$vote_diff <- totalResults$Clinton - totalResults$Trump

# adding primary information
primaryResults$class <- "primary"

totalResults <- merge(totalResults, primaryResults[primaryResults$state_abbreviation==stateAbbreviation & primaryResults$party==party,], by="fips")
totalResults <- totalResults[order(totalResults$order),]




#HillResults <- statePrimaryResults[statePrimaryResults$candidate=="Hillary Clinton",]
#BernResults <- statePrimaryResults[statePrimaryResults$candidate=="Bernie Sanders",]


#HillResults$percent_diff <- HillResults$fraction_votes - BernResults$fraction_votes
#HillResults$percent_diff <- HillResults$fraction_votes - BernResults$fraction_votes




p <- ggplot(totalResults) + 
  aes(long,lat,group=group,fill=vote_diff) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradientn(name="votes",
                       colours=brewer.pal(21,"BrBG"),
                       limits=c(-1,1),
                       labels=percent) + 
  theme_light(base_size=16) +
  theme(strip.text.x = element_text(size=14, colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ggtitle(paste("2016", state, "General Results"))
ggsave(paste0(state, "_General", ".png"), p, height=4, width=7, units="in")

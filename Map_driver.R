library(ggplot2)
library(plyr)
library(readr)
library(rgeos)
library(rgdal)
library(maptools)
library(maps)
library(RColorBrewer)
library(scales)

stateAbbreviation <- "IA"
party <- "Democrat"

primaryResults <- read_csv("input/primary_results.csv")
generalResults <- read_csv("input/general_results.csv")

counties <- readOGR(dsn="input/county_shapefiles", layer="cb_2014_us_county_500k")
counties@data$id <- rownames(counties@data)
counties.points <- fortify(counties, region="id")
counties.df <- join(counties.points, counties@data, by="id")
counties.df$fips <- as.integer(paste0(counties.df$STATEFP, counties.df$COUNTYFP))

data(state.fips)

stateFips <- state.fips$fips[state.fips$abb==stateAbbreviation]
state <- primaryResults[primaryResults$state_abbreviation==stateAbbreviation,]$state[[1]]
stateCounties <- counties.df[counties.df$STATEFP==sprintf("%02d", stateFips),]

#statePrimaryResults <- merge(stateCounties, primaryResults[primaryResults$state_abbreviation==stateAbbreviation & primaryResults$party==party,], by.x="NAME", by.y="county")
statePrimaryResults <- merge(stateCounties, primaryResults[primaryResults$state_abbreviation==stateAbbreviation & primaryResults$party==party,], by="fips")
statePrimaryResults <- statePrimaryResults[order(statePrimaryResults$order),]
HillResults <- statePrimaryResults[statePrimaryResults$candidate=="Hillary Clinton",]
BernResults <- statePrimaryResults[statePrimaryResults$candidate=="Bernie Sanders",]

HillResults$vote_diff <- HillResults$fraction_votes - BernResults$fraction_votes

p <- ggplot(HillResults) + 
  aes(long,lat,group=group,fill=vote_diff) + 
  geom_polygon() +
  geom_path(color="white") +
  facet_wrap(~candidate) +
  coord_equal() +
  scale_fill_gradientn(name="votes",
                       colours=brewer.pal(11,"BrBG"),
                       limits=c(-.5,.5),
                       labels=percent) + 
  theme_light(base_size=16) +
  theme(strip.text.x = element_text(size=14, colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ggtitle(paste("2016", state, party, "Primary Results"))
ggsave(paste0(state, "_", party, ".png"), p, height=4, width=7, units="in")

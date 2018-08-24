#win_loss
closeAllConnections()
rm(list=ls())

library(ggplot2)
library(plyr)
library(readr)
library(rgeos)
library(rgdal)
library(maptools)
library(maps)
library(RColorBrewer)
library(scales)
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

clintonMisses <- totalResults[ sign(totalResults$Trump-totalResults$Clinton)>0 & sign(totalResults$Obama-totalResults$Romney)>0,]


HillResults <- clintonMisses[clintonMisses$candidate=="Hillary Clinton",]
BernResults <- clintonMisses[clintonMisses$candidate=="Bernie Sanders",]

clintonMisses$vote_diff <- sign(HillResults$fraction_votes - BernResults$fraction_votes)
clintonMisses<-clintonMisses[!duplicated(clintonMisses$county_name),]



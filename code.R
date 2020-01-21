# Libraries
library(sf)
library(tidyverse)
library(maptools)
library(XML)
library(jsonlite)
# Place zip file in data folder

# Identify zip file
zip_file_name <- dir('data')[1]

# Unzip
unzip(paste0('data/', zip_file_name))

# Identify the semantic history
semantic <- paste0('Takeout/Location History/Semantic Location History/')
# Get all the files
semantics <- dir(semantic, recursive = TRUE)
semantic_files <- paste0(semantic, semantics)

read_semantic_file <- function(x){
  system.time(x <- fromJSON(x))
  loc = x$timelineObjects$placeVisit$location
  loc$time = as.POSIXct(as.numeric(x$timelineObjects$placeVisit$duration$startTimestampMs)/1000, 
                        origin = "1970-01-01")
  
  loc$lat = loc$latitudeE7 / 1e7
  loc$lon = loc$longitudeE7 / 1e7
  
  loc <- loc %>% filter(!is.na(lon), !is.na(lat)) %>%
    dplyr::select(time, lat, lon)
  return(loc)
  
  # library(sp)
  # loc.sp = loc
  # coordinates(loc.sp) = ~lon+lat
  # proj4string(loc.sp) = CRS("+proj=longlat +datum=WGS84")
  # return(loc.sp)
}

out_list <- list()
for(i in 1:length(semantic_files)){
  message(i)
  out <- read_semantic_file(semantic_files[i])
  out_list[[i]] <- out
}

joe <- bind_rows(out_list)
joe <- joe %>% arrange(time)
# coordinates(joe) <- ~lon+lat

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify


ggplot(data = joe,
       aes(x = lon,
           y = lat)) +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", color = NA, size=0.5) +
  # coord_map() +
  geom_point(color = 'red', size = 3, alpha =0.6) +
  geom_line(alpha = 0.4) +
  # geom_curve(alpha = 0.5, aes(xend = dplyr::lead(lon, 1),
  #                             yend = dplyr::lead(lat, 1))) +
  transition_time(time) +
  # labs(title = "Date: {frame_time}") +
  transition_reveal(time)
  # shadow_wake(wake_length = .2, size = TRUE, alpha = TRUE, colour = 'blue',
  #             fill = NULL, falloff = "cubic-in", wrap = TRUE,
  #             exclude_layer = NULL, exclude_phase = NULL) +
  # ease_aes('linear')


# plot(joe)

library(gganimate)


# Identify new file
new_file <- filename <- paste0('Takeout/Location History/Location History.kml')

kml <- xmlToList(filename)

tr = kml$Document$Placemark$Track
cc = which(names(tr) == "coord")
coord = t(sapply(kml$Document$Placemark$Track[cc], function(x) scan(text = x, quiet = TRUE)))[,1:2]
when = which(names(tr) == "when")
# convert the "-07:00" into " -0700" with sub:
time = strptime(sub("([+\\-])(\\d\\d):(\\d\\d)$", " \\1\\2\\3",
                    unlist(kml$Document$Placemark$Track[when])), "%Y-%m-%dT%H:%M:%OS %z")


library(sp)
library(spacetime)
library(trajectories)
track = Track(STI(SpatialPoints(coord, CRS("+proj=longlat +ellps=WGS84")), 
                  time))
summary(track)
plot(track, axes = TRUE)





# Read new file
df <- sf::st_read(new_file)
tkml <- maptools::getKMLcoordinates(new_file)

# Extract points
pp <- sp::Line(ktml)

#### LOAD THE REQUIRED LIBRARIES FOR THE SCRIPTS TO WORK

library(sp)
library(rgdal)
library(rgeos)
library(ggplot2)
library(scales)
library(ggmap)
library(lubridate)
library(plyr)
library(hexbin)
library(doMC)
library(reshape2)
library(RColorBrewer)


# GTFS required files are
# - agency.txt
# - stops.txt
# - routes.txt
# - trips.txt
# - stop_times.txt
# - calendar.txt
# - shapes.txt -- optional but required for geo SP package to display

# 7 Classes

# will only make S4 classes for the files according to the required fields of the spec only.

########################################################################
####	 Create S4 classes for each of the files to be imported.	####
########################################################################

setClass("Agency",
			representation ( "data.frame"), 
			contains = "data.frame",
			validity = function(object) {
 	   			if (nrow(object) != nrow(object))
	 			   	stop("length of agency.txt is not equal to '@.Data' extent")
 			   	for (i in c("agency_id","agency_name","agency_url","agency_timezone", "agency_lang")) {
 				   	if (! i %in% object@names) {
 						stop(c(i , " AGENCY is missing. Make sure this is present in the agency.txt file."))
 					}
 			   }
			   return(TRUE)
			}
		)

setClass("Stops", 
			representation ( "data.frame"), 
			contains = "data.frame",
			validity = function(object) {
 	   			if (nrow(object) != nrow(object))
	 			   	stop("length of stops.txt is not equal to '@.Data' extent")
 			   	for (i in c("stop_id", "stop_name", "stop_lat", "stop_lon")) {
 				   	if (! i %in% object@names) {
 						stop(c(i , "STOPS is missing. Make sure this is present in the stops.txt file."))
 					}
 			   }
			   return(TRUE)
			}
		)

setClass("Routes", 
			representation ( "data.frame"), 
			contains = "data.frame",
			validity = function(object) {
 	   			if (nrow(object) != nrow(object))
	 			   	stop("length of routes.txt is not equal to '@.Data' extent")
 			   	for (i in c("route_id", "agency_id", "route_short_name", "route_long_name", "route_type" )) {
 				   	if (! i %in% object@names) {
 						stop(c(i , "ROUTES is missing. Make sure this is present in the routes.txt file."))
 					}
 			   }
			   return(TRUE)
			}
		)

setClass("Trips", 
		representation ( "data.frame"), 
		contains = "data.frame",
			validity = function(object) {
 	   			if (nrow(object) != nrow(object))
	 			   	stop("length of trips.txt is not equal to '@.Data' extent")
 			   	for (i in c("route_id","service_id","trip_id","shape_id","trip_headsign","direction_id")) {
 				   	if (! i %in% object@names) {
 						stop(c(i , "TRIPS is missing. Make sure this is present in the trips.txt file."))
 					}
 			   }
			   return(TRUE)
			}
	)

setClass("StopTimes", 
			representation ( "data.frame"),
			contains = "data.frame",
			validity = function(object) {
 	   			if (nrow(object) != nrow(object))
	 			   	stop("length of stop_times.txt is not equal to '@.Data' extent")
 			   	for (i in c("trip_id","arrival_time", "departure_time","stop_id","stop_sequence","stop_headsign", "pickup_type", "drop_off_type","shape_dist_traveled")) {
 				   	if (! i %in% object@names) {
 						stop(c(i , "StopTIMES is missing. Make sure this is present in the stop_times.txt file."))
 					}
 			   }
			   return(TRUE)
			}
		)


setClass("Shapes", 
		representation ( "data.frame"), 
		contains = "data.frame",
			validity = function(object) {
 	   			if (nrow(object) != nrow(object))
	 			   	stop("length of shapes.txt is not equal to '@.Data' extent")
 			   	for (i in c("shape_id","shape_pt_lat","shape_pt_lon", "shape_pt_sequence", "shape_dist_traveled")) {
 				   	if (! i %in% object@names) {
 						stop(c(i , "SHAPES is missing. Make sure this is present in the shapes.txt file."))
 					}
 			   }
			   return(TRUE)
			}
	)



setClass("Calendar",
		representation ( calendars = "data.frame"),
		contains = "data.frame",
		validity = function(object) {
	 	   			if (nrow(object) != nrow(object))
		 			   	stop("length of '@calendars' not equal to '@.Data' extent")
	 			   	for (i in c("service_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday","start_date", "end_date")) {
	 				   	if (! i %in% object@names) {
	 						stop(c(i , "CALENDAR is missing. Make sure this is present in the calendar.txt file."))
	 					}
	 			   }
				   return(TRUE)
 			   }
	
	)

setClass("CalendarDates", 
			representation ( "data.frame"), 
			contains = "data.frame",
			validity = function(object){
 	   			if (nrow(object) != nrow(object))
	 			   	stop("length of object not equal to '@.Data' extent")
 			   	for (i in c("service_id", "date", "exception_type")) {
 				   	if (! i %in% object@names) {
 						stop(c(i , "CALENDAR_DATES is missing. Make sure this is present in the calendar_dates.txt file."))
 					}
 			   }
			   return(TRUE)
			}
			
		)
		

################################################################################

####	 Create S4 GTFS superclass to hold all the files after validation	####

################################################################################

setClass("GTFS", 
		representation(agency_id = "numeric", agency_name = "factor", agencies="Agency",
			stops="Stops", stop_times = "StopTimes", routes="Routes", trips="Trips",
			calendars = "Calendar", shapes="Shapes", calendar_dates="CalendarDates")
		)


###############################################################################

####	 Function to download the gtfs file to a chosen directory			####

################################################################################

download.gtfs.file <- function (url, path) {
	temp <- tempfile()
	download.file(url, temp)
	unzip(temp, exdir = path)
	unlink(temp)
	#print("Your %url has downloaded and extracted to %path")
	
	paste(" Your ", url, "had been downloaded to ", path)
}

################################################################################

####	 Function to read in each of the gtfs files located in "directory"	####

################################################################################


# Load the gtfs file into R
read.gtfs.file <- function (name, path) {
			  read.csv(file.path(path, name), na.strings = c("", "NA"))
		  }


################################################################################
####	 load in all of the required files into an object of class "GTFS" 	####
################################################################################

gtfs.make <- function(path) {
	# remove the notes from the R CMD check
	route_type <- stop_sequence <- service_id <- start_date <- end_date <- NULL
	
	agency <- new("Agency", read.gtfs.file("agency.txt", path))
	stops <- new("Stops",read.gtfs.file("stops.txt", path))
	
	routes <- new("Routes", transform(
	      read.gtfs.file("routes.txt", path),
	      route_type = factor(
	        route_type, 
	        levels = 0:7, 
	        labels = c("Tram", "Metro", "Rail", "Bus", "Ferry", "Cable car", "Gondola", "Funicular")
	      )
		  ))

	trips <- new("Trips",read.gtfs.file("trips.txt", path))

	stop_times <- new("StopTimes", transform(
	      read.gtfs.file("stop_times.txt", path),
	      stop_sequence = factor(stop_sequence)
		  ))


	calendar <- new("Calendar", transform(
	      read.gtfs.file("calendar.txt", path),
		  service_id = as.factor(service_id),
	      start_date = ymd(start_date),
	      end_date = ymd(end_date)
	    ))

	shapes <- new("Shapes", read.gtfs.file("shapes.txt", path))

	calendar_dates <- new("CalendarDates", transform(
	      read.gtfs.file("calendar_dates.txt", path),
		  service_id = as.factor(service_id),
	      date =  ymd(date)
	    ))
	
	## create a new gtfs object
	new("GTFS", agency_id = agency$agency_id, agency_name = agency$agency_name, 
	agencies=agency, stops=stops, stop_times=stop_times, routes=routes, trips=trips, 
	calendars = calendar, shapes=shapes, calendar_dates=calendar_dates)
	
}


################################################################################

#### This function will compute the bounding/convex hull box of GTFS object ####

################################################################################

gtfs.bbox <- function(gtfs, type = c("bbox", "convex")) {
  type <- match.arg(type)

  # passing coordinate reference system
  points <- data.frame(cbind(gtfs@stops$stop_lat, gtfs@stops$stop_lon))
  coordinates(points) <- ~ X2 + X1 # order the coords as x, then y
  # add string for WGS84
  proj4string(points) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  box <- gConvexHull(points) # str() is your friend
  
  if(type == "bbox") {
    box@bbox      
  } else {
    # str() function is helpful
    data.frame(box@polygons[[1]]@Polygons[[1]]@coords)
  }
}

################################################################################

#### Output a ggplot graphic given a GTFS instance and type of box and zoom level of map 														  #####

################################################################################

gtfs.bbox_plot <- function(gtfs, type = c("bbox", "convex"), zoom_level ) {
	# remove the notes from the R CMD check
	x <- y <- X1 <- X2 <- NULL
	
	#match the arguments and return error if required.
	type <- match.arg(type)
	# get the bbox
	polygons <- gtfs.bbox(gtfs, type)
	
	if (type == "convex") {
			bbox <- coord_map(xlim = extendrange(polygons$x), ylim = extendrange(polygons$y))
			# assign base for the map
			base <- with(polygons, get_map(paste(mean(y), mean(x), sep = " "), zoom = zoom_level, color = "bw"))
		} else {
			bbox <- coord_map(xlim = extendrange(cbind(polygons[[1]],polygons[[3]])), ylim = extendrange(cbind(polygons[[2]],polygons[[4]])))

			#must resassign the polygons as the map will not draw with only 4 points.
			polygons <- data.frame(x=c(polygons[[1]],polygons[[1]],polygons[[3]],polygons[[3]],polygons[[1]]), y=c(polygons[[2]],polygons[[4]],polygons[[4]],polygons[[2]],polygons[[2]]))
			
			# assign base for the map
			base <- with(polygons, get_map(paste(mean(y), mean(x), sep = " "), zoom = zoom_level, color = "bw"))
		}
	stop_points <- data.frame(cbind(gtfs@stops$stop_lat, gtfs@stops$stop_lon))
	return(gtfs@agency_name)
	#agency_ch <- as.character(gtfs@agency_name)
	
	## use ggmap and ggplot to make the plot
	ggmap(base) + 
	  geom_path(data = polygons, aes(x, y, colour = gtfs@agency_name), size = 1) +
	  scale_color_manual(name = "Agency",values = c("blue", "green", "red")) +
	  geom_point(data = stop_points, aes(x = X2, y = X1), size = 2, colour = "red") + # transit stops in black
	  bbox +
	  ggtitle(gtfs@agency_name)
}

################################################################################

#### Return a data frame of all the trips for a given day and GTFS instance.  ##

################################################################################

gtfs.trips <- function(gtfs, date = Sys.time()) {
  d <- trunc(date, "days")

  is.between <- function(x, a, b) { 
    (x >= a) & (b >= x) 
  }
  
  is.available <- function(data, date) {
	  data[[tolower(weekdays(date))]] == 1
  }
  
  calendar.dates <- gtfs@calendar_dates
  calendar <- gtfs@calendars
  trips <- gtfs@trips
  stop.times <- gtfs@stop_times
  routes <- gtfs@routes

  # get list of services basend on the calendar 
  services <- setdiff(
    union(
      calendar.dates[calendar.dates$date == d & calendar.dates$exception_type == 1, c("service_id")],
      calendar[is.available(calendar, date) & is.between(date, calendar$start_date, calendar$end_date), c("service_id")]
    ),
    calendar.dates[calendar.dates$date == d & calendar.dates$exception_type == 2, c("service_id")]
  )
  				  
  # services -> trips -> routes
  t <- trips[trips$service_id %in% services, c("trip_id")]
  r <- unique(trips[trips$service_id %in% services, c("trip_id", "route_id")])
  r <- merge(r, routes[, c("route_id", "route_type")], all.x = T)[, c("trip_id", "route_type")]
  
  # arrival_time, and departure_time have hh24:mm:ss format and value can go over 24 hours
  ret <- within(stop.times[stop.times$trip_id %in% t, ], {
    trip_id <- trip_id
    shape_dist_traveled <- shape_dist_traveled
    stop_sequence <- stop_sequence
    arrival_time <-  d + time_to.seconds(arrival_time)
    departure_time <- d + time_to.seconds(departure_time)
  })

  merge(ret, r, all.x = T)
}

################################################################################

## Given the trips on a given day from gtfs.trips, give summary in data.frame ##

################################################################################

gtfs.trips.summary <- function(trips) {
  s <- trips[order(trips$trip_id, trips$stop_sequence), ]
  s$n <- 1:nrow(s)
  s$n1 <- s$n - 1

  # Warning message can be ignored: column name ‘n’ is duplicated in the result
  # 's' is ordered by trip and stop sequence. Hence previous row to current row contains
  # previous stop in actual trip or last stop from different trip.
  ss <- merge(s, s, by.x = "n", by.y = "n1", all.x = T, sort = F, suffixes = c("", ".x"))
  
  # we only need first or last stop for each trip
  cols <- c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "shape_dist_traveled", "n", "route_type")
  ss <- ss[ss$stop_sequence == 1 | ss$stop_sequence.x == 1 | is.na(ss$stop_sequence.x), cols]
 
  # compute summary
  trips.summary <- with(
    # ss looks like this:
    # trip_id=1 <info about first stop>
    # trip_id=1 <info about last stop>
    # ...
    # we need structure like this:
    # trip_id=1 <info about first stop> <info about last stop>
    merge(ss[ss$stop_sequence == 1, ], ss[ss$stop_sequence != 1, ], by = "trip_id", all.x = T, all.y = F, suffixes = c(".s", ".e")), {
    stops_levels <- levels(factor(union(stop_id.s, stop_id.e)))
    data.frame(
      trip_id = factor(trip_id),
      stop_from = factor(stop_id.s, levels = stops_levels),
      stop_to = factor(stop_id.e, levels = stops_levels),
      start_time = arrival_time.s,
      end_time = departure_time.e,
      travel_time = as.numeric(difftime(arrival_time.e, arrival_time.s, units = "secs")),
      distance = shape_dist_traveled.e / 1000, # in km
      stops = n.e - n.s + 1,
      route_type = route_type.s
    )}
  )
}

################################################################################

## returns aggregated shape data											 ##

################################################################################
# returns aggregated shape data
# route can have many trips
# each trip has shape and shape can be different for each trip:
# - one way streets
# - early and late services have different starting terminal stops

compute.segments <- function(shapes) {
	
	# remove error of no global variable in package check
	shape_pt_lat <- shape_pt_lon <- shape_pt_lat.x <- shape_pt_lon.x <- NULL
	
  s <- shapes[order(shapes$shape_id, shapes$shape_pt_sequence), c("shape_pt_lat", "shape_pt_lon", "shape_dist_traveled")]
  s$n <- 1:nrow(s)
  s$n1 <- s$n - 1

  # Warning message can be ignored: column name ‘n’ is duplicated in the result
  # 's' is ordered by trip and stop sequence. Hence previous row to current row contains
  # previous stop in actual trip or last stop from different trip.
  ss <- merge(s, s, by.x = "n", by.y = "n1", all.x = TRUE, sort = FALSE, suffixes = c("", ".x"))
  ss <- ss[ss$shape_dist_traveled.x > 0, c("shape_pt_lat", "shape_pt_lon", "shape_pt_lat.x", "shape_pt_lon.x")]
  ss.agg <- count(ss[complete.cases(ss), ], .(shape_pt_lat, shape_pt_lon, shape_pt_lat.x, shape_pt_lon.x))
  names(ss.agg) <- c("y1", "x1", "y2", "x2", "count")
  
  ss.agg
}


################################################################################

## helper function to get seconds											 ##

################################################################################

truncDiffTime <- function(s, e, i = 900) {
  floor(as.numeric(difftime(e, s, units = "secs")) / i) * i
}

################################################################################

## helper function to return times as seconds								 ###

################################################################################
time_to.seconds <- function(t) {
  t <- strsplit(as.character(t), ":")
  sapply(t, function(y) sum(as.numeric(y) * c(3600, 60, 1)))
}

################################################################################

## function that returns the route name given a trip_id						 ###

################################################################################
trip.to.route <- function(trip_id, gtfs) {
  trips <- gtfs@trips
  routes <- gtfs@routes
  route_id <- trips[trips$trip_id %in% as.character(trip_id), c("route_id")]
  as.character(routes[routes$route_id %in% route_id, c("route_long_name")])
}


################################################################################

## function to compute some basic timetable stats							 ###

################################################################################

timetable.stats <- function(day, gtfs) {
  list(
    "total.trips"   = length(unique(day$trip_id)),
    "bus.trips"     = if("Bus" %in% day$route_type) length(unique(day[day$route_type == "Bus",   ]$trip_id)),
    "rail.trips"   = if("Rail" %in% day$route_type) length(unique(day[day$route_type == "Train", ]$trip_id)),
    "ferry.trips"   = if("Ferry" %in% day$route_type) length(unique(day[day$route_type == "Ferry", ]$trip_id)),
    
	"bus.longest"   = if("Bus" %in% day$route_type) trip.to.route(day[day$distance == max(day[day$route_type == "Bus",   ]$distance), ]$trip_id, gtfs),
	"rail.longest" = if("Rail" %in% day$route_type) trip.to.route(day[day$distance == max(day[day$route_type == "Train", ]$distance), ]$trip_id, gtfs),
	"ferry.longest" = if("Ferry" %in% day$route_type) trip.to.route(day[day$distance == max(day[day$route_type == "Ferry", ]$distance), ]$trip_id, gtfs)
  )
}




		
		
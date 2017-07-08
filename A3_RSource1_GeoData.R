#import necessary library
#1. For mongodb
library(devtools)
library(rmongodb)

#2. For convert coordinate system
library(sp)
library(rgdal)

#Connect Mongo
mongo <- mongo.create(host = "127.0.0.1", db = "fit5141")

#Create two empty columns - longitude/latitude into mongodb collection - stopfrisked2
ns = "fit5141.stopfrisked2"
if (mongo.is.connected(mongo) == TRUE) {
  query <- mongo.bson.from.list({})
  newCol <- mongo.bson.from.list(list("$set" = list("longitude" = "", "latitude" = "")))
  res <- mongo.update(mongo, ns, criteria = query, objNew = newCol, mongo.update.multi)
}
##############################################################

#new data frame to save all converted records
coordDataFrame <- data.frame(matrix(ncol = 3, nrow = 0))
colNames <- c("Id", "xcoord", "ycoord")
colnames(coordDataFrame) = colNames

#loop every x/y coordinate and convert to long/lat and update the records according to unique id
if (mongo.is.connected(mongo) == TRUE){
  allRecordCursor = mongo.find(mongo, ns)
  #Read all the records
  while (mongo.cursor.next(allRecordCursor)){
    record <- list(mongo.bson.to.list(mongo.cursor.value(allRecordCursor)))
    
    #check if x or y coordinate is blank
    if (nchar(as.character(record[[1]]$xcoord)) > 0 && 
        nchar(as.character(record[[1]]$ycoord)) > 0){
      #Get id for further update
      curId <- record[[1]]$Id
      #set x and y Coordinate
      xCoord <- record[[1]]$xcoord
      yCoord <- record[[1]]$ycoord
      
      #save into data frame : id, xcoord, ycoord
      coordDataFrame[nrow(coordDataFrame) + 1, ] <- c(curId, xCoord, yCoord)
    }
  }
}

#Add two new columns in coordDataFrame to save the new converted Geodata - Longitude, latitude
coordDataFrame["longitude"] = NA
coordDataFrame["latitude"] = NA
##############################################################

#Define the function to convert 
# from: x/y coordinate (1983 State Plane Coorindate System - feet)
#       Long Island Zone (3104)
# to  : Longitude/Latitude
convertCoord = function(x, y) {
  nad83_coords = data.frame(xCoord = x, yCoord = y)
  coordinates(nad83_coords) = c("xCoord", "yCoord")
  #EPSG 2908: NAD83(HARN) / New York Long Island (ftUS)
  proj4string(nad83_coords) = CRS("+init=epsg:2908")
  #EPSG 4326: GeodeticCRS (geographic 2D) - World
  result = spTransform(nad83_coords, CRS("+init=epsg:4326"))
  return(result)
}

#loop all records in coordDataFrame and converted to longitude and latitude and save
for (i in 1:nrow(coordDataFrame)){
  xCoord <- coordDataFrame[i,]$xcoord
  yCoord <- coordDataFrame[i,]$ycoord
  
  longLatDataFrame <- convertCoord(xCoord, yCoord)
  #the xCoord will converted to longitude, and yCoord will converted to latitude
  coordDataFrame[i,]$longitude <- longLatDataFrame[1,]$xCoord
  coordDataFrame[i,]$latitude <- longLatDataFrame[1,]$yCoord
}
####################################################################
#save the longitude/latitude data and further visualize in Tableau
if (mongo.is.connected(mongo) == TRUE) {
  
  for(i in 1:nrow(coordDataFrame)){
    query <- mongo.bson.from.list(list("Id" = coordDataFrame[i,]$Id))
    updateLongLat <- mongo.bson.from.list(
      list("$set" = list("longitude" = coordDataFrame[i,]$longitude, 
                         "latitude" = coordDataFrame[i,]$latitude)))
    mongo.update(mongo, ns, criteria = query, objNew = updateLongLat)
    
  }
}

#export command 
#./bin/mongoexport --db fit5141 --collection stopfrisked2 --type=csv 
#--fieldFile ./data/FieldFile.txt --out ./data/stopfrisked2.csv

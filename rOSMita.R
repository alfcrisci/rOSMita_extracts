library(rgdal)
library(rwikidata)
library(wikipediatrend)
library(jsonlite)
library(osmar)
require(maptools)  # spCbind
library(geojsonio)
library(rgeos)
library(httr)
library(downloader)
#########################################################################################
setwd("/home/alf/Scrivania/lav_ubu_osm")


################################################################################################################à
# read data as spatial object:

data(comuni_ita_bounds)



get_bbox_ita_comuni=function(nameadm3="venezia"){
  data(comuni_ita_bounds)
  index=grep(paste0("^",nameadm3,"$"),comuni_ita_bounds[,2],ignore.case=T)
  res=matrix(0,2,2)
  row.names(res)<-c("x","y")
  names(res)<-c("min","max")
  res[2,1]=comuni_ita_bounds$minlat[index]
  res[1,1]=comuni_ita_bounds$minlon[index]
  res[2,2]=comuni_ita_bounds$maxlat[index]
  res[1,2]=comuni_ita_bounds$maxlon[index]
  return(res)
}


##########################################################################################################

get_OSMita_json=function (nameadm3,key="tourism",value="museum",osmelem="node") {
  require(RCurl)
  index=grep(nameadm3,comuni_ita_bounds[,2],ignore.case=T)
  myQuery <- paste0(osmelem,"(",comuni_ita_bounds$minlat[index],",",comuni_ita_bounds$minlon[index],",",comuni_ita_bounds$maxlat[index],",",comuni_ita_bounds$maxlon[index],")[",key,"=",value,"];out%20body;")
  myJSONQuery<- paste("[out:json];", myQuery, sep = "")
  myURL <- paste("http://overpass-api.de/api/interpreter?data=", myJSONQuery, sep = "")
  temp=getURL(myURL,.encoding = "UTF-8")
  return(temp)    
}

# get_ex_overpass_json("venezia")



##########################################################################################################



ita_ex_node_overpass=function(nameadm3="venezia",expr="place=*"){
  require(osmar)
  require(downloader)
  t_bbox=get_bbox_ita_comuni(nameadm3)
  myURL <- paste0("http://overpass-api.de/api/xapi?node[bbox=",t_bbox[1,1],",",t_bbox[2,1],",",t_bbox[1,2],",",t_bbox[2,2],"][",expr,"]")
  download(myURL,dest=nameadm3, mode="wb")
  ciao_osm=get_osm(complete_file(), source = osmsource_file(nameadm3))
  return(ciao_osm)
}


##########################################################################################################

ita_ex_way_overpass=function(nameadm3="venezia",expr="buildings=*"){
  require(osmar)
  require(downloader)
  t_bbox=get_bbox_ita_comuni(nameadm3)
  myURL <- paste0("http://overpass-api.de/api/xapi?way[bbox=",t_bbox[1,1],",",t_bbox[2,1],",",t_bbox[1,2],",",t_bbox[2,2],"][",expr,"]")
  download(myURL,dest=file, mode="wb")
  ciao_osm=get_osm(complete_file(), source = osmsource_file(file))
  return(ciao_osm)
}

##########################################################################################################


retrieve_node_overpass=function(file,bbox,expr="place=*"){
  require(osmar)
  require(downloader)
  myURL <- paste0("http://overpass-api.de/api/xapi?node[bbox=",bbox[1,1],",",bbox[2,1],",",bbox[1,2],",",bbox[2,2],"][",expr,"]")
  download(myURL,dest=file, mode="wb")
  ciao_osm=get_osm(complete_file(), source = osmsource_file(file))
  return(ciao_osm)
}

##########################################################################################################



retrieve_way_overpass=function(file,bbox,expr="buildings=*"){
  require(osmar)
  require(downloader)
  myURL <- paste0("http://overpass-api.de/api/xapi?way[bbox=",bbox[1,1],",",bbox[2,1],",",bbox[1,2],",",bbox[2,2],"][",expr,"]")
  download(myURL,dest=file, mode="wb")
  ciao_osm=get_osm(complete_file(), source = osmsource_file(file))
  return(ciao_osm)
}


##########################################################################################################

get_osm_ITA_osm=function(nameadm3="venezia"){
  require(osmar)
  require(downloader)
  
  myURL=paste0("http://osm-toolserver-italia.wmflabs.org/estratti/comuni/osm/",
               as.character(comuni_ita_bounds$name[grep(nameadm3,comuni_ita_bounds[,2],ignore.case=T)]),
               "---",
               as.character(comuni_ita_bounds$id_adm_ITA[grep(nameadm3,comuni_ita_bounds[,2],ignore.case=T)]),
               ".osm.zip")
  
  filename=paste0(as.character(comuni_ita_bounds$name[grep(nameadm3,comuni_ita_bounds[,2],ignore.case=T)]),
                  "---",
                  as.character(comuni_ita_bounds$id_adm_ITA[grep(nameadm3,comuni_ita_bounds[,2],ignore.case=T)]),
                  ".osm")
  
  download(myURL, dest="dataset.zip", mode="wb") 
  Sys.sleep(1)
  unzip("dataset.zip", exdir = "./")
  file_osm=get_osm(complete_file(), source = osmsource_file(filename))
  return(file_osm)
}


##########################################################################################################


overpassJsonToDataframe <- function(jsonstring) {
  myDF <- data.frame(type = "", id = "", lat = "", lon = "", stringsAsFactors = FALSE)
  myDF <- myDF[-1, ]
  for (node in jsonstring$elements) {
    myDF[nrow(myDF) + 1, ] <- c(node$type, node$id, node$lat, node$lon)
  }
  myDF$lat <- as.numeric(myDF$lat)
  myDF$lon <- as.numeric(myDF$lon)
  return(myDF)
}



# http://rstudio-pubs-static.s3.amazonaws.com/12696_9fd49fb7055c40ff9b3a3ea740e13ab3.html
# http://wiki.openstreetmap.org/wiki/Overpass_API/Overpass_API_by_Example

######################################################################################################################

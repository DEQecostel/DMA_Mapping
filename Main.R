##Master DMA Mapping Script

# Load Packages ------------------------------------------------------------
library(sf)
library(lwgeom)
library(plyr)
library(dplyr)
library(stringr)
library(readxl)

# Input Data ------------------------------------------------------------

paths <- readxl::read_excel(path = "gis_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            col_types = c('text', 'text', 'text',
                                          'text', 'text', 'text'))

#County GIS directory name
gis_dir <- paths$gis_dir_path[1]
#County GIS file names
taxlot_name <- "Baker_Taxlot01withTable"
zoning_name <- "zoning_union"
public_name <-  "public_union"
citylim_name <-  "citylim_union"
#tribe_name <-
county_name <- "BakerCounty"

#Main GIS directory name
gis_main <- paths$gis_main_path[1]
#Main GIS file names
road_name <- "roads_2017_300ftBuffer"
rail_buff_name <- "railroads_2017_200ftBuffer"
rail_line_name <- "railroads"

#Look Up Table directory name
LU_dir <-  paths$LU_dir_path[1]
#Look Up Table names
LU_owner_name <- "TaxlotOwners.csv"
LU_zoning_name <- "ZoningClassification.csv"
LU_nlcd_name <- "LandcoverClassification.csv"
LU_rail_name <- "railroads.csv"
LU_roads_name <- "roads.csv"
LU_DMAs_name <- "DMAs.csv"

#County specific inputs
#Name of county
countyname <- "Baker County"
#Are there gaps in the tax lots shapefile? (usually roads and waterways) (yes=TRUE, no=FALSE)
gaps <- FALSE
#Tribal land in county? (yes=TRUE, no=FALSE)
tribal <- FALSE
#is zoning info available for the county? (yes=TRUE, no=FALSE)
zcodes <- TRUE
#Is the public land management data from 2015 or 2019?
pubyear <- 2019
#Name of column with tax lot owner name information
owner_col <- "OwnerLine1"


# Read in data ---------------------------------------------------------

# Read in GIS Data
taxlots<-st_read(gis_dir,taxlot_name, stringsAsFactors = FALSE)
zoning<-st_read(gis_dir, zoning_name, stringsAsFactors = FALSE)
public<-st_read(gis_dir,public_name, stringsAsFactors = FALSE)
citylim<-st_read(gis_dir,citylim_name, stringsAsFactors = FALSE)
#tribe<-st_read(gis_dir,tribe_name, stringsAsFactors = FALSE)
county <- st_read(gis_dir, county_name, stringsAsFactors = FALSE)
roads_buff <- st_read(gis_main,road_name, stringsAsFactors = FALSE)
rail_buff <- st_read(gis_main,rail_buff_name, stringsAsFactors = FALSE)
rail_line <- st_read(gis_main, rail_line_name, stringsAsFactors = FALSE)


# Read in Look Up Tables
LU_owner <- read.csv(paste0(LU_dir, "/", LU_owner_name), header=TRUE, sep=",", stringsAsFactors = FALSE)
LU_zoning<- read.csv(paste0(LU_dir, "/", LU_zoning_name), header=TRUE, sep=",", stringsAsFactors = FALSE)
LU_nlcd <- read.csv(paste0(LU_dir, "/", LU_nlcd_name), header=TRUE, sep=",", stringsAsFactors = FALSE)
LU_rail <- read.csv(paste0(LU_dir, "/", LU_rail_name), header=TRUE, sep=",", stringsAsFactors = FALSE)
LU_roads <- read.csv(paste0(LU_dir, "/", LU_roads_name), header=TRUE, sep=",", stringsAsFactors = FALSE)
LU_DMAs<- read.csv(paste0(LU_dir, "/", LU_DMAs_name), header=TRUE, sep=",", stringsAsFactors = FALSE)


# Clean Data ---------------------------------------------------------------

##ROADS
#Clip the roads buffer to the county outline
roads_buff <- st_transform(roads_buff, st_crs(taxlots))
county <- st_transform(county, st_crs(taxlots))
roads_buff_clip <- st_intersection(roads_buff, county)
st_write(roads_buff_clip, dsn = gis_dir, "roads_clip.shp", driver = "ESRI Shapefile", update = TRUE)

##Railroads
#Clip the railroads layer to the county outline
rail_buff <- st_transform(rail_buff, st_crs(county))
rail_line <- st_transform(rail_line, st_crs(county))
rail_buff_clip <- st_intersection(rail_buff, county)
st_write(rail_buff_clip,dsn = gis_dir, "rail_clip.shp", driver = "ESRI Shapefile", update=TRUE)

#Clip the rail lines to the county outline
rail_line_clip <- st_intersection(rail_line, county)
#filter
rail_line_clip <- rail_line_clip[,c("RR_NAME")]

##TAXLOTS##
if(zcodes==TRUE){
  #Subset tax lots attribute table to include unique tax lot identifier (APN), tax lot type, owner name and NLCD information.
  t<-taxlots[,c("Taxlot", "MapTaxlot", owner_col,"PrpClsDsc", "PrpClass", "NLCD", "NLCD_Type")]
  #buffer tax lots
  t<-st_buffer(t, 0.0)
  #set precision
  t<-st_set_precision(t, 100000)
  #make valid
  t<-st_make_valid(t)
  #join rail intersect data to each taxlot
  rail_line_clip <- st_transform(rail_line_clip, st_crs(t))
  t <- st_join(t, rail_line_clip)
  #subset attributes
  t <- t[,c("Taxlot", "MapTaxlot", owner_col, "PrpClsDsc", "PrpClass","NLCD", "NLCD_Type", "RR_NAME")]
  #rename columns
  t <- dplyr::rename(t, c("OwnerName" = all_of(owner_col), "RailInt" = "RR_NAME"))
  #export as shp to save progress
  st_write(t,dsn = gis_dir, "t.shp", driver = "ESRI Shapefile", update=TRUE)
}

if(zcodes==FALSE){
  #Subset tax lots attribute table to include unique tax lot identifier (APN), tax lot type, owner name and NLCD information.
  t<-taxlots[,c("Taxlot", "MapTaxlot", owner_col,"PrpClsDsc", "PrpClass", "NLCD", "NLCD_Type")]
  #buffer tax lots
  t<-st_buffer(t, 0.0)
  #set precision
  t<-st_set_precision(t, 100000)
  #make valid
  t<-st_make_valid(t)
  #join rail intersect data to each taxlot
  rail_line_clip <- st_transform(rail_line_clip, st_crs(t))
  t <- st_join(t, rail_line_clip)
}

##ZONING##
if(zcodes==TRUE){
  #Filter zoning attribute table, keep orZCode and orZDesc.
  z<-zoning[,c("orZCode", "orZDesc")]
  #transform
  z <- st_transform(z,st_crs(t))
  #buffer
  z<-st_buffer(z, 0.0)
  #set precision
  z<-st_set_precision(z, 100000)
  #make valid
  z<-st_make_valid(z)
  #export as shp to save progress
  st_write(z,dsn = gis_dir, "z.shp", driver = "ESRI Shapefile", update=TRUE)
}

##CITY LIMITS##
#Filter city limit attribute table, keep CITY_NAME
c<-citylim[,"CITY_NAME"]
c <- plyr::rename(c, c("CITY_NAME"="CityName"))
c <- st_transform(c,st_crs(t))
#buffer
c<-st_buffer(c, 0.0)
#set precision
c<-st_set_precision(c, 100000)
#make valid
c<-st_make_valid(c)
#save as shp
st_write(c,dsn = gis_dir, "c.shp", driver = "ESRI Shapefile", update=TRUE)


##PUBLIC LAND MANAGEMENT##
#if data is from 2019, rename land manager column name
if (pubyear==2019){
  names(public)[names(public) == 'PROPERTY_S'] <- 'LandManage'
}
#Filter public land management attribute table, keep LandManage, rename columns
p<-public[,"LandManage"]
p <- st_transform(p,st_crs(t))
#buffer
p<-st_buffer(p, 0.0)
#set precision
p<-st_set_precision(p, 100000)
#make valid
p<-st_make_valid(p)
#save as shp
st_write(p,dsn = gis_dir, "p.shp", driver = "ESRI Shapefile", update=TRUE)


##TRIBAL AREAS##
if(tribal==TRUE){
  #Filter tribal attribute table, keep NAME, rename columns
  tr<-tribe[,"NAME"]
  names(tr)<-c("Tribe", "geometry")
  tr <- st_transform(tr,st_crs(t))
  #buffer
  tr<-st_buffer(tr, 0.0)
  #set precision
  tr<-st_set_precision(tr, 100000)
  #make valid
  tr<-st_make_valid(tr)
  #save
  st_write(tr,dsn = gis_dir, "tr.shp", driver = "ESRI Shapefile", update=TRUE)
}


# Join Data ----------------------------------------------------------------

##ZONING##
#Join zoning data to tax lot feature
if(zcodes==TRUE){
  tz<-st_join(t,z,largest=TRUE)
  #save joined data
  st_write(tz,dsn = gis_dir, "tz.shp", driver = "ESRI Shapefile", update=TRUE)
}

##CITY LIMITS##
#Join city limit  data to tax lots/zoning feature
tzc<-st_join(tz, c, largest=TRUE)
#save joined data
st_write(tzc,dsn = gis_dir, "tzc.shp", driver = "ESRI Shapefile", update=TRUE)

##PUBLIC LAND MANAGEMENT##
#Join public land management data to tax lots/zoning/city limits feature
tzcp<-st_join(tzc, p, largest=TRUE)
#save joined data
st_write(tzcp,dsn=gis_dir, "tzcp.shp", driver = "ESRI Shapefile", update=TRUE)

##TRIBAL AREAS##
if(tribal==TRUE){
  #join tribal areas data to tax lots/zoning/city limits/public 
  tzcptr<-st_join(tzcp, tr, largest=TRUE)
  #save jonied data
  st_write(tzcptr,dsn=gis_dir, "tzcptr.shp", driver = "ESRI Shapefile", update=TRUE )
}

if(tribal==FALSE){
  tzcptr <- tzcp
  tzcptr$Tribe <- as.character(NA) 
  #save jonied data
  st_write(tzcptr,dsn=gis_dir, "tzcptr.shp", driver = "ESRI Shapefile", update=TRUE )
}


# STOP! Return to ArcMap to Union roads_clip and rail_clip to tzcptr feature! Name the final output tzcptr_rr --------------


# Assign DMAs-------------------------------------------------------------

#read in tzcptr feature with roads and rail unioned
tzcptr_rr <- st_read(gis_dir,"tzcptr_rr", stringsAsFactors = FALSE)

#Rename columns, and add DMA and DMA2 columns
DMAs<-tzcptr_rr[,c("Taxlot","MapTaxlot","ROADOWNER","RR_NAME", "RailInt", "CityName","OwnerName","LandManage","orZCode","orZDesc","PrpClsDsc",  "PrpClass", "NLCD","NLCD_Type", "Tribe")]
DMAs <- plyr::rename(DMAs, c("ROADOWNER"="RoadOwner", "RR_NAME"="RailOwner"))
DMAs$DMA<-as.character(NA)
DMAs$DMA2<-as.character(NA)

#Add PrpClsDsc and PrpClass columns if they do not already exist
if(!("PrpClass"  %in% colnames(DMAs))){
DMAs$PrpClass <- as.character("")
}
if(!("PrpClsDsc" %in% colnames(DMAs))){
          DMAs$PrpClsDsc <- as.character("")
}

#use LU_zoning and LU_nlcd to classify zone codes and land cover
DMAs$orZClass <- as.character("")
DMAs$NLCD_Class <- as.character("")
DMAs$orZClass <- ifelse(DMAs$orZCode %in% LU_zoning$orZCode, 
                                LU_zoning[match(DMAs$orZCode, LU_zoning$orZCode), 
                                          "orZClass"], 
                                "")
DMAs$NLCD_Class <- ifelse(DMAs$NLCD %in% LU_nlcd$NLCD.Code, 
                                  LU_nlcd[match(DMAs$NLCD, 
                                                LU_nlcd$NLCD.Code), 
                                          "NLCD_Class"], 
                                  "")

#export as shp to save progress
st_write(DMAs,dsn=gis_dir, "DMAs_R1.shp", driver = "ESRI Shapefile", update=TRUE )

#copy original DMAs dataframe for backup
df.all <- DMAs

##A: Roads##
if(nrow(DMAs) > 0 & gaps == FALSE){
  
  DMAs$DMA <- ifelse((grepl("ROAD", DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) & 
                                is.na(DMAs$Tribe) &
                                DMAs$RoadOwner %in% LU_roads$ROADOWNER) | 
                               (grepl("NON", DMAs$Taxlot,ignore.case = TRUE, fixed = FALSE) & 
                                  DMAs$RoadOwner %in% LU_roads$ROADOWNER)|
                              (grepl("STR", DMAs$Taxlot,ignore.case = TRUE, fixed = FALSE) & 
                                  DMAs$RoadOwner %in% LU_roads$ROADOWNER), 
                             LU_roads[match(DMAs$RoadOwner, LU_roads$ROADOWNER), "DMA"], 
                             DMAs$DMA)
  
  DMAs$DMA <- ifelse(is.na(DMAs$Taxlot) &
                               is.na(DMAs$Tribe) & 
                               DMAs$RoadOwner %in% LU_roads$ROADOWNER, 
                             LU_roads[match(DMAs$RoadOwner, LU_roads$ROADOWNER), "DMA"], 
                             DMAs$DMA)
  
  Roads <- DMAs %>% dplyr::filter(!is.na(DMA))
  
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  
}

if(nrow(DMAs) > 0 & gaps == TRUE){
  
  DMAs$DMA <- ifelse(is.na(DMAs$Taxlot) & 
                               is.na(DMAs$Tribe) & 
                               DMAs$RoadOwner %in% LU_roads$ROADOWNER, 
                             LU_roads[match(DMAs$RoadOwner, LU_roads$ROADOWNER), "DMA"], 
                             DMAs$DMA)
  
  
  Roads <- DMAs %>% dplyr::filter(!is.na(DMA))
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  
}

##B: Railroads##
if(nrow(DMAs) > 0 & gaps == FALSE){
  
  DMAs$DMA <- ifelse((grepl("RAIL", DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                DMAs$RailOwner %in% LU_rail$RR_NAME) | 
                          (grepl("NON", DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                  DMAs$RailOwner %in% LU_rail$RR_NAME)|
                          (grepl("RR", DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                   DMAs$RailOwner %in% LU_rail$RR_NAME),
                             LU_rail[match(DMAs$RailOwner, LU_rail$RR_NAME), "DMA"], 
                             DMAs$DMA)
  
  DMAs$DMA <- ifelse(is.na(DMAs$Taxlot) &
                               is.na(DMAs$Tribe) & 
                               DMAs$RailOwner %in% LU_rail$RR_NAME, 
                             LU_rail[match(DMAs$RailOwner, LU_rail$RR_NAME), "DMA"], 
                             DMAs$DMA)
  
  DMAs$DMA2 <- ifelse(is.na(DMAs$DMA2) &
                                is.na(DMAs$Tribe) &
                                DMAs$RailInt %in% LU_rail$RR_NAME, 
                              LU_rail[match(DMAs$RailInt, LU_rail$RR_NAME), "DMA"], 
                              DMAs$DMA2)
  
  Rails <- DMAs %>% dplyr::filter(!is.na(DMA))
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  
}

if(nrow(DMAs) > 0 & gaps == TRUE){
  
  DMAs$DMA <- ifelse(is.na(DMAs$Taxlot) &
                               is.na(DMAs$Tribe) &
                               DMAs$RailOwner %in% LU_rail$RR_NAME, 
                             LU_rail[match(DMAs$RailOwner, LU_rail$RR_NAME), "DMA"], 
                             DMAs$DMA)
  
  DMAs$DMA2 <- ifelse(is.na(DMAs$DMA2) &
                                is.na(DMAs$Tribe) & 
                                DMAs$RailInt %in% LU_rail$RR_NAME, 
                              LU_rail[match(DMAs$RailInt, LU_rail$RR_NAME), "DMA"], 
                              DMAs$DMA2)
  
  Rails <- DMAs %>% dplyr::filter(!is.na(DMA))
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  
}

##C: Ports
if(nrow(DMAs) > 0){
  DMAs$DMA <- ifelse((grepl("PORT OF", DMAs$OwnerName, ignore.case = TRUE, fixed = FALSE) &
                                  DMAs$OwnerName %in% LU_owner$Owner_Name),
                               LU_owner[match(DMAs$OwnerName, LU_owner$Owner_Name), "DMA"], 
                               DMAs$DMA)
  
  Ports <- DMAs %>% dplyr::filter(!is.na(DMA))
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
}

##D: Tribal Areas##
if(nrow(DMAs) > 0 & tribal == TRUE){
  DMAs$DMA <- ifelse(!is.na(DMAs$Tribe), DMAs$Tribe, DMAs$DMA)
  Tribal <- DMAs %>% dplyr::filter(!is.na(DMA))
  Tribal$DMA <- paste0("TRIBE ", Tribal$DMA)
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
}

##E: City Limits##
if(nrow(DMAs) > 0){
  DMAs$DMA <- ifelse(!is.na(DMAs$CityName), 
                             DMAs$CityName, 
                             DMAs$DMA)
  Cities <- DMAs %>% dplyr::filter(!is.na(DMA))
  Cities$DMA <- paste0("City of ", Cities$DMA)
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  
}

##F: Public Land Management##

if(nrow(DMAs) > 0){  
  if (pubyear==2015){
  DMAs$DMA <- ifelse( DMAs$LandManage != "PV" &
                                 DMAs$LandManage != "PVI" &
                                 DMAs$LandManage != "LG"&
                                 DMAs$LandManage != "BIA"& 
                                 DMAs$LandManage != "TRIBAL"&
                                 DMAs$LandManage != "GSA", 
                             DMAs$LandManage, 
                             DMAs$DMA)
  }
  
  if (pubyear==2019){
    DMAs$DMA <- ifelse( DMAs$LandManage != "PV" &
                                   DMAs$LandManage != "PVI" &
                                   DMAs$LandManage != "PVN" &
                                   DMAs$LandManage != "PVU" &
                                   DMAs$LandManage != "LG"&
                                   DMAs$LandManage != "BIA"& 
                                   DMAs$LandManage != "UND"&
                                   DMAs$LandManage != "GSA", 
                                 DMAs$LandManage, 
                                 DMAs$DMA)
  }
   
  Public <- DMAs %>% dplyr::filter(!is.na(DMA))
  
  
  Public$DMA <- ifelse(Public$DMA=="ODF", 
                               Public$DMA <- gsub("ODF$", "ODF-Public", Public$DMA), 
                               Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="NPS", 
                               Public$DMA <- gsub("NPS", "USNPS", Public$DMA), 
                               Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="FWS", 
                               Public$DMA <- gsub("FWS", "USFWS", Public$DMA),
                               Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="DOD", 
                               Public$DMA <- gsub("DOD", "USDOD", Public$DMA), 
                               Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="DOE", 
                               Public$DMA <- gsub("DOE", "USDOE", Public$DMA), 
                               Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="BR", 
                                Public$DMA <- gsub("BR", "USBR", Public$DMA), 
                                Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="COE", 
                                Public$DMA <- gsub("COE", "USACE", Public$DMA), 
                                Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="STF", 
                                Public$DMA <- gsub("STF", "ODF-Public", Public$DMA), 
                                Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="STL", 
                                Public$DMA <- gsub("STL", "ODSL", Public$DMA), 
                                Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="STP", 
                                Public$DMA <- gsub("STP", "OPRD", Public$DMA), 
                                Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="STW", 
                                Public$DMA <- gsub("STW", "ODFW", Public$DMA), 
                                Public$DMA)
  
  Public$DMA <- ifelse((Public$DMA=="ST" & 
                                  tolower(Public$OwnerName) %in% tolower(LU_owner$Owner_Name)), 
                               LU_owner[match(tolower(Public$OwnerName), tolower(LU_owner$Owner_Name)), "DMA"], 
                               Public$DMA)
  
  Public$DMA <- ifelse(Public$DMA=="ST", 
                                Public$DMA <- gsub("ST", "OR", Public$DMA), 
                                Public$DMA)
  
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
}

##G: Tax Lot Ownership##
if(nrow(DMAs) > 0){
  DMAs$DMA <- ifelse(tolower(DMAs$OwnerName) %in% tolower(LU_owner$Owner_Name),
                             LU_owner[match(tolower(DMAs$OwnerName), tolower(LU_owner$Owner_Name)), "DMA"], 
                             DMAs$DMA)
  TaxlotOwners <- DMAs %>% dplyr::filter(!is.na(DMA))
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
}



##H: Zoning
if(zcodes==TRUE){
  if(nrow(DMAs) > 0){
    DMAs$DMA <- ifelse(DMAs$orZClass == "Mining" &
                                   !is.na(DMAs$OwnerName), 
                                 "DOGAMI", 
                                 DMAs$DMA)
    
    DMAs$DMA <- ifelse(DMAs$orZClass == "Forestry" &
                                   !is.na(DMAs$OwnerName), 
                                 "ODF-Private", 
                                 DMAs$DMA)
    
    DMAs$DMA <- ifelse(DMAs$orZClass == "Agriculture" &
                                   !is.na(DMAs$OwnerName),
                                 "ODA", 
                                 DMAs$DMA)
    
    Zoning <- DMAs %>% dplyr::filter(!is.na(DMA))
    DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  }
}

if(zcodes==FALSE){
  if(nrow(DMAs) > 0){
    DMAs$DMA <- ifelse(DMAs$PrpClass == "Mining" &
                                   !is.na(DMAs$OwnerName), 
                                 "DOGAMI", 
                                 DMAs$DMA)
    
    DMAs$DMA <- ifelse(DMAs$PrpClass == "Forestry" &
                                   !is.na(DMAs$OwnerName), 
                                 "ODF-Private", 
                                 DMAs$DMA)
    
    DMAs$DMA <- ifelse(DMAs$PrpClass == "Agriculture" &
                                   !is.na(DMAs$OwnerName),
                                 "ODA", 
                                 DMAs$DMA)
    
    Zoning <- DMAs %>% dplyr::filter(!is.na(DMA))
    DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  }
}

##I: NLCD
if(zcodes==TRUE){
  if(nrow(DMAs) > 0){
    DMAs$DMA <- ifelse(DMAs$orZClass == "Agriculture & Forestry" &
                              DMAs$NLCD_Class == "Forest" &
                              !is.na(DMAs$OwnerName),
                            "ODF-Private", 
                            DMAs$DMA)
    
    DMAs$DMA <- ifelse(DMAs$orZClass == "Agriculture & Forestry" &
                              DMAs$NLCD_Class == "Agriculture" &
                              !is.na(DMAs$OwnerName),
                            "ODA", 
                            DMAs$DMA)
    
    DMAs$DMA2 <- ifelse(DMAs$orZClass == "Agriculture & Forestry" &
                               DMAs$DMA == "ODA",
                             "ODF-Private", 
                             DMAs$DMA2)
    
    DMAs$DMA2 <- ifelse(DMAs$orZClass == "Agriculture & Forestry" &
                               DMAs$DMA == "ODF-Private",
                             "ODA",
                             DMAs$DMA2)
    
    NLCD <- DMAs %>% dplyr::filter(!is.na(DMA))
    DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  }
}
if(zcodes==FALSE){
  if(nrow(DMAs) > 0){
    DMAs$DMA <- ifelse(DMAs$PrpClass == "Agriculture & Forestry" &
                              DMAs$NLCD_Class == "Forest" &
                              !is.na(DMAs$OwnerName),
                            "ODF-Private", 
                            DMAs$DMA)
    
    DMAs$DMA <- ifelse(DMAs$PrpClass == "Agriculture & Forestry" &
                              DMAs$NLCD_Class == "Agriculture" &
                              !is.na(DMAs$OwnerName),
                            "ODA", 
                            DMAs$DMA)
    
    DMAs$DMA2 <- ifelse(DMAs$PrpClass == "Agriculture & Forestry" &
                               DMAs$DMA == "ODA",
                             "ODF-Private", 
                             DMAs$DMA2)
    
    DMAs$DMA2 <- ifelse(DMAs$PrpClass == "Agriculture & Forestry" &
                               DMAs$DMA == "ODF-Private",
                             "ODA",
                             DMAs$DMA2)
    
    NLCD <- DMAs %>% dplyr::filter(!is.na(DMA))
    DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
  }
}

##J: Water##
if(nrow(DMAs) > 0){
  if (pubyear==2015){
  DMAs$DMA <- ifelse((grepl("WATER", DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                DMAs$LandManage != "PV" &
                                DMAs$LandManage != "PVI" &
                                DMAs$LandManage != "LG"&
                                DMAs$LandManage != "BIA"& 
                                DMAs$LandManage != "TRIBAL"&
                                DMAs$LandManage != "GSA"), 
                             DMAs$LandManage, 
                             DMAs$DMA)
  }
  
  if (pubyear==2019){
    DMAs$DMA <- ifelse((grepl("WATER", DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                   DMAs$LandManage != "PV" &
                                   DMAs$LandManage != "PVI" &
                                   DMAs$LandManage != "PVN" &
                                   DMAs$LandManage != "PVU" &
                                   DMAs$LandManage != "LG"&
                                   DMAs$LandManage != "BIA"& 
                                   DMAs$LandManage != "UND"&
                                   DMAs$LandManage != "GSA"), 
                                DMAs$LandManage, 
                                DMAs$DMA)
  }
  
  DMAs$DMA <- ifelse(is.na(DMAs$DMA) &
                               grepl("WATER", DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE), 
                             "WATER", 
                             DMAs$DMA)
  DMAs$DMA <- ifelse(is.na(DMAs$DMA) &
                            grepl("RIV", DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE), 
                          "WATER", 
                          DMAs$DMA)
  
  Water <- DMAs %>% dplyr::filter(!is.na(DMA))
  
  Water$DMA <- ifelse(Water$DMA=="ODF", 
                              Water$DMA <- gsub("ODF$", "ODF-Public", Water$DMA), 
                              Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="NPS", 
                              Water$DMA <- gsub("NPS", "USNPS", Water$DMA), 
                              Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="FWS", 
                              Water$DMA <- gsub("FWS", "USFWS", Water$DMA),
                              Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="DOD", 
                              Water$DMA <- gsub("DOD", "USDOD", Water$DMA), 
                              Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="DOE", 
                              Water$DMA <- gsub("DOE", "USDOE", Water$DMA), 
                              Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="BR", 
                                Water$DMA <- gsub("BR", "USBR", Water$DMA), 
                                Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="COE", 
                                Water$DMA <- gsub("COE", "USACE", Water$DMA), 
                                Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="ST", 
                                Water$DMA <- gsub("ST", "OR", Water$DMA), 
                                Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="STF", 
                                Water$DMA <- gsub("STF", "ODF-Public", Water$DMA), 
                                Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="STL", 
                                Water$DMA <- gsub("STL", "ODSL", Water$DMA), 
                                Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="STP", 
                                Water$DMA <- gsub("STP", "OPRD", Water$DMA), 
                                Water$DMA)
  
  Water$DMA <- ifelse(Water$DMA=="STW", 
                                Water$DMA <- gsub("STW", "ODFW", Water$DMA), 
                                Water$DMA)
  
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
}

##K: County##
#If a DMA has not yet been assigned, the county is the DMA
if(nrow(DMAs) > 0){
  DMAs$DMA <- countyname
  County <- DMAs %>% dplyr::filter(!is.na(DMA))
  DMAs <- DMAs %>% dplyr::filter(is.na(DMA))
}

# Review ------------------------------------------------------------------

#Search County for tax lots owned by DMAs. Add unique DMA names to Taxlot ownerhip csv, reload LU_owner, and rerun assign DMAs section.
#search NLCD for improperly assigned DMAs, look for timber company owners of DMA lots and ag company owners of ODF lots.

#recombine all subsets of the original tax lot data and write to shapefile
if(nrow(DMAs) == 0 & tribal== TRUE){
  DMAs <- rbind(Cities, County, NLCD, Public, Ports, Rails, Roads, TaxlotOwners, Tribal, Water, Zoning)
}

if(nrow(DMAs) == 0 & tribal== FALSE){
  DMAs <- rbind(Cities, County, NLCD, Public, Ports, Rails, Roads, TaxlotOwners, Water, Zoning)
}

#add official DMA name
DMAs$DMA_RP <- as.character("")
DMAs$DMA_RP <- ifelse(DMAs$DMA %in% LU_DMAs$DMA, LU_DMAs[match(DMAs$DMA, LU_DMAs$DMA), "DMA_FullName"], DMAs$DMA_RP)

#add official DMA name for DMA2
DMAs$DMA_RP2 <- as.character("")
DMAs$DMA_RP2 <- ifelse(DMAs$DMA2 %in% LU_DMAs$DMA, LU_DMAs[match(DMAs$DMA2, LU_DMAs$DMA), "DMA_FullName"], DMAs$DMA_RP2)

#Add DMA classification
DMAs$DMA_RP_Cl <- as.character("")
DMAs$DMA_RP_Cl <- ifelse(DMAs$DMA %in% LU_DMAs$DMA, LU_DMAs[match(DMAs$DMA, LU_DMAs$DMA), "DMA_Class"], DMAs$DMA_RP_Cl)

#Add DMA2 classification
DMAs$DMA_RP2_Cl <- as.character("")
DMAs$DMA_RP2_Cl <- ifelse(DMAs$DMA2 %in% LU_DMAs$DMA, LU_DMAs[match(DMAs$DMA2, LU_DMAs$DMA), "DMA_Class"], DMAs$DMA_RP2_Cl)

#Add symbology
DMAs$Symbol <- as.character("")
DMAs$Symbol <-ifelse(DMAs$DMA %in% LU_DMAs$DMA, LU_DMAs[match(DMAs$DMA, LU_DMAs$DMA), "Symbol"], DMAs$Symbol)

#Add flag field for manual edits needing
DMAs$edit <- as.character("")

#Rename DMA field
DMAs <- plyr::rename(DMAs, c("DMA"="DMA_RP_Ab","DMA2"="DMA_RP2_Ab"))

#reorder columns
DMAs <- DMAs[c("Taxlot","MapTaxlot","RoadOwner","RailOwner","RailInt","Tribe", "CityName","LandManage","OwnerName","orZCode","orZDesc","orZClass", "PrpClsDsc","PrpClass","NLCD","NLCD_Type", "NLCD_Class", "DMA_RP_Ab","DMA_RP","DMA_RP_Cl","DMA_RP2_Ab","DMA_RP2","DMA_RP2_Cl", "edit", "Symbol")]
#write to shapefile
st_write(DMAs,dsn=gis_dir, "DMAs.shp", driver = "ESRI Shapefile",
         update=TRUE )

# Return to ArcMap for final edits ----------------------------------
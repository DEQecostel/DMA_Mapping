##Master DMA Mapping Script

# Load Packages ------------------------------------------------------------
library(sf)
library(lwgeom)
library(plyr)
library(dplyr)
library(stringr)

# Input Data ------------------------------------------------------------

#County GIS directory name
gis_dir <- "//deqhq1/TMDL/DMA_Mapping/Baker/GIS"
#County GIS file names
taxlot_name <- "Baker_Taxlot01withTable"
zoning_name <- "zoning_union"
public_name <-  "public_union"
citylim_name <-  "citylim_union"
#tribe_name <-
county_name <- "BakerCounty"

#Main GIS directory name
gis_main <- "//deqhq1/TMDL/DMA_Mapping/Main/GIS"
#Main GIS file names
road_name <- "roads_2017_300ftBuffer"
rail_buff_name <- "railroads_2017_200ftBuffer"
rail_line_name <- "railroads"

#Look Up Table directory name
LU_dir <-  "//deqhq1/TMDL/DMA_Mapping/Main/Lookups"
#Look Up Table names
LU_owner_name <- "TaxlotOwners.csv"
LU_zoning_name <- "ZoningClassification.csv"
LU_nlcd_name <- "LandcoverClassification.csv"
LU_rail_name <- "railroads.csv"
LU_roads_name <- "roads.csv"
LU_DMAs_name <- "DMAs.csv"

#County specific details
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
  t<-taxlots[,c("Taxlot", "MapTaxlot", "OWNERLINE1", "NLCD", "NLCD_Type")]
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
  t <- t[,c("Taxlot", "MapTaxlot", "OWNERLINE1", "NLCD", "NLCD_Type", "RR_NAME")]
  #rename columns
  t <- plyr::rename(t, c("OWNERLINE1"="OwnerName", "RR_NAME"="RailInt"))
  #export as shp to save progress
  st_write(t,dsn = gis_dir, "t.shp", driver = "ESRI Shapefile", update=TRUE)
}

if(zcodes==FALSE){
  #Subset tax lots attribute table to include unique tax lot identifier (APN), tax lot type, owner name and NLCD information.
  t<-taxlots[,c("Taxlot", "MapTaxlot", "OwnerLine1","PrpClsDsc", "PrpClass", "NLCD", "NLCD_Type")]
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


# STOP! Return to ArcMap to Union roads_buff_clip and rail_buff_clip to tzcptr feature! --------------


# Assign DMAs-------------------------------------------------------------

#read in tzcptr feature with roads and rail unioned
tzcptr_rr <- st_read(gis_dir,"tzcptr2_rr", stringsAsFactors = FALSE)

#Rename columns, and add DMA and DMA2 columns
Yamhill_DMAs<-tzcptr_rr[,c("Taxlot","MapTaxlot","ROADOWNER","RR_NAME", "RailInt", "CityName","OwnerName","LandManage","orZCode","orZDesc","NLCD","NLCD_Type", "Tribe")]
Yamhill_DMAs <- plyr::rename(Yamhill_DMAs, c("ROADOWNER"="RoadOwner", "RR_NAME"="RailOwner"))
Yamhill_DMAs$DMA<-as.character(NA)
Yamhill_DMAs$DMA2<-as.character(NA)

#Add PrpClsDsc and PrpClass columns if they do not already exist
if(zcodes==TRUE){
Yamhill_DMAs$PrpClsDsc <- as.character("")
Yamhill_DMAs$PrpClass <- as.character("")
}

#use LU_zoning and LU_nlcd to classify zone codes and land cover
Yamhill_DMAs$orZClass <- as.character("")
Yamhill_DMAs$NLCD_Class <- as.character("")
Yamhill_DMAs$orZClass <- ifelse(Yamhill_DMAs$orZCode %in% LU_zoning$orZCode, 
                                LU_zoning[match(Yamhill_DMAs$orZCode, LU_zoning$orZCode), 
                                          "orZClass"], 
                                "")
Yamhill_DMAs$NLCD_Class <- ifelse(Yamhill_DMAs$NLCD %in% LU_nlcd$NLCD.Code, 
                                  LU_nlcd[match(Yamhill_DMAs$NLCD, 
                                                LU_nlcd$NLCD.Code), 
                                          "NLCD_Class"], 
                                  "")

#export as shp to save progress
st_write(Yamhill_DMAs,dsn=gis_dir, "Yamhill_DMAs_R1.shp", driver = "ESRI Shapefile", update=TRUE )
#Yamhill_DMAs <- st_read(gis_dir,"Yamhill_DMAs_R1", stringsAsFactors = FALSE)

#copy original Yamhill_DMAs dataframe for backup
df.all <- Yamhill_DMAs
#Yamhill_DMAs <- df.all

##A: Roads##
if(nrow(Yamhill_DMAs) > 0 & gaps == FALSE){
  
  Yamhill_DMAs$DMA <- ifelse((grepl("ROAD", Yamhill_DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) & 
                                is.na(Yamhill_DMAs$Tribe) &
                                Yamhill_DMAs$RoadOwner %in% LU_roads$ROADOWNER) | 
                               (grepl("NON", Yamhill_DMAs$Taxlot,ignore.case = TRUE, fixed = FALSE) & 
                                  Yamhill_DMAs$RoadOwner %in% LU_roads$ROADOWNER)|
                              (grepl("STR", Yamhill_DMAs$Taxlot,ignore.case = TRUE, fixed = FALSE) & 
                                  Yamhill_DMAs$RoadOwner %in% LU_roads$ROADOWNER), 
                             LU_roads[match(Yamhill_DMAs$RoadOwner, LU_roads$ROADOWNER), "DMA"], 
                             Yamhill_DMAs$DMA)
  
  Yamhill_DMAs$DMA <- ifelse(is.na(Yamhill_DMAs$Taxlot) &
                               is.na(Yamhill_DMAs$Tribe) & 
                               Yamhill_DMAs$RoadOwner %in% LU_roads$ROADOWNER, 
                             LU_roads[match(Yamhill_DMAs$RoadOwner, LU_roads$ROADOWNER), "DMA"], 
                             Yamhill_DMAs$DMA)
  
  Yamhill_Roads <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  
}

if(nrow(Yamhill_DMAs) > 0 & gaps == TRUE){
  
  Yamhill_DMAs$DMA <- ifelse(is.na(Yamhill_DMAs$Taxlot) & 
                               is.na(Yamhill_DMAs$Tribe) & 
                               Yamhill_DMAs$RoadOwner %in% LU_roads$ROADOWNER, 
                             LU_roads[match(Yamhill_DMAs$RoadOwner, LU_roads$ROADOWNER), "DMA"], 
                             Yamhill_DMAs$DMA)
  
  
  Yamhill_Roads <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  
}

##B: Railroads##
if(nrow(Yamhill_DMAs) > 0 & gaps == FALSE){
  
  Yamhill_DMAs$DMA <- ifelse((grepl("RAIL", Yamhill_DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                Yamhill_DMAs$RailOwner %in% LU_rail$RR_NAME) | 
                          (grepl("NON", Yamhill_DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                  Yamhill_DMAs$RailOwner %in% LU_rail$RR_NAME)|
                          (grepl("RR", Yamhill_DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                   Yamhill_DMAs$RailOwner %in% LU_rail$RR_NAME),
                             LU_rail[match(Yamhill_DMAs$RailOwner, LU_rail$RR_NAME), "DMA"], 
                             Yamhill_DMAs$DMA)
  
  Yamhill_DMAs$DMA <- ifelse(is.na(Yamhill_DMAs$Taxlot) &
                               is.na(Yamhill_DMAs$Tribe) & 
                               Yamhill_DMAs$RailOwner %in% LU_rail$RR_NAME, 
                             LU_rail[match(Yamhill_DMAs$RailOwner, LU_rail$RR_NAME), "DMA"], 
                             Yamhill_DMAs$DMA)
  
  Yamhill_DMAs$DMA2 <- ifelse(is.na(Yamhill_DMAs$DMA2) &
                                is.na(Yamhill_DMAs$Tribe) &
                                Yamhill_DMAs$RailInt %in% LU_rail$RR_NAME, 
                              LU_rail[match(Yamhill_DMAs$RailInt, LU_rail$RR_NAME), "DMA"], 
                              Yamhill_DMAs$DMA2)
  
  Yamhill_Rails <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  
}

if(nrow(Yamhill_DMAs) > 0 & gaps == TRUE){
  
  Yamhill_DMAs$DMA <- ifelse(is.na(Yamhill_DMAs$Taxlot) &
                               is.na(Yamhill_DMAs$Tribe) &
                               Yamhill_DMAs$RailOwner %in% LU_rail$RR_NAME, 
                             LU_rail[match(Yamhill_DMAs$RailOwner, LU_rail$RR_NAME), "DMA"], 
                             Yamhill_DMAs$DMA)
  
  Yamhill_DMAs$DMA2 <- ifelse(is.na(Yamhill_DMAs$DMA2) &
                                is.na(Yamhill_DMAs$Tribe) & 
                                Yamhill_DMAs$RailInt %in% LU_rail$RR_NAME, 
                              LU_rail[match(Yamhill_DMAs$RailInt, LU_rail$RR_NAME), "DMA"], 
                              Yamhill_DMAs$DMA2)
  
  Yamhill_Rails <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  
}

##C: Ports
if(nrow(Yamhill_DMAs) > 0){
  Yamhill_DMAs$DMA <- ifelse((grepl("PORT OF", Yamhill_DMAs$OwnerName, ignore.case = TRUE, fixed = FALSE) &
                                  Yamhill_DMAs$OwnerName %in% LU_owner$Owner_Name),
                               LU_owner[match(Yamhill_DMAs$OwnerName, LU_owner$Owner_Name), "DMA"], 
                               Yamhill_DMAs$DMA)
  
  Yamhill_Ports <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
}

##D: Tribal Areas##
if(nrow(Yamhill_DMAs) > 0 & tribal == TRUE){
  Yamhill_DMAs$DMA <- ifelse(!is.na(Yamhill_DMAs$Tribe), Yamhill_DMAs$Tribe, Yamhill_DMAs$DMA)
  Yamhill_Tribal <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  Yamhill_Tribal$DMA <- paste0("TRIBE ", Yamhill_Tribal$DMA)
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
}

##E: City Limits##
if(nrow(Yamhill_DMAs) > 0){
  Yamhill_DMAs$DMA <- ifelse(!is.na(Yamhill_DMAs$CityName), 
                             Yamhill_DMAs$CityName, 
                             Yamhill_DMAs$DMA)
  Yamhill_Cities <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  Yamhill_Cities$DMA <- paste0("City of ", Yamhill_Cities$DMA)
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  
}

##F: Public Land Management##

if(nrow(Yamhill_DMAs) > 0){  
  if (pubyear==2015){
  Yamhill_DMAs$DMA <- ifelse( Yamhill_DMAs$LandManage != "PV" &
                                 Yamhill_DMAs$LandManage != "PVI" &
                                 Yamhill_DMAs$LandManage != "LG"&
                                 Yamhill_DMAs$LandManage != "BIA"& 
                                 Yamhill_DMAs$LandManage != "TRIBAL"&
                                 Yamhill_DMAs$LandManage != "GSA", 
                             Yamhill_DMAs$LandManage, 
                             Yamhill_DMAs$DMA)
  }
  
  if (pubyear==2019){
    Yamhill_DMAs$DMA <- ifelse( Yamhill_DMAs$LandManage != "PV" &
                                   Yamhill_DMAs$LandManage != "PVI" &
                                   Yamhill_DMAs$LandManage != "PVN" &
                                   Yamhill_DMAs$LandManage != "PVU" &
                                   Yamhill_DMAs$LandManage != "LG"&
                                   Yamhill_DMAs$LandManage != "BIA"& 
                                   Yamhill_DMAs$LandManage != "UND"&
                                   Yamhill_DMAs$LandManage != "GSA", 
                                 Yamhill_DMAs$LandManage, 
                                 Yamhill_DMAs$DMA)
  }
   
  Yamhill_Public <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="ODF", 
                               Yamhill_Public$DMA <- gsub("ODF$", "ODF-Public", Yamhill_Public$DMA), 
                               Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="NPS", 
                               Yamhill_Public$DMA <- gsub("NPS", "USNPS", Yamhill_Public$DMA), 
                               Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="FWS", 
                               Yamhill_Public$DMA <- gsub("FWS", "USFWS", Yamhill_Public$DMA),
                               Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="DOD", 
                               Yamhill_Public$DMA <- gsub("DOD", "USDOD", Yamhill_Public$DMA), 
                               Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="DOE", 
                               Yamhill_Public$DMA <- gsub("DOE", "USDOE", Yamhill_Public$DMA), 
                               Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="BR", 
                                Yamhill_Public$DMA <- gsub("BR", "USBR", Yamhill_Public$DMA), 
                                Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="COE", 
                                Yamhill_Public$DMA <- gsub("COE", "USACE", Yamhill_Public$DMA), 
                                Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="STF", 
                                Yamhill_Public$DMA <- gsub("STF", "ODF-Public", Yamhill_Public$DMA), 
                                Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="STL", 
                                Yamhill_Public$DMA <- gsub("STL", "ODSL", Yamhill_Public$DMA), 
                                Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="STP", 
                                Yamhill_Public$DMA <- gsub("STP", "OPRD", Yamhill_Public$DMA), 
                                Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="STW", 
                                Yamhill_Public$DMA <- gsub("STW", "ODFW", Yamhill_Public$DMA), 
                                Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse((Yamhill_Public$DMA=="ST" & 
                                  tolower(Yamhill_Public$OwnerName) %in% tolower(LU_owner$Owner_Name)), 
                               LU_owner[match(tolower(Yamhill_Public$OwnerName), tolower(LU_owner$Owner_Name)), "DMA"], 
                               Yamhill_Public$DMA)
  
  Yamhill_Public$DMA <- ifelse(Yamhill_Public$DMA=="ST", 
                                Yamhill_Public$DMA <- gsub("ST", "OR", Yamhill_Public$DMA), 
                                Yamhill_Public$DMA)
  
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
}

##G: Tax Lot Ownership##
if(nrow(Yamhill_DMAs) > 0){
  Yamhill_DMAs$DMA <- ifelse(tolower(Yamhill_DMAs$OwnerName) %in% tolower(LU_owner$Owner_Name),
                             LU_owner[match(tolower(Yamhill_DMAs$OwnerName), tolower(LU_owner$Owner_Name)), "DMA"], 
                             Yamhill_DMAs$DMA)
  Yamhill_TaxlotOwners <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
}



##H: Zoning
if(zcodes==TRUE){
  if(nrow(Yamhill_DMAs) > 0){
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$orZClass == "Mining" &
                                   !is.na(Yamhill_DMAs$OwnerName), 
                                 "DOGAMI", 
                                 Yamhill_DMAs$DMA)
    
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$orZClass == "Forestry" &
                                   !is.na(Yamhill_DMAs$OwnerName), 
                                 "ODF-Private", 
                                 Yamhill_DMAs$DMA)
    
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$orZClass == "Agriculture" &
                                   !is.na(Yamhill_DMAs$OwnerName),
                                 "ODA", 
                                 Yamhill_DMAs$DMA)
    
    Yamhill_Zoning <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
    Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  }
}

if(zcodes==FALSE){
  if(nrow(Yamhill_DMAs) > 0){
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$PrpClass == "Mining" &
                                   !is.na(Yamhill_DMAs$OwnerName), 
                                 "DOGAMI", 
                                 Yamhill_DMAs$DMA)
    
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$PrpClass == "Forestry" &
                                   !is.na(Yamhill_DMAs$OwnerName), 
                                 "ODF-Private", 
                                 Yamhill_DMAs$DMA)
    
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$PrpClass == "Agriculture" &
                                   !is.na(Yamhill_DMAs$OwnerName),
                                 "ODA", 
                                 Yamhill_DMAs$DMA)
    
    Yamhill_Zoning <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
    Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  }
}

##I: NLCD
if(zcodes==TRUE){
  if(nrow(Yamhill_DMAs) > 0){
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$orZClass == "Agriculture & Forestry" &
                              Yamhill_DMAs$NLCD_Class == "Forest" &
                              !is.na(Yamhill_DMAs$OwnerName),
                            "ODF-Private", 
                            Yamhill_DMAs$DMA)
    
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$orZClass == "Agriculture & Forestry" &
                              Yamhill_DMAs$NLCD_Class == "Agriculture" &
                              !is.na(Yamhill_DMAs$OwnerName),
                            "ODA", 
                            Yamhill_DMAs$DMA)
    
    Yamhill_DMAs$DMA2 <- ifelse(Yamhill_DMAs$orZClass == "Agriculture & Forestry" &
                               Yamhill_DMAs$DMA == "ODA",
                             "ODF-Private", 
                             Yamhill_DMAs$DMA2)
    
    Yamhill_DMAs$DMA2 <- ifelse(Yamhill_DMAs$orZClass == "Agriculture & Forestry" &
                               Yamhill_DMAs$DMA == "ODF-Private",
                             "ODA",
                             Yamhill_DMAs$DMA2)
    
    Yamhill_NLCD <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
    Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  }
}
if(zcodes==FALSE){
  if(nrow(Yamhill_DMAs) > 0){
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$PrpClass == "Agriculture & Forestry" &
                              Yamhill_DMAs$NLCD_Class == "Forest" &
                              !is.na(Yamhill_DMAs$OwnerName),
                            "ODF-Private", 
                            Yamhill_DMAs$DMA)
    
    Yamhill_DMAs$DMA <- ifelse(Yamhill_DMAs$PrpClass == "Agriculture & Forestry" &
                              Yamhill_DMAs$NLCD_Class == "Agriculture" &
                              !is.na(Yamhill_DMAs$OwnerName),
                            "ODA", 
                            Yamhill_DMAs$DMA)
    
    Yamhill_DMAs$DMA2 <- ifelse(Yamhill_DMAs$PrpClass == "Agriculture & Forestry" &
                               Yamhill_DMAs$DMA == "ODA",
                             "ODF-Private", 
                             Yamhill_DMAs$DMA2)
    
    Yamhill_DMAs$DMA2 <- ifelse(Yamhill_DMAs$PrpClass == "Agriculture & Forestry" &
                               Yamhill_DMAs$DMA == "ODF-Private",
                             "ODA",
                             Yamhill_DMAs$DMA2)
    
    Yamhill_NLCD <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
    Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
  }
}

##J: Water##
if(nrow(Yamhill_DMAs) > 0){
  if (pubyear==2015){
  Yamhill_DMAs$DMA <- ifelse((grepl("WATER", Yamhill_DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                Yamhill_DMAs$LandManage != "PV" &
                                Yamhill_DMAs$LandManage != "PVI" &
                                Yamhill_DMAs$LandManage != "LG"&
                                Yamhill_DMAs$LandManage != "BIA"& 
                                Yamhill_DMAs$LandManage != "TRIBAL"&
                                Yamhill_DMAs$LandManage != "GSA"), 
                             Yamhill_DMAs$LandManage, 
                             Yamhill_DMAs$DMA)
  }
  
  if (pubyear==2019){
    Yamhill_DMAs$DMA <- ifelse((grepl("WATER", Yamhill_DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                   Yamhill_DMAs$LandManage != "PV" &
                                   Yamhill_DMAs$LandManage != "PVI" &
                                   Yamhill_DMAs$LandManage != "PVN" &
                                   Yamhill_DMAs$LandManage != "PVU" &
                                   Yamhill_DMAs$LandManage != "LG"&
                                   Yamhill_DMAs$LandManage != "BIA"& 
                                   Yamhill_DMAs$LandManage != "UND"&
                                   Yamhill_DMAs$LandManage != "GSA"), 
                                Yamhill_DMAs$LandManage, 
                                Yamhill_DMAs$DMA)
  }
  
  Yamhill_DMAs$DMA <- ifelse(is.na(Yamhill_DMAs$DMA) &
                               grepl("WATER", Yamhill_DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE), 
                             "WATER", 
                             Yamhill_DMAs$DMA)
  Yamhill_DMAs$DMA <- ifelse(is.na(Yamhill_DMAs$DMA) &
                            grepl("RIV", Yamhill_DMAs$Taxlot, ignore.case = TRUE, fixed = FALSE), 
                          "WATER", 
                          Yamhill_DMAs$DMA)
  
  Yamhill_Water <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="ODF", 
                              Yamhill_Water$DMA <- gsub("ODF$", "ODF-Public", Yamhill_Water$DMA), 
                              Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="NPS", 
                              Yamhill_Water$DMA <- gsub("NPS", "USNPS", Yamhill_Water$DMA), 
                              Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="FWS", 
                              Yamhill_Water$DMA <- gsub("FWS", "USFWS", Yamhill_Water$DMA),
                              Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="DOD", 
                              Yamhill_Water$DMA <- gsub("DOD", "USDOD", Yamhill_Water$DMA), 
                              Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="DOE", 
                              Yamhill_Water$DMA <- gsub("DOE", "USDOE", Yamhill_Water$DMA), 
                              Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="BR", 
                                Yamhill_Water$DMA <- gsub("BR", "USBR", Yamhill_Water$DMA), 
                                Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="COE", 
                                Yamhill_Water$DMA <- gsub("COE", "USACE", Yamhill_Water$DMA), 
                                Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="ST", 
                                Yamhill_Water$DMA <- gsub("ST", "OR", Yamhill_Water$DMA), 
                                Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="STF", 
                                Yamhill_Water$DMA <- gsub("STF", "ODF-Public", Yamhill_Water$DMA), 
                                Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="STL", 
                                Yamhill_Water$DMA <- gsub("STL", "ODSL", Yamhill_Water$DMA), 
                                Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="STP", 
                                Yamhill_Water$DMA <- gsub("STP", "OPRD", Yamhill_Water$DMA), 
                                Yamhill_Water$DMA)
  
  Yamhill_Water$DMA <- ifelse(Yamhill_Water$DMA=="STW", 
                                Yamhill_Water$DMA <- gsub("STW", "ODFW", Yamhill_Water$DMA), 
                                Yamhill_Water$DMA)
  
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
}

##K: County##
#If a DMA has not yet been assigned, the county is the DMA
if(nrow(Yamhill_DMAs) > 0){
  Yamhill_DMAs$DMA <- countyname
  Yamhill_County <- Yamhill_DMAs %>% dplyr::filter(!is.na(DMA))
  Yamhill_DMAs <- Yamhill_DMAs %>% dplyr::filter(is.na(DMA))
}

# Review ------------------------------------------------------------------

#Search Yamhill_County for tax lots owned by DMAs. Add unique DMA names to Taxlot ownerhip csv.
#search Yamhill_NLCD for improperly assigned DMAs, look for timber company owners of DMA lots and ag company owners of ODF lots.

#recombine all subsets of the original tax lot data and write to shapefile
if(nrow(Yamhill_DMAs) == 0 & tribal== TRUE){
  Yamhill_DMAs <- rbind(Yamhill_Cities, Yamhill_County, Yamhill_NLCD, Yamhill_Public, Yamhill_Ports, Yamhill_Rails, Yamhill_Roads, Yamhill_TaxlotOwners, Yamhill_Tribal, Yamhill_Water, Yamhill_Zoning)
}

if(nrow(Yamhill_DMAs) == 0 & tribal== FALSE){
  Yamhill_DMAs <- rbind(Yamhill_Cities, Yamhill_County, Yamhill_NLCD, Yamhill_Public, Yamhill_Ports, Yamhill_Rails, Yamhill_Roads, Yamhill_TaxlotOwners, Yamhill_Water, Yamhill_Zoning)
}

#add official DMA name
Yamhill_DMAs$DMA_RP <- as.character("")
Yamhill_DMAs$DMA_RP <- ifelse(Yamhill_DMAs$DMA %in% LU_DMAs$DMA, LU_DMAs[match(Yamhill_DMAs$DMA, LU_DMAs$DMA), "DMA_FullName"], Yamhill_DMAs$DMA_RP)

#add official DMA name for DMA2
Yamhill_DMAs$DMA_RP2 <- as.character("")
Yamhill_DMAs$DMA_RP2 <- ifelse(Yamhill_DMAs$DMA2 %in% LU_DMAs$DMA, LU_DMAs[match(Yamhill_DMAs$DMA2, LU_DMAs$DMA), "DMA_FullName"], Yamhill_DMAs$DMA_RP2)

#Add DMA classification
Yamhill_DMAs$DMA_RP_Cl <- as.character("")
Yamhill_DMAs$DMA_RP_Cl <- ifelse(Yamhill_DMAs$DMA %in% LU_DMAs$DMA, LU_DMAs[match(Yamhill_DMAs$DMA, LU_DMAs$DMA), "DMA_Class"], Yamhill_DMAs$DMA_RP_Cl)

#Add DMA2 classification
Yamhill_DMAs$DMA_RP2_Cl <- as.character("")
Yamhill_DMAs$DMA_RP2_Cl <- ifelse(Yamhill_DMAs$DMA2 %in% LU_DMAs$DMA, LU_DMAs[match(Yamhill_DMAs$DMA2, LU_DMAs$DMA), "DMA_Class"], Yamhill_DMAs$DMA_RP2_Cl)

#Add symbology
Yamhill_DMAs$Symbol <- as.character("")
Yamhill_DMAs$Symbol <-ifelse(Yamhill_DMAs$DMA %in% LU_DMAs$DMA, LU_DMAs[match(Yamhill_DMAs$DMA, LU_DMAs$DMA), "Symbol"], Yamhill_DMAs$Symbol)

#Add flag field for manual edits needing
Yamhill_DMAs$edit <- as.character("")

#Rename DMA field
Yamhill_DMAs <- plyr::rename(Yamhill_DMAs, c("DMA"="DMA_RP_Ab","DMA2"="DMA_RP2_Ab"))

#reorder columns
Yamhill_DMAs <- Yamhill_DMAs[c("Taxlot","MapTaxlot","RoadOwner","RailOwner","RailInt","Tribe", "CityName","LandManage","OwnerName","orZCode","orZDesc","orZClass", "PrpClsDsc","PrpClass","NLCD","NLCD_Type", "NLCD_Class", "DMA_RP_Ab","DMA_RP","DMA_RP_Cl","DMA_RP2_Ab","DMA_RP2","DMA_RP2_Cl", "edit", "Symbol")]
#write to shapefile
st_write(Yamhill_DMAs,dsn=gis_dir, "Yamhill_DMAs.shp", driver = "ESRI Shapefile",
         update=TRUE )

# Return to ArcMap for final edits ----------------------------------
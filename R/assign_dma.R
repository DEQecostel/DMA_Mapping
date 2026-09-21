#' Assign DMA to polygons
#'
#' @param DMA This is the DMA R object output by the nlcd_majority() function. This can also be the file name of the county DMA GIS file
#' @param DMA_dir IF reading in a GIS file for param DMA, this is the file path of the county DMA GIS file
#' @param countyname name of county with DMAs being updated by this function
#' @param gaps TRUE/FAlSE indicating whether there are gaps in the county tax lot GIS file
#' @param tribal TRUE/FALSe indicating whether there are tribal areas within the county
#' @param zcode TRUE/FALSE indicating whether zoning data is included for this county in the statwide basedata layer
#' @param pubyear 2016 or 2019 indicating the year of the public land management GIS base data layer
#' @param region "eastern" or "western" indicating which NLCD Lookup table to load. This determines how nlcd land cover type shrub/scrub will be classifiied; as "Agriculture" in the eastern region and as "Forest" in the western region.
#'
#' @returns DMA
#' @export
#'
#' @examples
#' DMA_v2 <- assign_dma(DMA= DMA, DMA_dir, countyname = "Jefferson County", gaps= FALSE, tribal = TRUE, zcode = TRUE, pubyear= 2019, region = "eastern")

assign_dma <- function(DMA,
                       DMA_dir,
                       countyname,
                       gaps,
                       tribal,
                       zcode,
                       pubyear,
                       region)
  {

  # # - TESTING
  # DMA <- DMA_v1
  # countyname <- "Jefferson County"
  # gaps <- FALSE
  # tribal <- TRUE
  # zcode <- TRUE
  # pubyear <- 2019
  # region <- "eastern"
  # # --

  #load packages
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)

  #Read in DMA polygons as R object or as shapefile
    if (inherits(DMA, "sf")) {
      DMA <- DMA
    }
    if (is.character(DMA) && file.exists(DMA_dir)) {
    # Treat as a file path
      DMA <- sf::st_read(DMA_dir, DMA, quiet = TRUE, stringsAsFactors = FALSE, options = "ENCODING=WINDOWS-1252")
      }

    if (is.data.frame(DMA)) {
    DMA <- DMA
    }

 #Read in look up tables
  # LU_nlcd <- readr::read_csv("//deqhq1/TMDL/DMA_Mapping/Main/Lookups/LandcoverClassification.csv", locale = locale(encoding = "latin1"))
  # LU_owner <- readr::read_csv("//deqhq1/TMDL/DMA_Mapping/Main/Lookups/TaxlotOwners.csv", locale = locale(encoding = "latin1"))
  # LU_zoning<- readr::read_csv("//deqhq1/TMDL/DMA_Mapping/Main/Lookups/ZoningClassification.csv", locale = locale(encoding = "latin1"))
  # LU_rail <- readr::read_csv("//deqhq1/TMDL/DMA_Mapping/Main/Lookups/railroads.csv", locale = locale(encoding = "latin1"))
  # LU_roads <- readr::read_csv("//deqhq1/TMDL/DMA_Mapping/Main/Lookups/roads.csv", locale = locale(encoding = "latin1"))
  # LU_DMAs<- readr::read_csv("//deqhq1/TMDL/DMA_Mapping/Main/Lookups/DMAs.csv", locale = locale(encoding = "latin1"))

  #Read in look up tables as rdata frames saved in DMAmapping package
  data(LU_DMAs)
  data(LU_owner)
  data(LU_rail)
  data(LU_roads)
  data(LU_zoning)
  if (region == "western") {
    data(LU_nlcd_wr)
  }
  if (region == "eastern") {
    data(LU_nlcd_er)
  }

  #create data frame of all polygons with manually edited DMAs
  Edit <- DMA %>%
    dplyr::filter(!is.na(edit))

  #Remove all polygons with manually edited DMAs from DMA data frame
  DMA <- DMA %>%
    dplyr::filter(is.na(edit))

  #delete previous DMA assignments for all polygons
  DMA <- DMA %>%
    dplyr::mutate(DMA_RP_Ab = as.character(NA),
                  DMA_RP = as.character(NA),
                  DMA_RP_Cl = as.character(NA),
                  DMA_RP2_Ab = as.character(NA),
                  DMA_RP2 = as.character(NA),
                  DMA_RP2_Cl = as.character(NA),
                  edit = as.character(NA),
                  Symbol = as.character(NA),
                  Version = as.character(NA)
    )

  ##A: Roads##
  if(nrow(DMA) > 0 & gaps == FALSE){

    DMA$DMA_RP_Ab <- ifelse((grepl("ROAD", DMA$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                is.na(DMA$Tribe) &
                                DMA$RoadOwner %in% LU_roads$ROADOWNER) |
                               (grepl("NON", DMA$Taxlot,ignore.case = TRUE, fixed = FALSE) &
                                  DMA$RoadOwner %in% LU_roads$ROADOWNER),
                             LU_roads$DMA[match(DMA$RoadOwner, LU_roads$ROADOWNER)],
                             DMA$DMA_RP_Ab)

    DMA$DMA_RP_Ab <- ifelse(is.na(DMA$Taxlot) &
                               is.na(DMA$Tribe) &
                               DMA$RoadOwner %in% LU_roads$ROADOWNER,
                             LU_roads$DMA[match(DMA$RoadOwner, LU_roads$ROADOWNER)],
                             DMA$DMA_RP_Ab)

    Roads <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab))

    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab))

  }

  if(nrow(DMA) > 0 & gaps == TRUE){

    DMA$DMA_RP_Ab <- ifelse(is.na(DMA$Taxlot) &
                               is.na(DMA$Tribe) &
                               DMA$RoadOwner %in% LU_roads$ROADOWNER,
                             LU_roads$DMA[match(DMA$RoadOwner, LU_roads$ROADOWNER)],
                             DMA$DMA_RP_Ab)


    Roads <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab))
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab))

  }

  ##B: Railroads##
  if(nrow(DMA) > 0 & gaps == FALSE){

    DMA$DMA_RP_Ab <- ifelse((grepl("RAIL", DMA$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                DMA$RailOwner %in% LU_rail$RR_NAME) |
                               (grepl("NON", DMA$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                  DMA$RailOwner %in% LU_rail$RR_NAME),
                             LU_rail$DMA[match(DMA$RailOwner, LU_rail$RR_NAME)],
                             DMA$DMA_RP_Ab)

    DMA$DMA_RP_Ab <- ifelse(is.na(DMA$Taxlot) &
                               is.na(DMA$Tribe) &
                               DMA$RailOwner %in% LU_rail$RR_NAME,
                             LU_rail$DMA[match(DMA$RailOwner, LU_rail$RR_NAME)],
                             DMA$DMA_RP_Ab)

    DMA$DMA_RP2_Ab <- ifelse(is.na(DMA$DMA_RP2_Ab) &
                                is.na(DMA$Tribe) &
                                DMA$RailInt %in% LU_rail$RR_NAME,
                              LU_rail$DMA[match(DMA$RailInt, LU_rail$RR_NAME)],
                              DMA$DMA_RP2_Ab)

    Rails <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab))
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab))

  }

  if(nrow(DMA) > 0 & gaps == TRUE){

    DMA$DMA_RP_Ab <- ifelse(is.na(DMA$Taxlot) &
                               is.na(DMA$Tribe) &
                               DMA$RailOwner %in% LU_rail$RR_NAME,
                             LU_rail$DMA[match(DMA$RailOwner, LU_rail$RR_NAME)],
                             DMA$DMA_RP_Ab)

    DMA$DMA_RP_Ab2 <- ifelse(is.na(DMA$DMA_RP_Ab2) &
                                is.na(DMA$Tribe) &
                                DMA$RailInt %in% LU_rail$RR_NAME,
                              LU_rail$DMA[match(DMA$RailInt, LU_rail$RR_NAME)],
                              DMA$DMA_RP_Ab2)

    Rails <- DMA %>% dplyr::filter(!is.na(DDMA_RP_Ab))
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab))

  }

  ##C: Tribal Areas##
  if(nrow(DMA) > 0 & tribal == TRUE){
    DMA$DMA_RP_Ab <- ifelse(!is.na(DMA$Tribe), DMA$Tribe, DMA$DMA_RP_Ab)
    Tribal <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab))
    Tribal$DMA_RP_Ab <- ifelse(!is.na(Tribal$Tribe),
                               paste0("TRIBE ", Tribal$DMA_RP_Ab),
                               Tribal$DMA_RP_Ab)
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab))
  }

  ##D: City Limits##
  if(nrow(DMA) > 0){
    DMA$DMA_RP_Ab <- ifelse(!is.na(DMA$CityName),
                             DMA$CityName,
                             DMA$DMA_RP_Ab)
    Cities <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab))
    Cities$DMA_RP_Ab <- paste0("City of ", Cities$DMA_RP_Ab)
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab))

  }

  ##E: Public Land Management##

  if(nrow(DMA) > 0){
    if (pubyear==2015){
      DMA$DMA_RP_Ab <- ifelse( DMA$LandManage != "PV" &
                                  DMA$LandManage != "PVI" &
                                  DMA$LandManage != "LG"&
                                  DMA$LandManage != "BIA"&
                                  DMA$LandManage != "TRIBAL"&
                                  DMA$LandManage != "GSA",
                                DMA$LandManage,
                                DMA$DMA_RP_Ab)
    }

    if (pubyear==2019){
      DMA$DMA_RP_Ab <- ifelse( DMA$LandManage != "PV" &
                                  DMA$LandManage != "PVI" &
                                  DMA$LandManage != "PVN" &
                                  DMA$LandManage != "PVU" &
                                  DMA$LandManage != "LG"&
                                  DMA$LandManage != "BIA"&
                                  DMA$LandManage != "UND"&
                                  DMA$LandManage != "GSA",
                                DMA$LandManage,
                                DMA$DMA_RP_Ab)
    }

    Public <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab))


    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="ODF",
                               gsub("ODF$", "ODF-Public", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="NPS",
                               gsub("NPS", "USNPS", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="FWS",
                               gsub("FWS", "USFWS", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="DOD",
                               gsub("DOD", "USDOD", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="DOE",
                               gsub("DOE", "USDOE", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="BR",
                               gsub("BR", "USBR", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="COE",
                               gsub("COE", "USACE", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="STF",
                               gsub("STF", "ODF-Public", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="STL",
                               gsub("STL", "ODSL", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="STP",
                               gsub("STP", "OPRD", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="STW",
                               gsub("STW", "ODFW", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    Public$DMA_RP_Ab <- ifelse(Public$DMA_RP_Ab=="ST",
                               gsub("ST", "OR", Public$DMA_RP_Ab),
                               Public$DMA_RP_Ab)

    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab))
  }

  ##F: Tax Lot Ownership##
  if(nrow(DMA) > 0){
    DMA$DMA_RP_Ab <- ifelse(DMA$OwnerName %in% LU_owner$Owner_Name,
                             LU_owner$DMA[match(DMA$OwnerName, stringr::regex(LU_owner$Owner_Name, ignore_case=TRUE))],
                             DMA$DMA_RP_Ab)
    TaxlotOwners <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab))
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab))
  }



  ##G: Zoning
  if(nrow(DMA) > 0){
    DMA$DMA_RP_Ab <- ifelse(DMA$orZClass == "Mining" &
                               !is.na(DMA$OwnerName),
                             "DOGAMI",
                             DMA$DMA_RP_Ab)

    DMA$DMA_RP_Ab <- ifelse(DMA$orZClass == "Forestry" &
                               !is.na(DMA$OwnerName),
                             "ODF-Private",
                             DMA$DMA_RP_Ab)

    DMA$DMA_RP_Ab <- ifelse(DMA$orZClass == "Agriculture" &
                               !is.na(DMA$OwnerName),
                             "ODA",
                             DMA$DMA_RP_Ab)

    Zoning <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab ))
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab ))
  }


  ##H: NLCD
  if(nrow(DMA) > 0){
    DMA$DMA_RP_Ab <- ifelse(DMA$orZClass == "Agriculture & Forestry" &
                               DMA$NLCD_Class == "Forest" &
                               !is.na(DMA$OwnerName),
                             "ODF-Private",
                             DMA$DMA_RP_Ab)

    DMA$DMA_RP_Ab <- ifelse(DMA$orZClass == "Agriculture & Forestry" &
                               DMA$NLCD_Class == "Agriculture" &
                               !is.na(DMA$OwnerName),
                             "ODA",
                             DMA$DMA_RP_Ab)

    DMA$DMA_RP2_Ab <- ifelse(DMA$orZClass == "Agriculture & Forestry" &
                                DMA$DMA_RP_Ab == "ODA",
                              "ODF-Private",
                              DMA$DMA_RP2_Ab)

    DMA$DMA_RP2_Ab <- ifelse(DMA$orZClass == "Agriculture & Forestry" &
                                DMA$DMA_RP_Ab == "ODF-Private",
                              "ODA",
                              DMA$DMA_RP2_Ab)

    NLCD <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab ))
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab ))
  }

  ##I: Water##
  if(nrow(DMA) > 0){
    if (pubyear==2015){
      DMA$DMA_RP_Ab <- ifelse((grepl("WATER", DMA$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                  DMA$LandManage != "PV" &
                                  DMA$LandManage != "PVI" &
                                  DMA$LandManage != "LG"&
                                  DMA$LandManage != "BIA"&
                                  DMA$LandManage != "TRIBAL"&
                                  DMA$LandManage != "GSA"),
                               DMA$LandManage,
                               DMA$DMA_RP_Ab)
    }

    if (pubyear==2019){
      DMA$DMA_RP_Ab <- ifelse((grepl("WATER", DMA$Taxlot, ignore.case = TRUE, fixed = FALSE) &
                                  DMA$LandManage != "PV" &
                                  DMA$LandManage != "PVI" &
                                  DMA$LandManage != "PVN" &
                                  DMA$LandManage != "PVU" &
                                  DMA$LandManage != "LG"&
                                  DMA$LandManage != "BIA"&
                                  DMA$LandManage != "UND"&
                                  DMA$LandManage != "GSA"),
                               DMA$LandManage,
                               DMA$DMA_RP_Ab)
    }

    DMA$DMA_RP_Ab <- ifelse(is.na(DMA$DMA_RP_Ab) &
                               grepl("WATER", DMA$Taxlot, ignore.case = TRUE, fixed = FALSE),
                             "WATER",
                             DMA$DMA_RP_Ab)

    Water <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab ))

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="ODF",
                              gsub("ODF$", "ODF-Public", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="NPS",
                              gsub("NPS", "USNPS", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="FWS",
                              gsub("FWS", "USFWS", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="DOD",
                              gsub("DOD", "USDOD", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="DOE",
                              gsub("DOE", "USDOE", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="BR",
                              gsub("BR", "USBR", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="COE",
                              gsub("COE", "USACE", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="ST",
                              gsub("ST", "OR", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="STF",
                              gsub("STF", "ODF-Public", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="STL",
                              gsub("STL", "ODSL", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="STP",
                              gsub("STP", "OPRD", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    Water$DMA_RP_Ab <- ifelse(Water$DMA_RP_Ab=="STW",
                              gsub("STW", "ODFW", Water$DMA_RP_Ab),
                              Water$DMA_RP_Ab)

    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab ))
  }

  ##J: County##
  #If a DMA has not yet been assigned, the county is the DMA
  if(nrow(DMA) > 0){
    DMA$DMA_RP_Ab <- countyname
    County <- DMA %>% dplyr::filter(!is.na(DMA_RP_Ab ))
    DMA <- DMA %>% dplyr::filter(is.na(DMA_RP_Ab ))
  }

  #recombine all subsets of the original tax lot data
  if(nrow(DMA) == 0 & tribal== TRUE){
    DMA <- rbind(Cities, County, Edit, NLCD, Public, Rails, Roads, TaxlotOwners, Tribal, Water, Zoning)
  }

  if(nrow(DMA) == 0 & tribal== FALSE){
    DMA <- rbind(Cities, County, Edit, NLCD, Public, Rails, Roads, TaxlotOwners, Water, Zoning)
  }

  #add official DMA name
  DMA$DMA_RP <- ifelse(DMA$DMA_RP_Ab %in% LU_DMAs$DMA,
                       LU_DMAs$DMA_FullName[match(DMA$DMA_RP_Ab, LU_DMAs$DMA)],
                       DMA$DMA_RP)

  #add official DMA name for DMA2
  DMA$DMA_RP2 <- ifelse(DMA$DMA_RP2_Ab %in% LU_DMAs$DMA,
                        LU_DMAs$DMA_FullName[match(DMA$DMA_RP2_Ab, LU_DMAs$DMA)],
                        DMA$DMA_RP2)

  #Add DMA classification
  DMA$DMA_RP_Cl <- ifelse(DMA$DMA_RP_Ab %in% LU_DMAs$DMA,
                          LU_DMAs$DMA_Class[match(DMA$DMA_RP_Ab, LU_DMAs$DMA)],
                          DMA$DMA_RP_Cl)

  #Add DMA2 classification
  DMA$DMA_RP2_Cl <- ifelse(DMA$DMA_RP2_Ab %in% LU_DMAs$DMA,
                           LU_DMAs$DMA_Class[match(DMA$DMA_RP2_Ab, LU_DMAs$DMA)],
                           DMA$DMA_RP2_Cl)

  #Add symbology
  DMA$Symbol <-ifelse(DMA$DMA_RP_Ab %in% LU_DMAs$DMA,
                      LU_DMAs$Symbol[match(DMA$DMA_RP_Ab, LU_DMAs$DMA)],
                      DMA$Symbol)
  #Add Version
  DMA$Version <- "Jefferson_2019-2"

  return(DMA)
}

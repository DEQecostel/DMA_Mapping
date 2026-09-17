#' Calculate majority NLCD per polygon and update NAs
#'
#' @param DMA_dir This is the file path to where the GIS file of version 1 DMA assignments for a county
#' @param DMA_file This is the file name of the GIS file of version 1 DMA assignment for a county
#' @param nlcd This is the file path to the nlcd raster
#'
#' @returns DMA_1
#' @export
#'
#' @examples
#' DMA_v1 <- nlcd_majority(nlcd="C:/Users/ecostel/DMA_Mapping/DMA_Mapping/GIS/NLCD_2016_Land_Cover_L48_20190424.img", DMA_dir = "C:/Users/ecostel/DMA_Mapping/DMA_Mapping/GIS", DMA_file = "Jefferson_DMAs_2019-2E" )

nlcd_majority<- function(nlcd, DMA_dir, DMA_file) {

  #load packages
  library(dplyr)
  library(exactextractr)
  library(readr)
  library(sf)
  library(terra)

  #DMA shapefile
  DMA <- sf::st_read(DMA_dir,DMA_file, stringsAsFactors = FALSE, options = "ENCODING=WINDOWS-1252")
  #NLCD raster
  nlcd <- terra::rast(nlcd)

  #Look up tables
  LU_nlcd <- read_csv("//deqhq1/TMDL/DMA_Mapping/Main/Lookups/LandcoverClassification.csv", locale = locale(encoding = "latin1")) |>
    rename(NLCD = NLCD.Code)

  #Create FID field for DMA polygons
  DMA$FID <- 0:(nrow(DMA) - 1)

  #extract DMA CRS and project polygon layer to match raster
  DMA_crs <- crs(DMA)
  DMA_p <- DMA |>
    st_transform(crs(nlcd))

  #Calculate majority NLCD per polygon
  results <- exact_extract(nlcd,
                           DMA_p,
                           fun = 'majority',
                           append_cols= "FID")

  #Join NLCD data to tax lots polygons.
  #If NLCD code is 0, replace with NA. 0 is not a valid nlcd code and indicates no data.
  #Use coalesce to add newly calculated nlcd majority code, NLCD type, and NLCD classification to polygons missing NLCD data.
  DMA_1 <- DMA |>
    left_join(results, by = "FID") |>
    mutate(NLCD = if_else(NLCD==0, NA, NLCD)) |>
    mutate(NLCD = coalesce(NLCD, majority)) |>
    left_join(LU_nlcd, by = "NLCD" ) |>
    mutate(NLCD_Type = coalesce(NLCD_Type, Description)) |>
    mutate(NLCD_Class.x = coalesce(NLCD_Class.x, NLCD_Class.y)) |>
    select(-majority, -Description, -NLCD_Class.y, -Details, -Notes, -FID) |>
    rename(NLCD_Class = NLCD_Class.x)

 return(DMA_1)
}

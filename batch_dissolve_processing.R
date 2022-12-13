# This script will read all the final undissolved county DMA features,
# adds a version based on the file name,
# unions HUC12 polygon into feature,
# dissolves them one by one by certain fields 
# (DMA_RP, DMA_RP_Ab, DMA_RP_Cl, Symbol, HUC12, Version), 
# and re-saves the dissolved feature as a shapefile back to a final dissolve 
# directory. From there, the each county shapefile should be merged into a single 
# statewide feature using ESRI.

library(sf)
library(dplyr)
library(units)
library(readxl)

paths <- readxl::read_excel(path = "gis_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            col_types = c('text', 'text', 'text',
                                          'text', 'text', 'text'))

huc_dir <- paths$huc_dir_path[1]
huc_12_fc_name <- "WBDHU12"

# Directory to the Final County DMA features (undissolved)
dma_in_dir = paths$dma_in_dir_path[1]

# Directory where to save dissolved DMA features
dma_out_dir = paths$dma_out_dir_path[1]

dmaGIS <- list(
  "Baker" = file.path(dma_in_dir, "Baker_DMAs_2019-1"),
  "Benton" = file.path(dma_in_dir, "Benton_DMAs_2019-1"),
  "Clackamas" = file.path(dma_in_dir, "Clackamas_DMAs_2019-1"),
  "Clatsop" = file.path(dma_in_dir, "Clatsop_DMAs_2019-1"),
  "Columbia" = file.path(dma_in_dir, "Columbia_DMAs_2019-1"),
  "Coos" = file.path(dma_in_dir, "Coos_DMAs_2019-1"),
  "Crook" = file.path(dma_in_dir, "Crook_DMAs_2019-1"),
  "Curry" = file.path(dma_in_dir, "Curry_DMAs_2019-1"),
  "Deschutes" = file.path(dma_in_dir, "Deschutes_DMAs_2019-1"),
  "Douglas" = file.path(dma_in_dir, "Douglas_DMAs_2017-1"),
  "Gilliam" = file.path(dma_in_dir, "Gilliam_DMAs_2020-1"),
  "Grant" = file.path(dma_in_dir, "Grant_DMAs_2020-1"),
  "Harney" = file.path(dma_in_dir, "Harney_DMAs_2019-1"),
  "Hood River" = file.path(dma_in_dir, "HoodRiver_DMAs_2020-1"),
  "Jackson" = file.path(dma_in_dir, "Jackson_DMAs_2019-1"),
  "Jefferson" = file.path(dma_in_dir, "Jefferson_DMAs_2019-1"),
  "Josephine" = file.path(dma_in_dir, "Josephine_DMAs_2019-1"),
  "Klamath" = file.path(dma_in_dir, "Klamath_DMAs_2019-1"),
  "Lake" = file.path(dma_in_dir, "Lake_DMAs_2019-1"),
  "Lane" = file.path(dma_in_dir, "Lane_DMAs_2019-1"),
  "Lincoln" = file.path(dma_in_dir, "Lincoln_DMAs_2019-1"),
  "Linn" = file.path(dma_in_dir, "Linn_DMAs_2019-1"),
  "Malheur" = file.path(dma_in_dir, "Malheur_DMAs_2020-1"),
  "Marion" = file.path(dma_in_dir, "Marion_DMAs_2019-1"),
  "Morrow" = file.path(dma_in_dir, "Morrow_DMAs_2019-1"),
  "Multnomah" = file.path(dma_in_dir, "Multnomah_DMAs_2019-1"),
  "Polk" = file.path(dma_in_dir, "Polk_DMAs_2019-1"),
  "Sherman" = file.path(dma_in_dir, "Sherman_DMAs_2020-1"),
  "Tillamook" = file.path(dma_in_dir, "Tillamook_DMAs_2019-1"),
  "Umatilla" = file.path(dma_in_dir, "Umatilla_DMAs_2019-1"),
  "Union" = file.path(dma_in_dir, "Union_DMAs_2019-1"),
  "Wallowa" = file.path(dma_in_dir, "Wallowa_DMAs_2019-1"),
  "Wasco" = file.path(dma_in_dir, "Wasco_DMAs_2019-2"),
  "Washington" = file.path(dma_in_dir, "Washington_DMAs_2019-1"),
  "Wheeler" = file.path(dma_in_dir, "Wheeler_DMAs_2019-1"),
  "Yamhill" = file.path(dma_in_dir, "Yamhill_DMAs_2019-1"))

huc_12 <- read_sf(dsn = huc_dir, layer = huc_12_fc_name, 
                  stringsAsFactors = FALSE, int64_as_string = FALSE)

# Transform into Web Mercator (WGS_1984_Web_Mercator_Auxiliary_Sphere)
huc_12 <- st_transform(huc_12, crs = 3857) %>% select(HUC12)

# TEST
#county_name <- "Baker"

# This function reads in the county DMA features and does some processing and a dissolve
dma_dissolve <- function(df_county, dmaGIS = dmaGIS, huc_12 = huc_12, out_dir = out_dir) {
  
  county_name <- unique(df_county$County)[1]
  
  gispath <- dmaGIS[[county_name]]

  print(paste0("Processing ", county_name))
  start_time <- Sys.time()
  
  dma.shp <- sf::read_sf(dsn = dirname(gispath), layer = basename(gispath)) %>% 
    dplyr::mutate(Version = gsub("_DMAs","", basename(gispath)))
  
  # clean up potential topology errors in DMA shapefile
  dma.shp2 <- sf::st_buffer(dma.shp, dist = 0)
  
  # Only Keep certain fields
  dma.shp2 <- dma.shp2 %>%
    dplyr::select(DMA_RP, DMA_RP_Ab, DMA_RP_Cl, Symbol, Version) 
  
  # Transform the dma feature into Web Mercator (WGS_1984_Web_Mercator_Auxiliary_Sphere)
  dma.shp3 <- sf::st_transform(dma.shp2, crs = 3857)
  
  # dissolve and intersect the DMA feature with HUC12
  dma.shp4 <- dma.shp3 %>%
    dplyr::group_by(DMA_RP, DMA_RP_Ab, DMA_RP_Cl, Symbol, Version) %>%
    dplyr::summarise()
  
  dma.shp5 <- sf::st_intersection(x = dma.shp4, y = huc_12) %>%
    dplyr::group_by(DMA_RP, DMA_RP_Ab, DMA_RP_Cl, Symbol, HUC12, Version) %>%
    dplyr::summarise()
  
  # Cleanup
  dma.shp6 <- dma.shp5 %>%
    dplyr::mutate(HUC6 = substr(HUC12, 1, 6),
                  HUC8 = substr(HUC12, 1, 8),
                  HUC10 = substr(HUC12, 1, 10)) %>%
    dplyr::select(DMA_RP, DMA_RP_Ab, DMA_RP_Cl, Symbol, HUC6, HUC8, HUC10, HUC12, Version)
  
  end_time <- Sys.time()
  print(paste0("Done processing ",county_name, " ", round(as.numeric(end_time - start_time, units = "mins"), 1)," minutes"))
  
  # Output to shapefile
  sf::st_write(dma.shp6, file.path(dma_out_dir, paste0(basename(gispath),"_dissolve.shp")), delete_layer = TRUE)
  
}

df_county <- data.frame(County = names(dmaGIS))

dma_shp_final <- df_county %>% 
  dplyr::group_by(County) %>%
  dplyr::group_split(.keep = TRUE) %>%
  lapply(FUN = dma_dissolve, dmaGIS = dmaGIS, huc_12 = huc_12, out_dir = out_dir) %>%
  dplyr::bind_rows()


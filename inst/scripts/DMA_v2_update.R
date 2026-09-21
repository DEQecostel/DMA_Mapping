#script for updating a version 1 county DMA shapefile to version 2.
#Version 2 updates include adding missing NLCD data to polygon then reassigning DMAs based on the updated NLCD data.
#NLCD data is typically missing due to overlapping duplicate polygons.

start_time <- Sys.time()
#Calculate the majority NLCD code for each polygon then add the information to the NLCD data cells that are currently empty.
DMA_v1 <- nlcd_majority(nlcd="C:/Users/ecostel/DMA_Mapping/DMA_Mapping/GIS/NLCD_2016_Land_Cover_L48_20190424.img",
                            DMA_dir = "C:/Users/ecostel/DMA_Mapping/DMA_Mapping/GIS",
                            DMA_file = "Jefferson_DMAs_2019-2E" )

#Reassign DMAs for all polygons based on updated nlcd data, except for those that have been flagged as edited manually.
DMA_v2 <- assign_dma(DMA= DMA_fc[1,],
                     countyname = "Jefferson County",
                     gaps= FALSE,
                     tribal = TRUE,
                     zcode = TRUE,
                     pubyear= 2019,
                     region = "eastern")

end_time <- Sys.time()
processing_time <-  end_time - start_time

#st_write(DMA_v2, "C:/Users/ecostel/DMA_Mapping/DMA_Mapping/GIS/Jefferson_DMAs_2019-2.shp")



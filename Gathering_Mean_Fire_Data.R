rm(list = ls())
wd = getwd()

#import libraries
library(raster)
library(rgdal)
library(stringr)

#Each set of files (fire perimeters, LST, NDVI, and TA) were projected at the Command Line
#The tifs were also re-sampled at the command line to provide a better/more precise clip

#Land Surface Temperature
#----

#input files 
fireshapes <- list.files("fireshapes_p",pattern = ".shp", full.names = T)
lst_firetifs <- list.files("LST_rs",pattern = ".tif", full.names = T)

#assign output file naming convention
lst_firetifs.outfile <- gsub("fireshapes_p","fire_images",fireshapes)
lst_firetifs.outfile <- gsub(".shp","_l.tif",lst_firetifs.outfile)

#isolate Jul dates for lst and fire perimeters
shp.juldate <- str_sub(fireshapes, -11, -5)
lst.juldate <-str_sub(lst_firetifs, 17, 23)
print(lst.juldate)
print(shp.juldate)

#match shps and lst tifs on the Jul date
for(i in 1:length(fireshapes)){
  for(j in 1:length(lst_firetifs)){
    try({
      if(shp.juldate[i] == lst.juldate[j]){
        r <-raster(lst_firetifs[j])
        shp1 <- readOGR(fireshapes[i])
        rc <- crop(r, shp1)
        rc <- mask(x = rc, mask = shp1)
        rc <- writeRaster(rc, lst_firetifs.outfile[j], overwrite = TRUE)
      }
      else(print("No Match"))
    })
  }
}

#Checked the cropped & masked tif
cropped_r = raster('fire_images/7_2018247_l.tif')
spplot(cropped_r)

#NDVI
#----

#looking at the ndvi raster
spplot(ndvi_rs)

#ndvi input files
ndvi_firetifs <- list.files("NDVI_rs",pattern = ".tif", full.names = T)

#setting up naming convention for ndvi
ndvi_firetifs.outfile <- gsub("fireshapes_p","fire_images", fireshapes)
ndvi_firetifs.outfile <- gsub(".shp","_n.tif", ndvi_firetifs.outfile)

#isolating the ndvi jul date
ndvi.juldate <-str_sub(ndvi_firetifs, 18, 24)
print(ndvi.juldate)

#loop to match the shp fils to the ndvis within 16 days
for(i in 1:length(fireshapes)){
  for(j in 1:length(ndvi_firetifs)){
    try({
      ndvi.tempjul <- (as.integer(ndvi.juldate[j]) + 16)
      if(shp.juldate[i] >= ndvi.juldate[j] && shp.juldate[i] <= ndvi.tempjul){
        r2 <-raster(ndvi_firetifs[j])
        shp3 = readOGR(fireshapes[i])
        rc2 <- crop(r2, shp3)
        rc2 <- mask(x = rc2, mask = shp3)
        rc2 <- writeRaster(rc2, ndvi_firetifs.outfile[j], overwrite = TRUE)
        }
      else(print("No match"))
    })
  }
}

#Thermal Anomalies 
#----

#TA input files
ta_firetifs <- list.files("TA_rs",pattern = ".tif", full.names = T)

#outfile names for ta
ta_firetifs.outfile <- gsub("fireshapes_p","fire_images",fireshapes)
ta_firetifs.outfile <- gsub(".shp","_t.tif",ta_firetifs.outfile)

#isolating ta jul date
ta.juldate <-str_sub(ta_firetifs, 16, 22)
print(ta.juldate)

#loop to match the shp fils to the ta within 8 days
for(i in 1:length(fireshapes)){
  for(j in 1:length(ta_firetifs)){
    try({
      ta.tempjul <- (as.integer(ta.juldate[j]) + 8)
      if(shp.juldate[i] >= ta.juldate[j] && shp.juldate[i] <= ta.tempjul){
        r3 <-raster(ta_firetifs[j])
        shp4 = readOGR(fireshapes[i])
        rc3 <- crop(r3, shp4)
        rc3 <- mask(x = rc3, mask = shp4)
        rc3 <- writeRaster(rc3, ta_firetifs.outfile[j], overwrite = TRUE)
        }
      else(print("No match"))
    })
  }
}

#----
#Writing the data out of R
#---

#input files for the cropped and masked images
lst_clipped <- list.files("fire_images", pattern = "_l.tif", full.names = T)
ndvi_clipped <- list.files("fire_images", pattern = "_n.tif", full.names = T)
ta_clipped <- list.files("fire_images", pattern = "_t.tif", full.names = T)

#get firename to reduce duplicates/tie information together in the dataframes
fireshapes2 <- list.files("fireshapes_p",pattern = ".shp", full.names = F)
fire_name <- gsub("fireshapes_p","fire_images",fireshapes2)
fire_name <- gsub(".shp","",fire_name)
print(fire_name)

#setting up unique ID which will be consistent across all four datasets, but
#still unique enough to serve as an ID
ta.id <-str_sub(ta_clipped, -15, -7)
lst.id <-str_sub(lst_clipped, -15, -7)
ndvi.id <-str_sub(ndvi_clipped, -15, -7)
fire.id <-str_sub(fire_name, -9, -1)
print(fire.id)

#initializing dataframes for the loops
#these will catch the means of the clipped tifs
ta_df <- data.frame(vector(mode = "character", length = length(ta_clipped)))
lst_df <- data.frame(vector(mode = "character", length = length(lst_clipped)))
ndvi_df <- data.frame(vector(mode = "character", length = length(ndvi_clipped)))
fire_df <- data.frame(vector(mode = "character", length = length(fire_name)))

#getting the mean & ID for the ta's
for(k in 1:length(ta_clipped)){
  r5 = na.omit(raster(ta_clipped[k]))
  ta = mean(getValues(r5),na.rm=TRUE)
  ta_df$ID[k] =ta.id[k]
  ta_df$ta[k] = ta  
}
print(ta_df)

#getting the mean & ID for the lsts
for(j in 1:length((lst_clipped))){
  r7 = na.omit(raster(lst_clipped[j], band=1))
  lst = mean(getValues(r7),na.rm=TRUE)
  lst_df$ID[j] <- lst.id[j]
  lst_df$lst[j] <- lst
}
print(lst_df)

#getting the mean & ID for ndvis
for(l in 1:length((ndvi_clipped))){
  r6 = na.omit(raster(ndvi_clipped[l]))
  ndvi = mean(getValues(r6),na.rm=TRUE)
  ndvi_df$ID[l] <- ndvi.id[l]
  ndvi_df$ndvi[l] <- ndvi
}
print(ndvi_df)

#getting the fire names and assigning them an ID
for(m in 1:length((fire_name))){
  fire_df$ID[m] <- fire.id[m]
  fire_df$fire[m] <- fire_name[m]
}
print(fire_df)

#joining my dataframes on the common ID
library(dplyr)

table3 = left_join(fire_df,lst_df, by = "ID")
table4 = left_join(table3,ta_df, by = "ID")
table5 = left_join(table4,ndvi_df, by = "ID")

#writing the joined tables out to a csv
#will need to delete the empty "vector" columns, but everything reads properly across
#including having NAs where data was not acquired
csv.outfile = data.frame(table5)
write.csv(csv.outfile,"C:\\Users\\local_miwvgej\\Desktop\\Wildfire_proj\\fire_training_data.csv", row.names = FALSE)


# validation test of ASCAT 2015 summer (JAS) backscatter against building data for US, Europe, China from Li et al. 2020 (RSE; https://doi.org/10.1016/j.rse.2020.111859)

# Li et al. (2020) data units: building height (m), building footprint (m2/m2), building volume (10^5 m3/km2, or 0.1 m3/m2)

library(RColorBrewer)

# first, get the max # of colors for each palette
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# then, call the brewer.pal function on each qualitative color palette
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) 
# col_vector is now a list of 74 different colors

library(stats)

smooth_name = c("NotSmooth","Smooth")

color_array = c("darkorange2","seagreen3","slateblue","black")

library(raster)

grid_res = 0.05
resolution = "0.05deg"

wd = getwd()

input_dir = paste(wd,"/city_grids_wat_bldg_gfcc_07_09_2021/",sep="")  # DO NOT read data from smoothed validation folder -- smoothing in GEE was odd and unreliable

bldg_resolution = c("0.05","smooth")
  
output_dir = paste(wd,"/ascat_validation_07_09_2021_output_files/",sep="")

# read in list of cities to be analyzed

city_list_filename = paste(wd,"/","city_list_478_lat_lon.csv", sep="")                  #  list of cities >1M

city_list = read.csv(file=city_list_filename,header = TRUE, stringsAsFactors = FALSE)
# HEADER: city_id,City,  Longitude, Latitude, Region_10, Region_10_id, validation_region, Region_13, Country  ##  region_10_id matches Mahtta et al. 2019; validation region for comparison to Li et al 2020 bldg data
#  Note: region_10 follows Mahtta et al. 2019 ERL; 
#         the three regions NorthAmerica (USA), China, and Europe were coverd in the building height, footprint, and volume analysis of Li et al. (2020, RSE) and used for ground validation

num_cities = dim(city_list)[1]

validation_city_list = subset(city_list, city_list$validation_region == "USA" | city_list$validation_region == "China" | city_list$validation_region == "Europe")
num_validation_cities = dim(validation_city_list)[1]

index_china = 0   # counters used to build regional arrays out of individual city results
index_usa = 0
index_europe = 0
index_all_grids = 0

###################################################################################################
# loop through all validation cities, building array of relevant data for validation analysis
###################################################################################################

for (icity in 1:num_validation_cities) {       #5) { 
  
  city_name = validation_city_list[icity,2]
  city_region = validation_city_list[icity,5]
  city_region_id = validation_city_list[icity,6]
  validation_region = validation_city_list[icity,7]
  
  # read in city data (5-km grid) of GHSL built fraction and population (1975, 1990, 2000, 2015) and ERS, QSCAT, and ASCAT summer (JAS) mean backscatter in dB
  
  city_grid_filename = paste(input_dir,city_name,'_city_grid_wat_bldg_gfcc','.csv', sep = '')
  
  if (city_name != "nothing") {
    
    city_grid_data_input = read.csv(file = city_grid_filename, header = TRUE, stringsAsFactors = FALSE)    
    
    # Header: 1-4:   latitude, longitude, cityname, ghsl_2015_smod, 
    #         5-12:  ghsl_bf_1975, ghsl_pop_1975, ghsl_bf_1990, ghsl_pop_1990, ghsl_bf_2000, ghsl_pop_2000, ghsl_bf_2015, ghsl_pop_2015, 
    #         13-14: SASS1978_JAS_mean, SASS1978_JAS_sd,
    #         15-30: ERS1993_JAS_mean, ERS1993_JAS_std, ERS1994_JAS_mean, ERS1994_JAS_std, ERS1995_JAS_mean, ERS1995_JAS_std, ERS1996_JAS_mean, ERS1996_JAS_std, 
    #                ERS1997_JAS_mean, ERS1997_JAS_std, ERS1998_JAS_mean, ERS1998_JAS_std, ERS1999_JAS_mean, ERS1999_JAS_std, ERS2000_JAS_mean, ERS2000_JAS_std, 
    #         31-52: QSCAT1999_JAS_mean, QSCAT1999_JAS_std, QSCAT2000_JAS_mean, QSCAT2000_JAS_std, QSCAT2001_JAS_mean, QSCAT2001_JAS_std, QSCAT2002_JAS_mean, QSCAT2002_JAS_std, 
    #                QSCAT2003_JAS_mean, QSCAT2003_JAS_std, QSCAT2004_JAS_mean, QSCAT2004_JAS_std, QSCAT2005_JAS_mean, QSCAT2005_JAS_std, QSCAT2006_JAS_mean, QSCAT2006_JAS_std, 
    #                QSCAT2007_JAS_mean, QSCAT2007_JAS_std, QSCAT2008_JAS_mean, QSCAT2008_JAS_std, QSCAT2009_JAS_mean, QSCAT2009_JAS_std, 
    #         53-80: ASCAT2007_JAS_mean, ASCAT2007_JAS_std, ASCAT2008_JAS_mean, ASCAT2008_JAS_std, ASCAT2009_JAS_mean, ASCAT2009_JAS_std, ASCAT2010_JAS_mean, ASCAT2010_JAS_std, 
    #                ASCAT2011_JAS_mean, ASCAT2011_JAS_std, ASCAT2012_JAS_mean, ASCAT2012_JAS_std, ASCAT2013_JAS_mean, ASCAT2013_JAS_std, ASCAT2014_JAS_mean, ASCAT2014_JAS_std 
    #                ASCAT2015_JAS_mean, ASCAT2015_JAS_std, ASCAT2016_JAS_mean, ASCAT2016_JAS_std, ASCAT2017_JAS_mean, ASCAT2017_JAS_std, ASCAT2018_JAS_mean, ASCAT2018_JAS_std,
    #                ASCAT2019_JAS_mean, ASCAT2019_JAS_std, ASCAT2020_JAS_mean, ASCAT2020_JAS_std 
    #         81-87: Water, density, height, volume, area [gridcell], gfcc_2015, gfcc_2015_uncertainty
    
    # NOTE, sigma-0 values are -9999 if grid cell has no data in Tom's analysis on GEE
    #       in the one city analyzed so far (29 April 2020) where this happens (Shanghai), for most -9999 grid cells, value is -9999 for ers, qscat, & ascat mean summer sig0 for all years; assumed to be open water
    #       however, for at least one grid cell, only ers sig0 values were -9999; looking at historical images on google earth, it seems this coastal grid cell was water in the 1990s, but filled in c.2000
    #       water is grid cell precent water (0 to 100) based on ???? land cover mask on GEE; this is used with a threshold value (just below) to mask non-land grid cells
    
    num_grids = dim(city_grid_data_input)[1]
    df.city_validation_grids_subset = as.data.frame(array(NA,c(num_grids,15)))
    
    df.city_validation_grids_subset[,1] = city_name
    df.city_validation_grids_subset[,2] = validation_region
    df.city_validation_grids_subset[,3] = city_grid_data_input$latitude                             # Latitude
    df.city_validation_grids_subset[,4] = city_grid_data_input$longitude                            # longitude
    df.city_validation_grids_subset[,5] = city_grid_data_input$ghsl_bf_2015                         # GHSL 2015 built fraction
    df.city_validation_grids_subset[,6] = city_grid_data_input$ASCAT2015_JAS_mean                   # ASCAT2015_mean_s0
    df.city_validation_grids_subset[,7] = 10^(city_grid_data_input$ASCAT2015_JAS_mean/10)           # ASCAT2015_mean_pr
    df.city_validation_grids_subset[,8] = city_grid_data_input$density                              # bldg density (Li et al. 2020, RSE) 
    #    df.city_validation_grids_subset[,9] = city_grid_data_input$density_smooth                       # computed below from 'density'
    df.city_validation_grids_subset[,10] = city_grid_data_input$height                              # bldg height (Li et al. 2020, RSE)
    #    df.city_validation_grids_subset[,11] = city_grid_data_input$height_smooth                       # computed below from 'height'
    df.city_validation_grids_subset[,12] = city_grid_data_input$volume                              # bldg volume (Li et al. 2020, RSE)
    #    df.city_validation_grids_subset[,13] = city_grid_data_input$volume_smooth                       # computed below from 'volume'
    df.city_validation_grids_subset[,14] = city_grid_data_input$gfcc_2015                           # GFCC forest cover percent -- NOT USED
    
    # add index for validation region
    for (igrid in 1:num_grids) {
      if (df.city_validation_grids_subset[igrid,2] == "China") {
        df.city_validation_grids_subset[igrid,15] = 1
      } else if (df.city_validation_grids_subset[igrid,2] == "Europe") {
        df.city_validation_grids_subset[igrid,15] = 2
      } else                                                     # USA
        df.city_validation_grids_subset[igrid,15] = 3
    }
    
    # Compute building_height_smooth, building_volume_smooth, building_density_smooth with r smoothing of 0.05° grid data on 5x5 grid
    
    long_lat.array = df.city_validation_grids_subset[,c(4,3)]
    colnames(long_lat.array) = c("longitude", "latitude")
    
    df_volume = df.city_validation_grids_subset[,c(4,3,12)]
    colnames(df_volume) = c("longitude", "latitude","volume")
    df_volume$volume[df_volume$volume == -9999.] = 'NA'
    
    df_density = df.city_validation_grids_subset[,c(4,3,8)]
    colnames(df_density) = c("longitude", "latitude","density")
    df_density$density[df_density$density == -9999.] = 'NA'
    
    df_height = df.city_validation_grids_subset[,c(4,3,10)]
    colnames(df_height) = c("longitude", "latitude","height")
    df_height$height[df_height$height == -9999.] = NA
    
    r_volume = rasterFromXYZ(df_volume)    # basic data
    #  plot(r_volume)
    r_density = rasterFromXYZ(df_density)    # basic data
    #  plot(r_density)
    r_height = rasterFromXYZ(df_height)    # basic data
    #  plot(r_height)
    
    # Smoothing method #3
    r3_volume = focal(r_volume, w=matrix(1,5,5), na.rm=TRUE, pad=TRUE, padValue=NA, mean)  # smoothed as 5x5 window, using mean, padding extends domain, but with NA values
    # plot(r3_volume)
    r3_density = focal(r_density, w=matrix(1,5,5), na.rm=TRUE, pad=TRUE, padValue=NA, mean)  # smoothed as 5x5 window, using mean, padding extends domain, but with NA values
    # plot(r3_density)
    r3_height = focal(r_height, w=matrix(1,5,5), na.rm=TRUE, pad=TRUE, padValue=NA, mean)  # smoothed as 5x5 window, using mean, padding extends domain, but with NA values
    # plot(r3_volume)
    
    r5_volume = disaggregate(r3_volume, 5, method = 'bilinear')
    # plot(r5_volume)
    r5_density = disaggregate(r3_density, 5, method = 'bilinear')
    # plot(r5_density)
    r5_height = disaggregate(r3_height, 5, method = 'bilinear')
    # plot(r5_height)
    
    m5_volume = extract(r5_volume,long_lat.array, method = "bilinear")  # make a matrix of the r5 raster grid value
    m5_volume_array = cbind(long_lat.array,m5_volume)
    r_m5_volume_array = rasterFromXYZ(m5_volume_array)  #   for evaluation, not used otherwise
    # plot(r_m5_volume_array)
    m5_density = extract(r5_density,long_lat.array, method = "bilinear")  # make a matrix of the r5 raster grid value
    m5_density_array = cbind(long_lat.array,m5_density)
    r_m5_density_array = rasterFromXYZ(m5_density_array)  #   for evaluation, not used otherwise
    # plot(r_m5_density_array)
    m5_height = extract(r5_height,long_lat.array, method = "bilinear")  # make a matrix of the r5 raster grid value
    m5_height_array = cbind(long_lat.array,m5_height)
    r_m5_height_array = rasterFromXYZ(m5_height_array)  #   for evaluation, not used otherwise
    # plot(r_m5_height_array)
    
    # load smoothed building data into array
    df.city_validation_grids_subset[,9]  = m5_density   # bldg density (Li et al. 2020, RSE) smoothed
    df.city_validation_grids_subset[,11]  = m5_height    # bldg height (Li et al. 2020, RSE) smoothed
    df.city_validation_grids_subset[,13]  = m5_volume    # bldg volume (Li et al. 2020, RSE) smoothed
    
    if (icity == 1) {
      df.all_grids_validation_table_full = df.city_validation_grids_subset
    } else {
      df.all_grids_validation_table_full = rbind(df.all_grids_validation_table_full, df.city_validation_grids_subset)
    }
    
  } 
}

colnames(df.all_grids_validation_table_full) = c("city","validation_region","latitude","longitude","ghsl_bf_2015","ASCAT2015_mean_s0","ASCAT2015_mean_pr",
                                                "bldg_density","bldg_density_smooth","bldg_height","bldg_height_smooth","bldg_volume","bldg_volume_smooth","gfcc_2015","region_id")

df.all_grids_validation_table_full[df.all_grids_validation_table_full == -9999] <- NA

# remove grid cells with NA values (problem with ASCAT 2015 sig0 = NA -> ASCAT 2015 PR = 0)

df.all_grids_validation_table_full = na.omit(df.all_grids_validation_table_full)

###################################################################################################
#  loop through all cases, subsetting data and computing linear fit intercept and thresholds, making regional plots 
###################################################################################################


bltfrac_thresholds = c(0.10, 0.20)  # minimum thresholds for grid cell GHSL 2015 built fraction to be included in validation
bltfract_threshold_names = c("0.10", "0.20")
num_bf_thresh = length(bltfrac_thresholds)

region_list1 = c("China", "Europe", "USA", "All")
region_list2 = c("China", "Europe", "USA", "China + USA + Europe")
num_regions = length(region_list1)

bldvarname_array = c("BldgVol","BldgHgt","BldgDen","WallArea", "BldgArea")

df.summary.ASCAT.vs.BldgVar.linearfit = as.data.frame(array(0,c((2*2*4),(5+1+1+2+1+1+2+2))))# array rows: 4 regions X 2 BF_min_vals X 2 building variables
colnames(df.summary.ASCAT.vs.BldgVar.linearfit) <- c("native_smooth","backscatter_mode","gfcc_constraint","BF_min","Bldg_variable","all/large_cities","region","number_grids",
                                                   "adj.R2","intercept","linear_coef","intercept_StdErr","linear_coef_StdErr","intercept_Pval","slope_Pval")
linear.fit.table.count = 0

backscat_mode = 2   # = 2, use backscatter in PR, or = 1 use backscatter in dB
backscatter_mode_name = c('s0', 'pr')

ismooth = 2 # =2 for smoothed data, or =1 for unsmoothed data

gfcc_name = c("<=20%",">20%","0-100%")
city_include = "all"

for (iforest in 3:3) {  # do not use forest cover mask, so only index = 3
  
  for (ibldvar in 1:2) {   #  1:2 to only use building volume and height  bldvarname_array = c("BldgVol","BldgHgt","BldgDen","WallArea", "BldgArea")
    
    pdf(paste(output_dir,"AllCities_",grid_res,"deg_",smooth_name[ismooth],"_",bldvarname_array[ibldvar],
              "_vs_ASCAT_",backscatter_mode_name[backscat_mode],"_scatter_Fit",'.pdf',sep = ''))
    par(mfrow=c(1,1),mar=c(4,4,4,4))
    
    for (ibltfrac in 1:num_bf_thresh) {
      
      df.all_grids_validation_table_bf = subset(df.all_grids_validation_table_full,df.all_grids_validation_table_full$ghsl_bf_2015 >= bltfrac_thresholds[ibltfrac])
      blt_fract_min = bltfract_threshold_names[ibltfrac]
      
      for (iregion in 1:4) {  # only use all regions, not 1:num_regions
        
        region_name1 = region_list1[iregion]
        region_name2 = region_list2[iregion]
        if (iregion < 4) {
          df.all_grids_validation_table_bf_region = subset(df.all_grids_validation_table_bf, df.all_grids_validation_table_bf$validation_region == region_list1[iregion])
        } else {
          df.all_grids_validation_table_bf_region = df.all_grids_validation_table_bf
        }
        
        colnames(df.all_grids_validation_table_bf_region) = c("city","validation_region","latitude","longitude","ghsl_bf_2015","ASCAT2015_mean_s0","ASCAT2015_mean_pr",
                                                              "bldg_density","bldg_density_smooth","bldg_height","bldg_height_smooth","bldg_volume",
                                                              "bldg_volume_smooth","gfcc_2015","region_id")
        
        if (backscat_mode == 1) {
          backscatter = df.all_grids_validation_table_bf_region$ASCAT2015_mean_s0
        } else {
          backscatter = df.all_grids_validation_table_bf_region$ASCAT2015_mean_pr
        }
        
        if (ibldvar == 1) {
          bldg_var = df.all_grids_validation_table_bf_region$bldg_volume_smooth
          bldvarname = "BldVolSmooth"
          x_axis_title = "smoothed building volume (10^5 m3/km2)"
        } else if (ibldvar == 2) {
          bldg_var = df.all_grids_validation_table_bf_region$bldg_height_smooth
          bldvarname = "BldHgtSmooth"
          x_axis_title = "smoothed building height (m)"
        } 
        
        linear.fit.backscatter = lm(backscatter ~ bldg_var)
        linear.fit_r2 = summary(linear.fit.backscatter)$adj.r.squared
        
        linear.fit.table.count = linear.fit.table.count + 1
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,1] = bldg_resolution[ismooth]
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,2] = backscatter_mode_name[backscat_mode]
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,3] = gfcc_name[iforest]
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,4] = bltfrac_thresholds[ibltfrac]
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,5] = bldvarname
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,6] = city_include
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,7] = region_name1
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,8] = length(backscatter)
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,9] = linear.fit_r2
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,10:11] = linear.fit.backscatter$coefficients[1:2]
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,12:13] = coef(summary(linear.fit.backscatter))[,2]
        df.summary.ASCAT.vs.BldgVar.linearfit[linear.fit.table.count,14:15] = coef(summary(linear.fit.backscatter))[,4]
        
        ymax = max(backscatter, na.rm = TRUE)
        ymin = min(backscatter, na.rm = TRUE)
        xmin = min(bldg_var, na.rm = TRUE)
        xmax = max(bldg_var, na.rm = TRUE)
        
        linear.fit.backscatter = lm(backscatter ~ bldg_var)
        linear.fit_r2 = summary(linear.fit.backscatter)$adj.r.squared
        
        # plot linear fit         
        
        if (backscat_mode == 1) {
          #   png_filename = paste(output_dir,city_include,"_cities_region_",region_name1,"_",resolution[ismooth],"_BFmin_",
          #                     bltfrac_thresholds[ibltfrac],"_",bldvarname,"_vs_ASCAT_scatter_LinearFit",'.png',sep = '')
          y_label = "ASCAT JAS backscatter (dB)"
          x_text = 0.8*xmax
          y_text1 = 0.8*ymin
          y_text2 = 0.85*ymin
          y_text3 = 0.9*ymin
          y_text4 = 0.95*ymin
        } else {
          # png_filename = paste(output_dir,city_include,"_cities_region_",region_name1,"_",resolution[ismooth],"_BFmin_",
          #                      bltfrac_thresholds[ibltfrac],"_",bldvarname,"_vs_ASCAT_scatter_pr_LinearFit",'.png',sep = '')
          y_label = "ASCAT JAS backscatter PR"
          x_text = 0.8*xmax
          y_text1 = 0.2*ymax
          y_text2 = 0.15*ymax
          y_text3 = 0.1*ymax
          y_text4 = 0.05*ymax
          
        }
        #  png(png_filename,width = 1000, height = 1000, units = "px")
        
        
        if (iregion < 4) {
          
          # don't make scatterplots for individual regions

                    # plot(bldg_var,backscatter,pch=15,cex=0.35,                              
          #      xlab = x_axis_title, cex.lab = 1.,cex.axis = 1.25,
          #      # xlab = x_axis_title, cex.lab = 0.75,cex.axis = 0.75,cex.main=0.75,
          #      ylab = y_label,
          #      xlim = c(xmin,xmax),
          #      ylim = c(ymin,ymax),
          #      main = paste("All cities/regions, BFmin:",bltfrac_thresholds[ibltfrac], "GFCC_forest: ",gfcc_name[iforest])
          # )
          
        } else {
          plot(bldg_var,backscatter,pch=15,cex=0.35,
               xlab = x_axis_title, cex.lab = 1.,cex.axis = 1.25,
               # xlab = x_axis_title, cex.lab = 0.75,cex.axis = 0.75,cex.main=0.75,
               ylab = y_label,
               xlim = c(xmin,xmax),
               ylim = c(0,0.8),    # c(ymin,ymax),
               col = alpha(color_array[df.all_grids_validation_table_bf_region$region_id],0.25),
               main = paste("All cities/regions, ",grid_res,"°, BFmin:",blt_fract_min, ", GFCC_forest: ",gfcc_name[iforest],sep="")
          )
          legend("topleft", c("China","Europe","USA", "All_LinFit"), pch = c(15,15,15,20),col = color_array)
          
          lines(c(0,0.9*xmax),c(linear.fit.backscatter$coefficients[1],linear.fit.backscatter$coefficients[1] + linear.fit.backscatter$coefficients[2]*0.9*xmax),col="black",lwd=2)
          
          subset_1 = subset(df.summary.ASCAT.vs.BldgVar.linearfit, df.summary.ASCAT.vs.BldgVar.linearfit$native_smooth == bldg_resolution[ismooth])
          subset_2 = subset(subset_1,subset_1$backscatter_mode == "PR")
          subset_2 = subset(subset_1,subset_1$backscatter_mode == backscatter_mode_name[backscat_mode])
          subset_3 = subset(subset_2,subset_2$gfcc_constraint == gfcc_name[iforest])
          subset_4 = subset(subset_3,subset_3$BF_min == as.numeric(bltfract_threshold_names[ibltfrac]))
          subset_5 = subset(subset_4,subset_4$Bldg_variable == bldvarname)
          
          china_intercept = subset(subset_5,subset_5$region == "China")[,10]
          china_slope = subset(subset_5,subset_5$region == "China")[,11]
          china_r2 = subset(subset_5,subset_5$region == "China")[,9]
          china_numgrid = subset(subset_5,subset_5$region == "China")[,8]
          
          europe_intercept = subset(subset_5,subset_5$region == "Europe")[,10]
          europe_slope = subset(subset_5,subset_5$region == "Europe")[,11]
          europe_r2 = subset(subset_5,subset_5$region == "Europe")[,9]
          europe_numgrid = subset(subset_5,subset_5$region == "Europe")[,8]
          
          usa_intercept = subset(subset_5,subset_5$region == "USA")[,10]
          usa_slope = subset(subset_5,subset_5$region == "USA")[,11]
          usa_r2 = subset(subset_5,subset_5$region == "USA")[,9]
          usa_numgrid = subset(subset_5,subset_5$region == "USA")[,8]
          
          lines(c(0,0.9*xmax),c(china_intercept,china_intercept + china_slope*0.9*xmax),col=color_array[1],lwd=2)
          lines(c(0,0.9*xmax),c(europe_intercept,europe_intercept + europe_slope*0.9*xmax),col=color_array[2],lwd=2)
          lines(c(0,0.9*xmax),c(usa_intercept,usa_intercept + usa_slope*0.9*xmax),col=color_array[3],lwd=2)
          
          all_intercept = linear.fit.backscatter$coefficients[1]
          all_slope = linear.fit.backscatter$coefficients[2]
          all_r2 = linear.fit_r2
          all_numgrid = length(backscatter)
          
          # lines(quadratic.fit_x.values,predicted.backscatter,col = "red", lwd = 2)
          
          text(x_text,y_text1,paste("                       All      China      Europe      USA"),cex = 0.5,col = "black")
          text(x_text,y_text2,paste("linear fit R^2: ", formatC(all_r2,digits=3,format='f'), "   ",
                                    formatC(china_r2,digits=3,format='f'),"   ", formatC(europe_r2,digits=3,format='f'),"   ", formatC(usa_r2,digits=3,format='f')),col = "black", cex= 0.5)
          text(x_text,y_text3,paste("linear fit slope: ", formatC(all_slope,digits=3,format='f'), "   ",
                                    formatC(china_slope,digits=3,format='f'),"   ", formatC(europe_slope,digits=3,format='f'),"   ", 
                                    formatC(usa_slope,digits=3,format='f')),col = "black", cex= 0.5)
          text(x_text,y_text4,paste("# grid cells: ", formatC(all_numgrid,digits=0,format='f'), "    ",
                                    formatC(china_numgrid,digits=0,format='f'),"    ", formatC(europe_numgrid,digits=0,format='f'),"    ", 
                                    formatC(usa_numgrid,digits=0,format='f')),col = "black", cex= 0.5)
          
        }
        
      }
      
    }
    
    dev.off()
  
  }
  
}

outfile.name = paste(output_dir,"all_cities_AllRegions_smooth_NoSmooth_AllBFmin_vs_ASCAT_",resolution,"_scatter_s0_pr_LinearFit",'.csv',sep = '')
write.csv(df.summary.ASCAT.vs.BldgVar.linearfit,file = outfile.name,row.names = F)
 
# make some plots for USA cities

subset_array_bf_gt_0.2 = subset(df.all_grids_validation_table_full,df.all_grids_validation_table_full$ghsl_bf_2015 >= 0.2)

subset_array_bf_gt_0.2_usa = subset(subset_array_bf_gt_0.2,subset_array_bf_gt_0.2$validation_region == "USA")

subset_array_bf_gt_0.2_nyc = subset(subset_array_bf_gt_0.2_usa,subset_array_bf_gt_0.2_usa$city == "NewYork")
subset_array_bf_gt_0.2_la = subset(subset_array_bf_gt_0.2_usa,subset_array_bf_gt_0.2_usa$city == "LosAngeles")
subset_array_bf_gt_0.2_dc = subset(subset_array_bf_gt_0.2_usa,subset_array_bf_gt_0.2_usa$city == "WashingtonDC")
subset_array_bf_gt_0.2_atl = subset(subset_array_bf_gt_0.2_usa,subset_array_bf_gt_0.2_usa$city == "Atlanta")

# Make scatter plots for 3 US cities

ymax = max(df.all_grids_validation_table_full$ASCAT2015_mean_pr, na.rm = TRUE)
ymin = min(df.all_grids_validation_table_full$ASCAT2015_mean_pr, na.rm = TRUE)
vol_xmin = min(df.all_grids_validation_table_full$bldg_volume_smooth, na.rm = TRUE)
vol_xmax = max(df.all_grids_validation_table_full$bldg_volume_smooth, na.rm = TRUE)
ht_xmin = min(df.all_grids_validation_table_full$bldg_height_smooth, na.rm = TRUE)
ht_xmax = max(df.all_grids_validation_table_full$bldg_height_smooth, na.rm = TRUE)
dens_xmin = min(df.all_grids_validation_table_full$bldg_density_smooth, na.rm = TRUE)
dens_xmax = max(df.all_grids_validation_table_full$bldg_density_smooth, na.rm = TRUE)

pdf(paste(output_dir,"USA_NYC_DC_LA_ASCAT_vs_bldg_metrics_BF2015min>=0.2",'.pdf',sep = ''))

plot(subset_array_bf_gt_0.2_nyc$bldg_volume_smooth,subset_array_bf_gt_0.2_nyc$ASCAT2015_mean_pr, pch=16, col = "red",
     ylim = c(ymin,ymax),
     xlim = c(vol_xmin,vol_xmax),
     xlab = "smoothed building volume (10^5 m3/km2)",
     ylab = "2015 mean summer ASCAT backscatter PR")
points(subset_array_bf_gt_0.2_dc$bldg_volume_smooth,subset_array_bf_gt_0.2_dc$ASCAT2015_mean_pr, pch=16, col = "blue")
points(subset_array_bf_gt_0.2_la$bldg_volume_smooth,subset_array_bf_gt_0.2_la$ASCAT2015_mean_pr, pch=16, col = "black")

# add regression lines from all regions and USA 
subset1 = subset(df.summary.ASCAT.vs.BldgVar.linearfit,df.summary.ASCAT.vs.BldgVar.linearfit$BF_min == 0.2)
subset2 = subset(subset1,subset1$Bldg_variable == "BldVolSmooth")
subset3 = subset(subset2,subset1$region == "All")
subset4 = subset(subset2,subset1$region == "USA")
USA_fit_int = subset4[1,10]
USA_fit_slope = subset4[1,11]
all_fit_int = subset3[1,10]
all_fit_slope = subset3[1,11]

lines(c(0,35),c(USA_fit_int, USA_fit_int + USA_fit_slope*35),col=color_array[3],lwd=2,lty=2)
lines(c(0,35),c(all_fit_int, all_fit_int + all_fit_slope*35),col='black',lwd=2,lty=2)

legend("topleft",c("New York City", "Washington DC","Los Angeles"),pch = c(16,16,16), col = c("red","blue","black"))

# plot(subset_array_bf_gt_0.2_nyc$bldg_height_smooth,subset_array_bf_gt_0.2_nyc$ASCAT2015_mean_pr, pch=16, col = "red",
#      ylim = c(ymin,ymax),
#      xlim = c(ht_xmin,ht_xmax),
#      xlab = "smoothed building height (m)",
#      ylab = "2015 mean summer ASCAT backscatter PR")
# points(subset_array_bf_gt_0.2_dc$bldg_height_smooth,subset_array_bf_gt_0.2_dc$ASCAT2015_mean_pr, pch=16, col = "blue")
# points(subset_array_bf_gt_0.2_la$bldg_height_smooth,subset_array_bf_gt_0.2_la$ASCAT2015_mean_pr, pch=16, col = "black")
# legend("topleft",c("New York City", "Washington DC","Los Angeles"),pch = c(16,16,16), col = c("red","blue","black"))
# 
# plot(subset_array_bf_gt_0.2_nyc$bldg_density_smooth,subset_array_bf_gt_0.2_nyc$ASCAT2015_mean_pr, pch=16, col = "red",
#      ylim = c(ymin,ymax),
#      xlim = c(dens_xmin,dens_xmax),
#      xlab = "smoothed building density/footprint (m2/m2)",
#      ylab = "2015 mean summer ASCAT backscatter PR")
# points(subset_array_bf_gt_0.2_dc$bldg_density_smooth,subset_array_bf_gt_0.2_dc$ASCAT2015_mean_pr, pch=16, col = "blue")
# points(subset_array_bf_gt_0.2_la$bldg_density_smooth,subset_array_bf_gt_0.2_la$ASCAT2015_mean_pr, pch=16, col = "black")
# legend("topleft",c("New York City", "Washington DC","Los Angeles"),pch = c(16,16,16), col = c("red","blue","black"))

dev.off()


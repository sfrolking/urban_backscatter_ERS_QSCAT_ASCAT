# July 2021
#  code to plot urban time series (and invariant time series?) for data paper

wd = getwd()

input_dir = paste(wd,"/city_grids_wat_bldg_gfcc_07_09_2021/",sep="")

output_dir = paste(wd,"/city_grids_wat_bldg_gfcc_07_09_2021_output_files_data_paper/",sep="")
# city_output_dir = paste(wd,"/city_grids_wat_bldg_gfcc_05_04_2021_output_files_by_city/",sep="")

# 12 cities to plot for data paper
city_list_filename = paste(wd,"/","city_list_12_lat_lon.csv", sep="")      # "Tokyo" , "Busan", "Urumqi"  ,"Guangzhou" , "Mumbai",  "Tehran" ,  "MexicoCity", "LasVegas", " Munich", "Moscow" , " Lagos", "Aleppo"  

city_list = read.csv(file=city_list_filename,header = TRUE, stringsAsFactors = FALSE)
# HEADER: city_id,	City,	Longitude,	Latitude,	Region_10,	Region_10_id,	validation_region,	Region_13,	Country   
#  Note: region_10 follows Mahtta et al. 2019 ERL; 
#         Validation_region: the three regions NorthAmerica (USA), China, and Europe were covered in the building height, footprint, and volume analysis of Li et al. (2020, RSE) and used for ground validation
num_cities = dim(city_list)[1]

ers_years = 1993:2000
qscat_years = 1999:2009
ascat_years = 2007:2020
ers_len = length(ers_years)
qscat_len = length(qscat_years)
ascat_len = length(ascat_years)

# minimum built fraction thresholds used in urban time series plots 
bltfrac_mins = c(0.1,0.2)
num_bf_mins = length(bltfrac_mins)

df.all_city_pr_20_slopes= as.data.frame(array(0,c(num_cities,10)))
df.all_city_pr_10_slopes= as.data.frame(array(0,c(num_cities,10)))

colnames(df.all_city_pr_20_slopes) = c('city', 'ers_pr_slope', 'ers_pr_r2', 'ers_pr_slope_pval', 'qscat_pr_slope', 'qscat_pr_r2', 'qscat_pr_slope_pval', 'ascat_pr_slope', 'ascat_pr_r2', 'ascat_pr_slope_pval')
colnames(df.all_city_pr_10_slopes) = c('city', 'ers_pr_slope', 'ers_pr_r2', 'ers_pr_slope_pval', 'qscat_pr_slope', 'qscat_pr_r2', 'qscat_pr_slope_pval', 'ascat_pr_slope', 'ascat_pr_r2', 'ascat_pr_slope_pval')

for (icity in 1:num_cities) {      
  
  city_name = city_list[icity,2]

  # read in city data (5-km grid) of GHSL built fraction and population (1975, 1990, 2000, 2015) and ERS, QSCAT, and ASCAT summer (JAS) mean backscatter in dB
  
  city_grid_filename = paste(input_dir,city_name,'_city_grid_wat_bldg_gfcc','.csv', sep = '')
  
  if (file.exists(city_grid_filename)) {
    
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
    #                ASCAT2011_JAS_mean, ASCAT2011_JAS_std, ASCAT2012_JAS_mean, ASCAT2012_JAS_std, ASCAT2013_JAS_mean, ASCAT2013_JAS_std, ASCAT2014_JAS_mean, ASCAT2014_JAS_std. 
    #                ASCAT2015_JAS_mean, ASCAT2015_JAS_std, ASCAT2016_JAS_mean, ASCAT2016_JAS_std, 
    #                ASCAT2019_JAS_mean, ASCAT2019_JAS_std, ASCAT2020_JAS_mean, ASCAT2020_JAS_std,
    #         81-87: Water, density, height, volume, area [gridcell], gfcc_2015, gfcc_2015_uncertainty
    
    # NOTE, sigma-0 values are -9999 if grid cell has no data in Tom's analysis on GEE
    #       in the one city analyzed so far (29 April 2020) where this happens (Shanghai), for most -9999 grid cells, value is -9999 for ers, qscat, & ascat mean summer sig0 for all years; assumed to be open water
    #       however, for at least one grid cell, only ers sig0 values were -9999; looking at historical images on google earth, it seems this coastal grid cell was water in the 1990s, but filled in c.2000
    #       water is grid cell precent water (0 to 100) based on ???? land cover mask on GEE; this is used with a threshold value (just below) to mask non-land grid cells
    
    water.threshold = 50  # threshold of grid cell water percentage to flag as 'water' and remove backscatter data from processing
    
    city_grid_data_input = subset(city_grid_data_input,city_grid_data_input$Water < water.threshold)
    
    city_grid_data_input_20 = subset(city_grid_data_input, city_grid_data_input$ghsl_bf_2015 >= 0.2)
    city_grid_data_input_10 = subset(city_grid_data_input, city_grid_data_input$ghsl_bf_2015 >= 0.1)
    
    city_grid_data_input_20[city_grid_data_input_20 == -9999] <- NA
    city_grid_data_input_10[city_grid_data_input_10 == -9999] <- NA
    
    num_grids_20 = dim(city_grid_data_input_20)[1]
    num_grids_10 = dim(city_grid_data_input_10)[1]
    
    city_grids_20 = city_grid_data_input_20[,c(1,2,3,11)]  # lat, lon, name, BF_2014
    city_grids_10 = city_grid_data_input_10[,c(1,2,3,11)]  # lat, lon, name, BF_2014
    
# build arrays of summer season backscatter for each sensor for each BF_2014 min values
    
    if (max(city_grid_data_input_20$latitude, na.rm=T) >= 0) { # northern hemisphere, so 'summer' = JAS
      
      city_grids_ers_s0_20 = cbind(city_grids_20, 
                                   city_grid_data_input_20$ERS1993_JAS_mean, city_grid_data_input_20$ERS1994_JAS_mean, city_grid_data_input_20$ERS1995_JAS_mean, city_grid_data_input_20$ERS1996_JAS_mean,
                                   city_grid_data_input_20$ERS1997_JAS_mean, city_grid_data_input_20$ERS1998_JAS_mean, city_grid_data_input_20$ERS1999_JAS_mean, city_grid_data_input_20$ERS2000_JAS_mean)
 
      city_grids_qscat_s0_20 = cbind(city_grids_20, 
                                   city_grid_data_input_20$QSCAT1999_JAS_mean, city_grid_data_input_20$QSCAT2000_JAS_mean, city_grid_data_input_20$QSCAT2001_JAS_mean, city_grid_data_input_20$QSCAT2002_JAS_mean,
                                   city_grid_data_input_20$QSCAT2003_JAS_mean, city_grid_data_input_20$QSCAT2004_JAS_mean, city_grid_data_input_20$QSCAT2005_JAS_mean, city_grid_data_input_20$QSCAT2006_JAS_mean,
                                   city_grid_data_input_20$QSCAT2007_JAS_mean, city_grid_data_input_20$QSCAT2008_JAS_mean, city_grid_data_input_20$QSCAT2009_JAS_mean)
      
      city_grids_ascat_s0_20 = cbind(city_grids_20, 
                                     city_grid_data_input_20$ASCAT2007_JAS_mean, city_grid_data_input_20$ASCAT2008_JAS_mean, city_grid_data_input_20$ASCAT2009_JAS_mean, city_grid_data_input_20$ASCAT2010_JAS_mean,
                                     city_grid_data_input_20$ASCAT2011_JAS_mean, city_grid_data_input_20$ASCAT2012_JAS_mean, city_grid_data_input_20$ASCAT2013_JAS_mean, city_grid_data_input_20$ASCAT2014_JAS_mean,
                                     city_grid_data_input_20$ASCAT2015_JAS_mean, city_grid_data_input_20$ASCAT2016_JAS_mean, city_grid_data_input_20$ASCAT2017_JAS_mean, city_grid_data_input_20$ASCAT2018_JAS_mean,
                                     city_grid_data_input_20$ASCAT2019_JAS_mean, city_grid_data_input_20$ASCAT2020_JAS_mean)
      
      city_grids_ers_s0_10 = cbind(city_grids_10, 
                                   city_grid_data_input_10$ERS1993_JAS_mean, city_grid_data_input_10$ERS1994_JAS_mean, city_grid_data_input_10$ERS1995_JAS_mean, city_grid_data_input_10$ERS1996_JAS_mean,
                                   city_grid_data_input_10$ERS1997_JAS_mean, city_grid_data_input_10$ERS1998_JAS_mean, city_grid_data_input_10$ERS1999_JAS_mean, city_grid_data_input_10$ERS2000_JAS_mean)
      
      city_grids_qscat_s0_10 = cbind(city_grids_10, 
                                     city_grid_data_input_10$QSCAT1999_JAS_mean, city_grid_data_input_10$QSCAT2000_JAS_mean, city_grid_data_input_10$QSCAT2001_JAS_mean, city_grid_data_input_10$QSCAT2002_JAS_mean,
                                     city_grid_data_input_10$QSCAT2003_JAS_mean, city_grid_data_input_10$QSCAT2004_JAS_mean, city_grid_data_input_10$QSCAT2005_JAS_mean, city_grid_data_input_10$QSCAT2006_JAS_mean,
                                     city_grid_data_input_10$QSCAT2007_JAS_mean, city_grid_data_input_10$QSCAT2008_JAS_mean, city_grid_data_input_10$QSCAT2009_JAS_mean)
      
      city_grids_ascat_s0_10 = cbind(city_grids_10, 
                                     city_grid_data_input_10$ASCAT2007_JAS_mean, city_grid_data_input_10$ASCAT2008_JAS_mean, city_grid_data_input_10$ASCAT2009_JAS_mean, city_grid_data_input_10$ASCAT2010_JAS_mean,
                                     city_grid_data_input_10$ASCAT2011_JAS_mean, city_grid_data_input_10$ASCAT2012_JAS_mean, city_grid_data_input_10$ASCAT2013_JAS_mean, city_grid_data_input_10$ASCAT2014_JAS_mean,
                                     city_grid_data_input_10$ASCAT2015_JAS_mean, city_grid_data_input_10$ASCAT2016_JAS_mean, city_grid_data_input_10$ASCAT2017_JAS_mean, city_grid_data_input_10$ASCAT2018_JAS_mean,
                                     city_grid_data_input_10$ASCAT2019_JAS_mean, city_grid_data_input_10$ASCAT2020_JAS_mean)
      
      
    } else {          #  southern hemisphere, so 'summer' = JFM
      
      city_grids_ers_s0_20 = cbind(city_grids_20, 
                                   city_grid_data_input_20$ERS1993_DJF_mean, city_grid_data_input_20$ERS1994_DJF_mean, city_grid_data_input_20$ERS1995_DJF_mean, city_grid_data_input_20$ERS1996_DJF_mean,
                                   city_grid_data_input_20$ERS1997_DJF_mean, city_grid_data_input_20$ERS1998_DJF_mean, city_grid_data_input_20$ERS1999_DJF_mean, city_grid_data_input_20$ERS2000_DJF_mean)
      
      city_grids_qscat_s0_20 = cbind(city_grids_20, 
                                     city_grid_data_input_20$QSCAT1999_DJF_mean, city_grid_data_input_20$QSCAT2000_DJF_mean, city_grid_data_input_20$QSCAT2001_DJF_mean, city_grid_data_input_20$QSCAT2002_DJF_mean,
                                     city_grid_data_input_20$QSCAT2003_DJF_mean, city_grid_data_input_20$QSCAT2004_DJF_mean, city_grid_data_input_20$QSCAT2005_DJF_mean, city_grid_data_input_20$QSCAT2006_DJF_mean,
                                     city_grid_data_input_20$QSCAT2007_DJF_mean, city_grid_data_input_20$QSCAT2008_DJF_mean, city_grid_data_input_20$QSCAT2009_DJF_mean)
      
      city_grids_ascat_s0_20 = cbind(city_grids_20, 
                                     city_grid_data_input_20$ASCAT2007_DJF_mean, city_grid_data_input_20$ASCAT2008_DJF_mean, city_grid_data_input_20$ASCAT2009_DJF_mean, city_grid_data_input_20$ASCAT2010_DJF_mean,
                                     city_grid_data_input_20$ASCAT2011_DJF_mean, city_grid_data_input_20$ASCAT2012_DJF_mean, city_grid_data_input_20$ASCAT2013_DJF_mean, city_grid_data_input_20$ASCAT2014_DJF_mean,
                                     city_grid_data_input_20$ASCAT2015_DJF_mean, city_grid_data_input_20$ASCAT2016_DJF_mean, city_grid_data_input_20$ASCAT2017_DJF_mean, city_grid_data_input_20$ASCAT2018_DJF_mean,
                                     city_grid_data_input_20$ASCAT2019_DJF_mean, city_grid_data_input_20$ASCAT2020_DJF_mean)
      
      city_grids_ers_s0_10 = cbind(city_grids_10, 
                                   city_grid_data_input_10$ERS1993_DJF_mean, city_grid_data_input_10$ERS1994_DJF_mean, city_grid_data_input_10$ERS1995_DJF_mean, city_grid_data_input_10$ERS1996_DJF_mean,
                                   city_grid_data_input_10$ERS1997_DJF_mean, city_grid_data_input_10$ERS1998_DJF_mean, city_grid_data_input_10$ERS1999_DJF_mean, city_grid_data_input_10$ERS2000_DJF_mean)
      
      city_grids_qscat_s0_10 = cbind(city_grids_10, 
                                     city_grid_data_input_10$QSCAT1999_DJF_mean, city_grid_data_input_10$QSCAT2000_DJF_mean, city_grid_data_input_10$QSCAT2001_DJF_mean, city_grid_data_input_10$QSCAT2002_DJF_mean,
                                     city_grid_data_input_10$QSCAT2003_DJF_mean, city_grid_data_input_10$QSCAT2004_DJF_mean, city_grid_data_input_10$QSCAT2005_DJF_mean, city_grid_data_input_10$QSCAT2006_DJF_mean,
                                     city_grid_data_input_10$QSCAT2007_DJF_mean, city_grid_data_input_10$QSCAT2008_DJF_mean, city_grid_data_input_10$QSCAT2009_DJF_mean)
      
      city_grids_ascat_s0_10 = cbind(city_grids_10, 
                                     city_grid_data_input_10$ASCAT2007_DJF_mean, city_grid_data_input_10$ASCAT2008_DJF_mean, city_grid_data_input_10$ASCAT2009_DJF_mean, city_grid_data_input_10$ASCAT2010_DJF_mean,
                                     city_grid_data_input_10$ASCAT2011_DJF_mean, city_grid_data_input_10$ASCAT2012_DJF_mean, city_grid_data_input_10$ASCAT2013_DJF_mean, city_grid_data_input_10$ASCAT2014_DJF_mean,
                                     city_grid_data_input_10$ASCAT2015_DJF_mean, city_grid_data_input_10$ASCAT2016_DJF_mean, city_grid_data_input_10$ASCAT2017_DJF_mean, city_grid_data_input_10$ASCAT2018_DJF_mean,
                                     city_grid_data_input_10$ASCAT2019_DJF_mean, city_grid_data_input_10$ASCAT2020_DJF_mean)
      
    }   
    
    colnames(city_grids_ers_s0_20) = c("latitude", "longitude", "cityname", "BF_2014_min", 
                                       "ERS_sum_mean_1993","ERS_sum_mean_1994","ERS_sum_mean_1995","ERS_sum_mean_1996",
                                       "ERS_sum_mean_1997","ERS_sum_mean_1998","ERS_sum_mean_1999","ERS_sum_mean_2000")
    colnames(city_grids_qscat_s0_20) = c("latitude", "longitude", "cityname", "BF_2014_min", 
                                         "QSCAT_sum_mean_1999","QSCAT_sum_mean_2000","QSCAT_sum_mean_2001","QSCAT_sum_mean_2002",
                                         "QSCAT_sum_mean_2003","QSCAT_sum_mean_2004","QSCAT_sum_mean_2005","QSCAT_sum_mean_2006",
                                         "QSCAT_sum_mean_2007","QSCAT_sum_mean_2008","QSCAT_sum_mean_2009")
    colnames(city_grids_ascat_s0_20) = c("latitude", "longitude", "cityname", "BF_2014_min", 
                                         "ASCAT_sum_mean_2007","ASCAT_sum_mean_2008","ASCAT_sum_mean_2009","ASCAT_sum_mean_2010",
                                         "ASCAT_sum_mean_2011","ASCAT_sum_mean_2012","ASCAT_sum_mean_2013","ASCAT_sum_mean_2014",
                                         "ASCAT_sum_mean_2015","ASCAT_sum_mean_2016","ASCAT_sum_mean_2017","ASCAT_sum_mean_2018",
                                         "ASCAT_sum_mean_2019","ASCAT_sum_mean_2020")
    
    colnames(city_grids_ers_s0_10) = c("latitude", "longitude", "cityname", "BF_2014_min", 
                                       "ERS_sum_mean_1993","ERS_sum_mean_1994","ERS_sum_mean_1995","ERS_sum_mean_1996",
                                       "ERS_sum_mean_1997","ERS_sum_mean_1998","ERS_sum_mean_1999","ERS_sum_mean_2000")
    colnames(city_grids_qscat_s0_10) = c("latitude", "longitude", "cityname", "BF_2014_min", 
                                         "QSCAT_sum_mean_1999","QSCAT_sum_mean_2000","QSCAT_sum_mean_2001","QSCAT_sum_mean_2002",
                                         "QSCAT_sum_mean_2003","QSCAT_sum_mean_2004","QSCAT_sum_mean_2005","QSCAT_sum_mean_2006",
                                         "QSCAT_sum_mean_2007","QSCAT_sum_mean_2008","QSCAT_sum_mean_2009")
    colnames(city_grids_ascat_s0_10) = c("latitude", "longitude", "cityname", "BF_2014_min", 
                                         "ASCAT_sum_mean_2007","ASCAT_sum_mean_2008","ASCAT_sum_mean_2009","ASCAT_sum_mean_2010",
                                         "ASCAT_sum_mean_2011","ASCAT_sum_mean_2012","ASCAT_sum_mean_2013","ASCAT_sum_mean_2014",
                                         "ASCAT_sum_mean_2015","ASCAT_sum_mean_2016","ASCAT_sum_mean_2017","ASCAT_sum_mean_2018",
                                         "ASCAT_sum_mean_2019","ASCAT_sum_mean_2020")
    
# compute city mean and std. dev. across grid cells    
    
  # 1. ERS with BF min = 20%
    
    df.city_ers_time_series_20 = as.data.frame(array(0,c(5,(2+ers_len))))
    colnames(df.city_ers_time_series_20) = c("city","variable", '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000')
    
    df.city_ers_time_series_20[1,1] = city_name
    df.city_ers_time_series_20[1,2] = 'ers_s0_mean'
    df.city_ers_time_series_20[1,3:(2+ers_len)] = colMeans(city_grids_ers_s0_20[5:(4+ers_len)],na.rm=T)
    
    df.city_ers_time_series_20[2,1] = city_name
    df.city_ers_time_series_20[2,2] = 'ers_s0_stdev'
    for (iyear in 1:ers_len) {
      df.city_ers_time_series_20[2,(2+iyear)] = sqrt(var(city_grids_ers_s0_20[,(4+iyear)],use="complete.obs"))
    }
    # df.city_ers_time_series_20[2,3:(2+ers_len)] = sqrt(var(city_grids_ers_s0_20[5:(4+ers_len)],use="complete.obs"))
    
    df.city_ers_time_series_20[3,1] = city_name
    df.city_ers_time_series_20[3,2] = 'ers_pr_mean'
    df.city_ers_time_series_20[3,3:(2+ers_len)] = 10^(df.city_ers_time_series_20[1,3:(2+ers_len)]/10)  # convert sig0_dB in PR
    
    df.city_ers_time_series_20[4,1] = city_name
    df.city_ers_time_series_20[4,2] = 'ers_pr_mean+1sigma'
    df.city_ers_time_series_20[4,3:(2+ers_len)] = 10^((df.city_ers_time_series_20[1,3:(2+ers_len)] + df.city_ers_time_series_20[2,3:(2+ers_len)])/10)  
    
    df.city_ers_time_series_20[5,1] = city_name
    df.city_ers_time_series_20[5,2] = 'ers_pr_mean-1sigma'
    df.city_ers_time_series_20[5,3:(2+ers_len)] = 10^((df.city_ers_time_series_20[1,3:(2+ers_len)] - df.city_ers_time_series_20[2,3:(2+ers_len)])/10)  
    
    city_ers_pr_linear_fit = lm(as.numeric(df.city_ers_time_series_20[3,3:(2+ers_len)]) ~ ers_years)
    ers_20_pr_slope = city_ers_pr_linear_fit$coefficients[2]
    ers_20_pr_r2 = summary(city_ers_pr_linear_fit)$r.squared
    ers_20_pr_slope_pval = coef(summary(city_ers_pr_linear_fit))[2,4]
    
    df.all_city_pr_20_slopes[icity,1] = city_name 
    df.all_city_pr_20_slopes[icity,2] = ers_20_pr_slope 
    df.all_city_pr_20_slopes[icity,3] = ers_20_pr_r2 
    df.all_city_pr_20_slopes[icity,4] = ers_20_pr_slope_pval 
    
  # 2. ERS with BF min = 10%
    
    df.city_ers_time_series_10 = as.data.frame(array(0,c(5,(2+ers_len))))
    colnames(df.city_ers_time_series_10) = c("city","variable", '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000')
    
    df.city_ers_time_series_10[1,1] = city_name
    df.city_ers_time_series_10[1,2] = 'ers_s0_mean'
    df.city_ers_time_series_10[1,3:(2+ers_len)] = colMeans(city_grids_ers_s0_10[5:(4+ers_len)],na.rm=T)
    
    df.city_ers_time_series_10[2,1] = city_name
    df.city_ers_time_series_10[2,2] = 'ers_s0_stdev'
    for (iyear in 1:ers_len) {
      df.city_ers_time_series_10[2,(2+iyear)] = sqrt(var(city_grids_ers_s0_10[,(4+iyear)],use="complete.obs"))
    }
    #     df.city_ers_time_series_10[2,3:(2+ers_len)] = sqrt(var(city_grids_ers_s0_10[5:(4+ers_len)],use="complete.obs"))
    
    df.city_ers_time_series_10[3,1] = city_name
    df.city_ers_time_series_10[3,2] = 'ers_pr_mean'
    df.city_ers_time_series_10[3,3:(2+ers_len)] = 10^(df.city_ers_time_series_10[1,3:(2+ers_len)]/10)  # convert sig0_dB in PR
    
    df.city_ers_time_series_10[4,1] = city_name
    df.city_ers_time_series_10[4,2] = 'ers_pr_mean+1sigma'
    df.city_ers_time_series_10[4,3:(2+ers_len)] = 10^((df.city_ers_time_series_10[1,3:(2+ers_len)] + df.city_ers_time_series_10[2,3:(2+ers_len)])/10)  
    
    df.city_ers_time_series_10[5,1] = city_name
    df.city_ers_time_series_10[5,2] = 'ers_pr_mean-1sigma'
    df.city_ers_time_series_10[5,3:(2+ers_len)] = 10^((df.city_ers_time_series_10[1,3:(2+ers_len)] - df.city_ers_time_series_10[2,3:(2+ers_len)])/10)  
    
    city_ers_pr_linear_fit = lm(as.numeric(df.city_ers_time_series_10[3,3:(2+ers_len)]) ~ ers_years)
    ers_10_pr_slope = city_ers_pr_linear_fit$coefficients[2]
    ers_10_pr_r2 = summary(city_ers_pr_linear_fit)$r.squared
    ers_10_pr_slope_pval = coef(summary(city_ers_pr_linear_fit))[2,4]
    
    df.all_city_pr_10_slopes[icity,1] = city_name 
    df.all_city_pr_10_slopes[icity,2] = ers_10_pr_slope 
    df.all_city_pr_10_slopes[icity,3] = ers_10_pr_r2 
    df.all_city_pr_10_slopes[icity,4] = ers_10_pr_slope_pval 
    
  # 3. QSCAT with BF min = 20%
    
    df.city_qscat_time_series_20 = as.data.frame(array(0,c(5,(2+qscat_len))))
    colnames(df.city_qscat_time_series_20) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
    
    df.city_qscat_time_series_20[1,1] = city_name
    df.city_qscat_time_series_20[1,2] = 'qscat_s0_mean'
    df.city_qscat_time_series_20[1,3:(2+qscat_len)] = colMeans(city_grids_qscat_s0_20[5:(4+qscat_len)],na.rm=T)
    
    df.city_qscat_time_series_20[2,1] = city_name
    df.city_qscat_time_series_20[2,2] = 'qscat_s0_stdev'
    for (iyear in 1:qscat_len) {
      df.city_qscat_time_series_20[2,(2+iyear)] = sqrt(var(city_grids_qscat_s0_20[,(4+iyear)],use="complete.obs"))
    }
    #     df.city_qscat_time_series_20[2,3:(2+qscat_len)] = sqrt(var(city_grids_qscat_s0_20[5:(4+qscat_len)],use="complete.obs"))
    
    df.city_qscat_time_series_20[3,1] = city_name
    df.city_qscat_time_series_20[3,2] = 'qscat_pr_mean'
    df.city_qscat_time_series_20[3,3:(2+qscat_len)] = 10^(df.city_qscat_time_series_20[1,3:(2+qscat_len)]/10)  # convert sig0_dB in PR
    
    df.city_qscat_time_series_20[4,1] = city_name
    df.city_qscat_time_series_20[4,2] = 'qscat_pr_mean+1sigma'
    df.city_qscat_time_series_20[4,3:(2+qscat_len)] = 10^((df.city_qscat_time_series_20[1,3:(2+qscat_len)] + df.city_qscat_time_series_20[2,3:(2+qscat_len)])/10)  
    
    df.city_qscat_time_series_20[5,1] = city_name
    df.city_qscat_time_series_20[5,2] = 'qscat_pr_mean-1sigma'
    df.city_qscat_time_series_20[5,3:(2+qscat_len)] = 10^((df.city_qscat_time_series_20[1,3:(2+qscat_len)] - df.city_qscat_time_series_20[2,3:(2+qscat_len)])/10)  
    
    city_qscat_pr_linear_fit = lm(as.numeric(df.city_qscat_time_series_20[3,3:(2+qscat_len)]) ~ qscat_years)
    qscat_20_pr_slope = city_qscat_pr_linear_fit$coefficients[2]
    qscat_20_pr_r2 = summary(city_qscat_pr_linear_fit)$r.squared
    qscat_20_pr_slope_pval = coef(summary(city_qscat_pr_linear_fit))[2,4]
    
    df.all_city_pr_20_slopes[icity,5] = qscat_20_pr_slope 
    df.all_city_pr_20_slopes[icity,6] = qscat_20_pr_r2 
    df.all_city_pr_20_slopes[icity,7] = qscat_20_pr_slope_pval 
    
  # 4. QSCAT with BF min = 10%
    
    df.city_qscat_time_series_10 = as.data.frame(array(0,c(5,(2+qscat_len))))
    colnames(df.city_qscat_time_series_10) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
    
    df.city_qscat_time_series_10[1,1] = city_name
    df.city_qscat_time_series_10[1,2] = 'qscat_s0_mean'
    df.city_qscat_time_series_10[1,3:(2+qscat_len)] = colMeans(city_grids_qscat_s0_10[5:(4+qscat_len)],na.rm=T)
    
    df.city_qscat_time_series_10[2,1] = city_name
    df.city_qscat_time_series_10[2,2] = 'qscat_s0_stdev'
    for (iyear in 1:qscat_len) {
      df.city_qscat_time_series_10[2,(2+iyear)] = sqrt(var(city_grids_qscat_s0_10[,(4+iyear)],use="complete.obs"))
    }
    #     df.city_qscat_time_series_10[2,3:(2+qscat_len)] = sqrt(var(city_grids_qscat_s0_10[5:(4+qscat_len)],use="complete.obs"))
    
    df.city_qscat_time_series_10[3,1] = city_name
    df.city_qscat_time_series_10[3,2] = 'qscat_pr_mean'
    df.city_qscat_time_series_10[3,3:(2+qscat_len)] = 10^(df.city_qscat_time_series_10[1,3:(2+qscat_len)]/10)  # convert sig0_dB in PR
    
    df.city_qscat_time_series_10[4,1] = city_name
    df.city_qscat_time_series_10[4,2] = 'qscat_pr_mean+1sigma'
    df.city_qscat_time_series_10[4,3:(2+qscat_len)] = 10^((df.city_qscat_time_series_10[1,3:(2+qscat_len)] + df.city_qscat_time_series_10[2,3:(2+qscat_len)])/10)  
    
    df.city_qscat_time_series_10[5,1] = city_name
    df.city_qscat_time_series_10[5,2] = 'qscat_pr_mean-1sigma'
    df.city_qscat_time_series_10[5,3:(2+qscat_len)] = 10^((df.city_qscat_time_series_10[1,3:(2+qscat_len)] - df.city_qscat_time_series_10[2,3:(2+qscat_len)])/10)  
    
    city_qscat_pr_linear_fit = lm(as.numeric(df.city_qscat_time_series_10[3,3:(2+qscat_len)]) ~ qscat_years)
    qscat_10_pr_slope = city_qscat_pr_linear_fit$coefficients[2]
    qscat_10_pr_r2 = summary(city_qscat_pr_linear_fit)$r.squared
    qscat_10_pr_slope_pval = coef(summary(city_qscat_pr_linear_fit))[2,4]
    
    df.all_city_pr_10_slopes[icity,5] = qscat_10_pr_slope 
    df.all_city_pr_10_slopes[icity,6] = qscat_10_pr_r2 
    df.all_city_pr_10_slopes[icity,7] = qscat_10_pr_slope_pval 
    
  # 5. ASCAT with BF min = 20%
    
    df.city_ascat_time_series_20 = as.data.frame(array(0,c(5,(2+ascat_len))))
    colnames(df.city_ascat_time_series_20) = c("city","variable",'2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
    
    df.city_ascat_time_series_20[1,1] = city_name
    df.city_ascat_time_series_20[1,2] = 'ascat_s0_mean'
    df.city_ascat_time_series_20[1,3:(2+ascat_len)] = colMeans(city_grids_ascat_s0_20[5:(4+ascat_len)],na.rm=T)
    
    df.city_ascat_time_series_20[2,1] = city_name
    df.city_ascat_time_series_20[2,2] = 'ascat_s0_stdev'
    for (iyear in 1:ascat_len) {
      df.city_ascat_time_series_20[2,(2+iyear)] = sqrt(var(city_grids_ascat_s0_20[,(4+iyear)],use="complete.obs"))
    }
    #     df.city_ascat_time_series_20[2,3:(2+ascat_len)] = sqrt(var(city_grids_ascat_s0_20[5:(4+ascat_len)],use="complete.obs"))
    
    df.city_ascat_time_series_20[3,1] = city_name
    df.city_ascat_time_series_20[3,2] = 'ascat_pr_mean'
    df.city_ascat_time_series_20[3,3:(2+ascat_len)] = 10^(df.city_ascat_time_series_20[1,3:(2+ascat_len)]/10)  # convert sig0_dB in PR
    
    df.city_ascat_time_series_20[4,1] = city_name
    df.city_ascat_time_series_20[4,2] = 'ascat_pr_mean+1sigma'
    df.city_ascat_time_series_20[4,3:(2+ascat_len)] = 10^((df.city_ascat_time_series_20[1,3:(2+ascat_len)] + df.city_ascat_time_series_20[2,3:(2+ascat_len)])/10)  
    
    df.city_ascat_time_series_20[5,1] = city_name
    df.city_ascat_time_series_20[5,2] = 'ascat_pr_mean-1sigma'
    df.city_ascat_time_series_20[5,3:(2+ascat_len)] = 10^((df.city_ascat_time_series_20[1,3:(2+ascat_len)] - df.city_ascat_time_series_20[2,3:(2+ascat_len)])/10)  
    
    city_ascat_pr_linear_fit = lm(as.numeric(df.city_ascat_time_series_20[3,3:(2+ascat_len)]) ~ ascat_years)
    ascat_20_pr_slope = city_ascat_pr_linear_fit$coefficients[2]
    ascat_20_pr_r2 = summary(city_ascat_pr_linear_fit)$r.squared
    ascat_20_pr_slope_pval = coef(summary(city_ascat_pr_linear_fit))[2,4]
    
    df.all_city_pr_20_slopes[icity,8] = ascat_20_pr_slope 
    df.all_city_pr_20_slopes[icity,9] = ascat_20_pr_r2 
    df.all_city_pr_20_slopes[icity,10] = ascat_20_pr_slope_pval 
    
  # 6. ASCAT with BF min = 10%
    
    df.city_ascat_time_series_10 = as.data.frame(array(0,c(5,(2+ascat_len))))
    colnames(df.city_ascat_time_series_10) = c("city","variable",'2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
    
    df.city_ascat_time_series_10[1,1] = city_name
    df.city_ascat_time_series_10[1,2] = 'ascat_s0_mean'
    df.city_ascat_time_series_10[1,3:(2+ascat_len)] = colMeans(city_grids_ascat_s0_10[5:(4+ascat_len)],na.rm=T)
    
    df.city_ascat_time_series_10[2,1] = city_name
    df.city_ascat_time_series_10[2,2] = 'ascat_s0_stdev'
    for (iyear in 1:ascat_len) {
      df.city_ascat_time_series_10[2,(2+iyear)] = sqrt(var(city_grids_ascat_s0_10[,(4+iyear)],use="complete.obs"))
    }
    #         df.city_ascat_time_series_10[2,3:(2+ascat_len)] = sqrt(var(city_grids_ascat_s0_10[5:(4+ascat_len)],use="complete.obs"))
    
    df.city_ascat_time_series_10[3,1] = city_name
    df.city_ascat_time_series_10[3,2] = 'ascat_pr_mean'
    df.city_ascat_time_series_10[3,3:(2+ascat_len)] = 10^(df.city_ascat_time_series_10[1,3:(2+ascat_len)]/10)  # convert sig0_dB in PR
    
    df.city_ascat_time_series_10[4,1] = city_name
    df.city_ascat_time_series_10[4,2] = 'ascat_pr_mean+1sigma'
    df.city_ascat_time_series_10[4,3:(2+ascat_len)] = 10^((df.city_ascat_time_series_10[1,3:(2+ascat_len)] + df.city_ascat_time_series_10[2,3:(2+ascat_len)])/10)  
    
    df.city_ascat_time_series_10[5,1] = city_name
    df.city_ascat_time_series_10[5,2] = 'ascat_pr_mean-1sigma'
    df.city_ascat_time_series_10[5,3:(2+ascat_len)] = 10^((df.city_ascat_time_series_10[1,3:(2+ascat_len)] - df.city_ascat_time_series_10[2,3:(2+ascat_len)])/10)  
    
    city_ascat_pr_linear_fit = lm(as.numeric(df.city_ascat_time_series_10[3,3:(2+ascat_len)]) ~ ascat_years)
    ascat_10_pr_slope = city_ascat_pr_linear_fit$coefficients[2]
    ascat_10_pr_r2 = summary(city_ascat_pr_linear_fit)$r.squared
    ascat_10_pr_slope_pval = coef(summary(city_ascat_pr_linear_fit))[2,4]
    
    df.all_city_pr_10_slopes[icity,8] = ascat_10_pr_slope 
    df.all_city_pr_10_slopes[icity,9] = ascat_10_pr_r2 
    df.all_city_pr_10_slopes[icity,10] = ascat_10_pr_slope_pval 
    
# Compute qscat offset and make arrays of those values

    if (max(city_grid_data_input_20$latitude, na.rm=T) >= 0) { # northern hemisphere, so 'summer' = JAS
      
      kuband.offset_pr_20 = (sum(df.city_ers_time_series_20[3,c(9,10)], na.rm = T) + sum(df.city_ascat_time_series_20[3,c(3,4,5)], na.rm = T)  - 
                            sum(df.city_qscat_time_series_20[3,c(3,4,11,12,13)], na.rm = T)) / 5
      kuband.offset_pr_10 = (sum(df.city_ers_time_series_10[3,c(9,10)], na.rm = T) + sum(df.city_ascat_time_series_10[3,c(3,4,5)], na.rm = T)  - 
                               sum(df.city_qscat_time_series_10[3,c(3,4,11,12,13)], na.rm = T)) / 5
      
      df.city_qscat_offset_time_series_20 = as.data.frame(array(NA,c(5,(2+qscat_len))))
      colnames(df.city_qscat_offset_time_series_20) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
      df.city_qscat_offset_time_series_20[,1:2] = df.city_qscat_time_series_20[,1:2] 
      df.city_qscat_offset_time_series_20[3:5,3:(2+qscat_len)] = df.city_qscat_time_series_20[3:5,3:(2+qscat_len)] + kuband.offset_pr_20
 
      df.city_qscat_offset_time_series_10 = as.data.frame(array(NA,c(5,(2+qscat_len))))
      colnames(df.city_qscat_offset_time_series_10) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
      df.city_qscat_offset_time_series_10[,1:2] = df.city_qscat_time_series_10[,1:2] 
      df.city_qscat_offset_time_series_10[3:5,3:(2+qscat_len)] = df.city_qscat_time_series_10[3:5,3:(2+qscat_len)] + kuband.offset_pr_10
      
      
    } else { # southern hemisphere, so 'summer' = JFM, and Qscat has no summer value for 1999.
      
      kuband.offset_pr_20 = (sum(df.city_ers_time_series_20[3,c(10)], na.rm = T) + sum(df.city_ascat_time_series_20[3,c(3,4,5)], na.rm = T)  - 
                               sum(df.city_qscat_time_series_20[3,c(4,11,12,13)], na.rm = T)) / 4
      kuband.offset_pr_10 = (sum(df.city_ers_time_series_10[3,c(10)], na.rm = T) + sum(df.city_ascat_time_series_10[3,c(3,4,5)], na.rm = T)  - 
                               sum(df.city_qscat_time_series_10[3,c(4,11,12,13)], na.rm = T)) / 4
      
      df.city_qscat_offset_time_series_20 = as.data.frame(array(NA,c(5,(2+qscat_len))))
      colnames(df.city_qscat_offset_time_series_20) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
      df.city_qscat_offset_time_series_20[,1:2] = df.city_qscat_time_series_20[,1:2] 
      df.city_qscat_offset_time_series_20[3:5,3:(2+qscat_len)] = df.city_qscat_time_series_20[3:5,3:(2+qscat_len)] + kuband.offset_pr_20
      
      df.city_qscat_offset_time_series_10 = as.data.frame(array(NA,c(5,(2+qscat_len))))
      colnames(df.city_qscat_offset_time_series_10) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
      df.city_qscat_offset_time_series_10[,1:2] = df.city_qscat_time_series_10[,1:2] 
      df.city_qscat_offset_time_series_10[3:5,3:(2+qscat_len)] = df.city_qscat_time_series_10[3:5,3:(2+qscat_len)] + kuband.offset_pr_10
      
    }
    
# make time series plot for city
    
    pdf(paste(output_dir,city_name,"_","backscatter_pr_timeseries",'.pdf',sep = ''))
    par(mfrow=c(2,1),mar=c(4,4,4,4))
    
    # make plots for backscatter time series in PR for BF_2014 minima of 20% and 10%
    
    xrange = c(1993,2020)
    yrange = c(0,0.8)
  
    # plot for 20% minimum
    
    main_title =paste(city_name,", BltFrac >= 20%" ,sep="")
    
    plot(ers_years,df.city_ers_time_series_20[3,3:(2+ers_len)],
         xlim = xrange,
         ylim = yrange,
         xlab = "year",
         ylab = "backscatter PR",
         col = "red",pch = 19,
         #           main = paste(city_name,", BltFrac = ",bltfrac_intervals[ibltfrac]," - ",bltfrac_intervals[ibltfrac+1],", Ngrid=",df.city_bltfrac_backscatter_s0_time_series[ibltfrac,3],sep="")
         main = main_title
    )
    arrows(as.numeric(ers_years), as.numeric(df.city_ers_time_series_20[4,3:(2+ers_len)]), 
           as.numeric(ers_years), as.numeric(df.city_ers_time_series_20[5,3:(2+ers_len)]), 
           length=0.05, col="red", angle=90, code=3)
#    text(1997,0.025, paste(as.character(formatC(df.all_city_pr_20_slopes[icity,2],digits=3,format='f')), sep=""), col="red",cex=1)
    
    points(qscat_years,df.city_qscat_time_series_20[3,3:(2+qscat_len)], col = "blue",pch = 19)
    arrows(as.numeric(qscat_years), as.numeric(df.city_qscat_time_series_20[4,3:(2+qscat_len)]), 
           as.numeric(qscat_years), as.numeric(df.city_qscat_time_series_20[5,3:(2+qscat_len)]), 
           length=0.05, col="blue", angle=90, code=3)
#    text(2004,0.025, paste(as.character(formatC(df.all_city_pr_20_slopes[icity,5],digits=3,format='f')), sep=""), col="blue",cex=1)
    
    points(ascat_years,df.city_ascat_time_series_20[3,3:(2+ascat_len)], col = "goldenrod",pch = 19)
    arrows(as.numeric(ascat_years), as.numeric(df.city_ascat_time_series_20[4,3:(2+ascat_len)]), 
           as.numeric(ascat_years), as.numeric(df.city_ascat_time_series_20[5,3:(2+ascat_len)]), 
           length=0.05, col="goldenrod", angle=90, code=3)
#    text(2015,0.025, paste(as.character(formatC(df.all_city_pr_20_slopes[icity,8],digits=3,format='f')), sep=""), col="goldenrod",cex=1)
    
    points(qscat_years,df.city_qscat_offset_time_series_20[3,3:(2+qscat_len)], col = "cyan4",pch = 19)
    arrows(as.numeric(qscat_years), as.numeric(df.city_qscat_offset_time_series_20[4,3:(2+qscat_len)]), 
           as.numeric(qscat_years), as.numeric(df.city_qscat_offset_time_series_20[5,3:(2+qscat_len)]), 
           length=0.05, col="cyan4", angle=90, code=3)

    legend("topleft",c("ERS","QSCAT","QSCAT + offset","ASCAT"),pch=c(19,7,19,19),col=c("red","blue","cyan4","goldenrod"),cex=0.55)
    
    text(2004,0.75,paste(city_name,", BF_2014 >= 0.20" ,sep=""), col="black",cex=1)
    
    
    # plot for 20% minimum
    
    main_title =paste(city_name,", BltFrac >= 10%" ,sep="")
    
    plot(ers_years,df.city_ers_time_series_10[3,3:(2+ers_len)],
         xlim = xrange,
         ylim = yrange,
         xlab = "year",
         ylab = "backscatter PR",
         col = "red",pch = 19,
         #           main = paste(city_name,", BltFrac = ",bltfrac_intervals[ibltfrac]," - ",bltfrac_intervals[ibltfrac+1],", Ngrid=",df.city_bltfrac_backscatter_s0_time_series[ibltfrac,3],sep="")
         main = main_title
    )
    arrows(as.numeric(ers_years), as.numeric(df.city_ers_time_series_10[4,3:(2+ers_len)]), 
           as.numeric(ers_years), as.numeric(df.city_ers_time_series_10[5,3:(2+ers_len)]), 
           length=0.05, col="red", angle=90, code=3)
#    text(1997,0.025, paste(as.character(formatC(df.all_city_pr_10_slopes[icity,2],digits=3,format='f')), sep=""), col="red",cex=1)
    
    points(qscat_years,df.city_qscat_time_series_10[3,3:(2+qscat_len)], col = "blue",pch = 19)
    arrows(as.numeric(qscat_years), as.numeric(df.city_qscat_time_series_10[4,3:(2+qscat_len)]), 
           as.numeric(qscat_years), as.numeric(df.city_qscat_time_series_10[5,3:(2+qscat_len)]), 
           length=0.05, col="blue", angle=90, code=3)
#    text(2004,0.025, paste(as.character(formatC(df.all_city_pr_10_slopes[icity,5],digits=3,format='f')), sep=""), col="blue",cex=1)
    
    points(ascat_years,df.city_ascat_time_series_10[3,3:(2+ascat_len)], col = "goldenrod",pch = 19)
    arrows(as.numeric(ascat_years), as.numeric(df.city_ascat_time_series_10[4,3:(2+ascat_len)]), 
           as.numeric(ascat_years), as.numeric(df.city_ascat_time_series_10[5,3:(2+ascat_len)]), 
           length=0.05, col="goldenrod", angle=90, code=3)
#    text(2015,0.025, paste(as.character(formatC(df.all_city_pr_10_slopes[icity,8],digits=3,format='f')), sep=""), col="goldenrod",cex=1)
    
    points(qscat_years,df.city_qscat_offset_time_series_10[3,3:(2+qscat_len)], col = "cyan4",pch = 19)
    arrows(as.numeric(qscat_years), as.numeric(df.city_qscat_offset_time_series_10[4,3:(2+qscat_len)]), 
           as.numeric(qscat_years), as.numeric(df.city_qscat_offset_time_series_10[5,3:(2+qscat_len)]), 
           length=0.05, col="cyan4", angle=90, code=3)
    
    legend("topleft",c("ERS","QSCAT","QSCAT + offset","ASCAT"),pch=c(19,7,19,19),col=c("red","blue","cyan4","goldenrod"),cex=0.55)
    
    text(2004,0.75,paste(city_name,", BF_2014 >= 0.10" ,sep=""), col="black",cex=1)
    
    
    
    dev.off()
    
  }  # end 'if city file exists'
  
  
}  # end loop through cities

outfile.name = paste(output_dir,"all_city_backscatter_pr_20_slopes",".csv",sep = "")
write.csv(df.all_city_pr_20_slopes,file = outfile.name,row.names = F)

outfile.name = paste(output_dir,"all_city_backscatter_pr_10_slopes",".csv",sep = "")
write.csv(df.all_city_pr_10_slopes,file = outfile.name,row.names = F)


# July 2021
#  code to plot urban time series (and invariant time series?) for data paper

wd = getwd()

cities_tf = 0  # = 1 if plotting cities, = 0 if plotting invariant sites

if (cities_tf == 1) {
  
  input_dir = paste(wd,"/city_grids_wat_bldg_gfcc_05_04_2021/",sep="")
  
  output_dir = paste(wd,"/city_grids_wat_bldg_gfcc_05_04_2021_output_files_data_paper/",sep="")
  # city_output_dir = paste(wd,"/city_grids_wat_bldg_gfcc_05_04_2021_output_files_by_city/",sep="")
  
  # 12 cities to plot for data paper
  city_list_filename = paste(wd,"/","city_list_12_lat_lon.csv", sep="")      # "Tokyo" , "Busan", "Urumqi"  ,"Guangzhou" , "Mumbai",  "Tehran" ,  "MexicoCity", "LasVegas", " Munich", "Moscow" , " Lagos", "Aleppo"  
  
  city_list = read.csv(file=city_list_filename,header = TRUE, stringsAsFactors = FALSE)
  # HEADER: city_id,	City,	Longitude,	Latitude,	Region_10,	Region_10_id,	validation_region,	Region_13,	Country   
  #  Note: region_10 follows Mahtta et al. 2019 ERL; 
  #         Validation_region: the three regions NorthAmerica (USA), China, and Europe were covered in the building height, footprint, and volume analysis of Li et al. (2020, RSE) and used for ground validation
  num_cities = dim(city_list)[1]
  
} else {
  
  input_dir = paste(wd,"/invariant_grids_07_09_2021/",sep="")
  output_dir = paste(wd,"/invariant_grids_07_09_2021_output_files/",sep="")
  
  # read in list of invariant sites to be analyzed -- restrict tropical forest sites
  
  # city_list_filename = paste(wd,"/","city_list_478_lat_lon.csv", sep="")                  #  list of cities >1M
  
  invariant_list = c("Amazon2","DemRepCongo","PapuaNewGuinea","Amazon1")         
  num_cities = length(invariant_list)
  
}

ers_years = 1993:2000
qscat_years = 1999:2009
ascat_years = 2007:2020
ers_len = length(ers_years)
qscat_len = length(qscat_years)
ascat_len = length(ascat_years)

df.all_invariant_pr_slopes = as.data.frame(array(0,c(num_cities,10)))

colnames(df.all_invariant_pr_slopes) = c('city', 'ers_pr_slope', 'ers_pr_r2', 'ers_pr_slope_pval', 'qscat_pr_slope', 'qscat_pr_r2', 'qscat_pr_slope_pval', 'ascat_pr_slope', 'ascat_pr_r2', 'ascat_pr_slope_pval')

for (icity in 1:num_cities) {      
  
  if (cities_tf == 1) {
    city_name = city_list[icity,2]
  } else {
    city_name = invariant_list[icity]
  }
  
  # read in city data (5-km grid) of GHSL built fraction and population (1975, 1990, 2000, 2015) and ERS, QSCAT, and ASCAT summer (JAS) mean backscatter in dB
  
  invariant_grid_filename = paste(input_dir,city_name,'_invariant_grid','.csv', sep = '')
  
  if (file.exists(invariant_grid_filename)) {
    
   invariant_grid_data_input = read.csv(file = invariant_grid_filename, header = TRUE, stringsAsFactors = FALSE)    
    
    # Header: 1-3:   latitude, longitude, cityname, 
    #         4-19: ERS1993_JFM_mean, ERS1993_JFM_std, ERS1994_JFM_mean, ERS1994_JFM_std, ERS1995_JFM_mean, ERS1995_JFM_std, ERS1996_JFM_mean, ERS1996_JFM_std, 
    #                ERS1997_JFM_mean, ERS1997_JFM_std, ERS1998_JFM_mean, ERS1998_JFM_std, ERS1999_JFM_mean, ERS1999_JFM_std, ERS2000_JFM_mean, ERS2000_JFM_std, 
    #         20-41: QSCAT1999_JFM_mean, QSCAT1999_JFM_std, QSCAT2000_JFM_mean, QSCAT2000_JFM_std, QSCAT2001_JFM_mean, QSCAT2001_JFM_std, QSCAT2002_JFM_mean, QSCAT2002_JFM_std, 
    #                QSCAT2003_JFM_mean, QSCAT2003_JFM_std, QSCAT2004_JFM_mean, QSCAT2004_JFM_std, QSCAT2005_JFM_mean, QSCAT2005_JFM_std, QSCAT2006_JFM_mean, QSCAT2006_JFM_std, 
    #                QSCAT2007_JFM_mean, QSCAT2007_JFM_std, QSCAT2008_JFM_mean, QSCAT2008_JFM_std, QSCAT2009_JFM_mean, QSCAT2009_JFM_std, 
    #         42-69: ASCAT2007_JFM_mean, ASCAT2007_JFM_std, ASCAT2008_JFM_mean, ASCAT2008_JFM_std, ASCAT2009_JFM_mean, ASCAT2009_JFM_std, ASCAT2010_JFM_mean, ASCAT2010_JFM_std, 
    #                ASCAT2011_JFM_mean, ASCAT2011_JFM_std, ASCAT2012_JFM_mean, ASCAT2012_JFM_std, ASCAT2013_JFM_mean, ASCAT2013_JFM_std, ASCAT2014_JFM_mean, ASCAT2014_JFM_std. 
    #                ASCAT2015_JFM_mean, ASCAT2015_JFM_std, ASCAT2016_JFM_mean, ASCAT2016_JFM_std, 
    #                ASCAT2019_JFM_mean, ASCAT2019_JFM_std, ASCAT2020_JFM_mean, ASCAT2020_JFM_std,
    #         70: Water
    
    # NOTE, sigma-0 values are -9999 if grid cell has no data in Tom's analysis on GEE
    #       in the one city analyzed so far (29 April 2020) where this happens (Shanghai), for most -9999 grid cells, value is -9999 for ers, qscat, & ascat mean summer sig0 for all years; assumed to be open water
    #       however, for at least one grid cell, only ers sig0 values were -9999; looking at historical images on google earth, it seems this coastal grid cell was water in the 1990s, but filled in c.2000
    #       water is grid cell precent water (0 to 100) based on ???? land cover mask on GEE; this is used with a threshold value (just below) to mask non-land grid cells
    
    water.threshold = 50  # threshold of grid cell water percentage to flag as 'water' and remove backscatter data from processing
    
    invariant_grid_data_input = subset(invariant_grid_data_input,invariant_grid_data_input$Water < water.threshold)
    
    invariant_grid_data_input[invariant_grid_data_input == -9999] <- NA

    num_grids = dim(invariant_grid_data_input)[1]

    invariant_grids = invariant_grid_data_input[,c(1,2,3)]  # lat, lon, name
# build arrays of summer season backscatter for each sensor for each BF_2014 min values
    
    if (max(invariant_grid_data_input$latitude, na.rm=T) >= 0) { # northern hemisphere, so 'summer' = JAS
      
      invariant_grids_ers_s0 = cbind(invariant_grids,  
                                   invariant_grid_data_input$ERS1993_JAS_mean, invariant_grid_data_input$ERS1994_JAS_mean, invariant_grid_data_input$ERS1995_JAS_mean, invariant_grid_data_input$ERS1996_JAS_mean,
                                   invariant_grid_data_input$ERS1997_JAS_mean, invariant_grid_data_input$ERS1998_JAS_mean, invariant_grid_data_input$ERS1999_JAS_mean, invariant_grid_data_input$ERS2000_JAS_mean)
 
      invariant_grid_qscat_s0 = cbind(invariant_grids, 
                                   invariant_grid_data_input$QSCAT1999_JAS_mean, invariant_grid_data_input$QSCAT2000_JAS_mean, invariant_grid_data_input$QSCAT2001_JAS_mean, invariant_grid_data_input$QSCAT2002_JAS_mean,
                                   invariant_grid_data_input$QSCAT2003_JAS_mean, invariant_grid_data_input$QSCAT2004_JAS_mean, invariant_grid_data_input$QSCAT2005_JAS_mean, invariant_grid_data_input$QSCAT2006_JAS_mean,
                                   invariant_grid_data_input$QSCAT2007_JAS_mean, invariant_grid_data_input$QSCAT2008_JAS_mean, invariant_grid_data_input$QSCAT2009_JAS_mean)
      
      invariant_grid_ascat_s0 = cbind(invariant_grids, 
                                     invariant_grid_data_input$ASCAT2007_JAS_mean, invariant_grid_data_input$ASCAT2008_JAS_mean, invariant_grid_data_input$ASCAT2009_JAS_mean, invariant_grid_data_input$ASCAT2010_JAS_mean,
                                     invariant_grid_data_input$ASCAT2011_JAS_mean, invariant_grid_data_input$ASCAT2012_JAS_mean, invariant_grid_data_input$ASCAT2013_JAS_mean, invariant_grid_data_input$ASCAT2014_JAS_mean,
                                     invariant_grid_data_input$ASCAT2015_JAS_mean, invariant_grid_data_input$ASCAT2016_JAS_mean, invariant_grid_data_input$ASCAT2017_JAS_mean, invariant_grid_data_input$ASCAT2018_JAS_mean,
                                     invariant_grid_data_input$ASCAT2019_JAS_mean, invariant_grid_data_input$ASCAT2020_JAS_mean)
      
    } else {          #  southern hemisphere, so 'summer' = JFM
      
      invariant_grid_ers_s0 = cbind(invariant_grids, 
                                   invariant_grid_data_input$ERS1993_JFM_mean, invariant_grid_data_input$ERS1994_JFM_mean, invariant_grid_data_input$ERS1995_JFM_mean, invariant_grid_data_input$ERS1996_JFM_mean,
                                   invariant_grid_data_input$ERS1997_JFM_mean, invariant_grid_data_input$ERS1998_JFM_mean, invariant_grid_data_input$ERS1999_JFM_mean, invariant_grid_data_input$ERS2000_JFM_mean)
      
      invariant_grid_qscat_s0 = cbind(invariant_grids, 
                                     invariant_grid_data_input$QSCAT1999_JFM_mean, invariant_grid_data_input$QSCAT2000_JFM_mean, invariant_grid_data_input$QSCAT2001_JFM_mean, invariant_grid_data_input$QSCAT2002_JFM_mean,
                                     invariant_grid_data_input$QSCAT2003_JFM_mean, invariant_grid_data_input$QSCAT2004_JFM_mean, invariant_grid_data_input$QSCAT2005_JFM_mean, invariant_grid_data_input$QSCAT2006_JFM_mean,
                                     invariant_grid_data_input$QSCAT2007_JFM_mean, invariant_grid_data_input$QSCAT2008_JFM_mean, invariant_grid_data_input$QSCAT2009_JFM_mean)
      
      invariant_grid_ascat_s0 = cbind(invariant_grids, 
                                     invariant_grid_data_input$ASCAT2007_JFM_mean, invariant_grid_data_input$ASCAT2008_JFM_mean, invariant_grid_data_input$ASCAT2009_JFM_mean, invariant_grid_data_input$ASCAT2010_JFM_mean,
                                     invariant_grid_data_input$ASCAT2011_JFM_mean, invariant_grid_data_input$ASCAT2012_JFM_mean, invariant_grid_data_input$ASCAT2013_JFM_mean, invariant_grid_data_input$ASCAT2014_JFM_mean,
                                     invariant_grid_data_input$ASCAT2015_JFM_mean, invariant_grid_data_input$ASCAT2016_JFM_mean, invariant_grid_data_input$ASCAT2017_JFM_mean, invariant_grid_data_input$ASCAT2018_JFM_mean,
                                     invariant_grid_data_input$ASCAT2019_JFM_mean, invariant_grid_data_input$ASCAT2020_JFM_mean)
      
    }   
    
    colnames(invariant_grid_ers_s0) = c("latitude", "longitude", "cityname", 
                                       "ERS_sum_mean_1993","ERS_sum_mean_1994","ERS_sum_mean_1995","ERS_sum_mean_1996",
                                       "ERS_sum_mean_1997","ERS_sum_mean_1998","ERS_sum_mean_1999","ERS_sum_mean_2000")
    colnames(invariant_grid_qscat_s0) = c("latitude", "longitude", "cityname", 
                                         "QSCAT_sum_mean_1999","QSCAT_sum_mean_2000","QSCAT_sum_mean_2001","QSCAT_sum_mean_2002",
                                         "QSCAT_sum_mean_2003","QSCAT_sum_mean_2004","QSCAT_sum_mean_2005","QSCAT_sum_mean_2006",
                                         "QSCAT_sum_mean_2007","QSCAT_sum_mean_2008","QSCAT_sum_mean_2009")
    colnames(invariant_grid_ascat_s0) = c("latitude", "longitude", "cityname", 
                                         "ASCAT_sum_mean_2007","ASCAT_sum_mean_2008","ASCAT_sum_mean_2009","ASCAT_sum_mean_2010",
                                         "ASCAT_sum_mean_2011","ASCAT_sum_mean_2012","ASCAT_sum_mean_2013","ASCAT_sum_mean_2014",
                                         "ASCAT_sum_mean_2015","ASCAT_sum_mean_2016","ASCAT_sum_mean_2017","ASCAT_sum_mean_2018",
                                         "ASCAT_sum_mean_2019","ASCAT_sum_mean_2020")
    
 
# compute city mean and std. dev. across grid cells    
    
  # 1. ERS
    
    df.invariant_ers_time_series = as.data.frame(array(0,c(5,(2+ers_len))))
    colnames(df.invariant_ers_time_series) = c("city","variable", '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000')
    
    df.invariant_ers_time_series[1,1] = city_name
    df.invariant_ers_time_series[1,2] = 'ers_s0_mean'
    df.invariant_ers_time_series[1,3:(2+ers_len)] = colMeans(invariant_grid_ers_s0[4:(3+ers_len)],na.rm=T)
    
    df.invariant_ers_time_series[2,1] = city_name
    df.invariant_ers_time_series[2,2] = 'ers_s0_stdev'
    if (city_name == 'Amazon1' | city_name == 'Amazon2') {
      for (iyear in 1:ers_len) {
        df.invariant_ers_time_series[2,(2+iyear)] = sqrt(var(invariant_grid_ers_s0[,(3+iyear)],use="complete.obs"))
      }
    } else {  # NOTE, 1994 ERS JFM data missing for both PapuaNewGuinea and DemRepCongo
      df.invariant_ers_time_series[2,3] = sqrt(var(invariant_grid_ers_s0[,4],use="complete.obs"))
      df.invariant_ers_time_series[2,4] = NaN
      for (iyear in 3:ers_len) {
        df.invariant_ers_time_series[2,(2+iyear)] = sqrt(var(invariant_grid_ers_s0[,(3+iyear)],use="complete.obs"))
      }
    }
      
    df.invariant_ers_time_series[3,1] = city_name
    df.invariant_ers_time_series[3,2] = 'ers_pr_mean'
    df.invariant_ers_time_series[3,3:(2+ers_len)] = 10^(df.invariant_ers_time_series[1,3:(2+ers_len)]/10)  # convert sig0_dB in PR
    
    df.invariant_ers_time_series[4,1] = city_name
    df.invariant_ers_time_series[4,2] = 'ers_pr_mean+1sigma'
    df.invariant_ers_time_series[4,3:(2+ers_len)] = 10^((df.invariant_ers_time_series[1,3:(2+ers_len)] + df.invariant_ers_time_series[2,3:(2+ers_len)])/10)  
    
    df.invariant_ers_time_series[5,1] = city_name
    df.invariant_ers_time_series[5,2] = 'ers_pr_mean-1sigma'
    df.invariant_ers_time_series[5,3:(2+ers_len)] = 10^((df.invariant_ers_time_series[1,3:(2+ers_len)] - df.invariant_ers_time_series[2,3:(2+ers_len)])/10)  
    
    invariant_ers_pr_linear_fit = lm(as.numeric(df.invariant_ers_time_series[3,3:(2+ers_len)]) ~ ers_years)
    ers_20_pr_slope = invariant_ers_pr_linear_fit$coefficients[2]
    ers_20_pr_r2 = summary(invariant_ers_pr_linear_fit)$r.squared
    ers_20_pr_slope_pval = coef(summary(invariant_ers_pr_linear_fit))[2,4]
    
    df.all_invariant_pr_slopes[icity,1] = city_name 
    df.all_invariant_pr_slopes[icity,2] = ers_20_pr_slope 
    df.all_invariant_pr_slopes[icity,3] = ers_20_pr_r2 
    df.all_invariant_pr_slopes[icity,4] = ers_20_pr_slope_pval 
    
  # 3. QSCAT
    
    df.invariant_qscat_time_series = as.data.frame(array(0,c(5,(2+qscat_len))))
    colnames(df.invariant_qscat_time_series) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
    
    df.invariant_qscat_time_series[1,1] = city_name
    df.invariant_qscat_time_series[1,2] = 'qscat_s0_mean'
    df.invariant_qscat_time_series[1,3:(2+qscat_len)] = colMeans(invariant_grid_qscat_s0[4:(3+qscat_len)],na.rm=T)
    
    df.invariant_qscat_time_series[2,1] = city_name
    df.invariant_qscat_time_series[2,2] = 'qscat_s0_stdev'
    
    for (iyear in 2:qscat_len) {
      df.invariant_qscat_time_series[2,(3+iyear)] = sqrt(var(invariant_grid_qscat_s0[,(3+iyear)],use="complete.obs"))
    }
    # issue with summer 1999 QSCAT data in southern hemisphere - NO DATA
    if (max(invariant_grid_data_input$latitude, na.rm=T) >= 0) { # northern hemisphere, so 'summer' = JAS QSCAT data exist for 1999
      df.invariant_qscat_time_series[2,3] = sqrt(var(invariant_grid_qscat_s0[,3],use="complete.obs"))
    } else {   # southern hemisphere, so 'summer' = JAS QSCAT data DO NOT exist for 1999
      df.invariant_qscat_time_series[2,3] = NaN
    }

    df.invariant_qscat_time_series[3,1] = city_name
    df.invariant_qscat_time_series[3,2] = 'qscat_pr_mean'
    df.invariant_qscat_time_series[3,3:(2+qscat_len)] = 10^(df.invariant_qscat_time_series[1,3:(2+qscat_len)]/10)  # convert sig0_dB in PR
    
    df.invariant_qscat_time_series[4,1] = city_name
    df.invariant_qscat_time_series[4,2] = 'qscat_pr_mean+1sigma'
    df.invariant_qscat_time_series[4,3:(2+qscat_len)] = 10^((df.invariant_qscat_time_series[1,3:(2+qscat_len)] + df.invariant_qscat_time_series[2,3:(2+qscat_len)])/10)  
    
    df.invariant_qscat_time_series[5,1] = city_name
    df.invariant_qscat_time_series[5,2] = 'qscat_pr_mean-1sigma'
    df.invariant_qscat_time_series[5,3:(2+qscat_len)] = 10^((df.invariant_qscat_time_series[1,3:(2+qscat_len)] - df.invariant_qscat_time_series[2,3:(2+qscat_len)])/10)  
    
    invariant_qscat_pr_linear_fit = lm(as.numeric(df.invariant_qscat_time_series[3,3:(2+qscat_len)]) ~ qscat_years)
    qscat_20_pr_slope = invariant_qscat_pr_linear_fit$coefficients[2]
    qscat_20_pr_r2 = summary(invariant_qscat_pr_linear_fit)$r.squared
    qscat_20_pr_slope_pval = coef(summary(invariant_qscat_pr_linear_fit))[2,4]
    
    df.all_invariant_pr_slopes[icity,5] = qscat_20_pr_slope 
    df.all_invariant_pr_slopes[icity,6] = qscat_20_pr_r2 
    df.all_invariant_pr_slopes[icity,7] = qscat_20_pr_slope_pval 
    
  # 3. ASCAT
    
    df.invariant_ascat_time_series = as.data.frame(array(0,c(5,(2+ascat_len))))
    colnames(df.invariant_ascat_time_series) = c("city","variable",'2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
    
    df.invariant_ascat_time_series[1,1] = city_name
    df.invariant_ascat_time_series[1,2] = 'ascat_s0_mean'
    df.invariant_ascat_time_series[1,3:(2+ascat_len)] = colMeans(invariant_grid_ascat_s0[4:(3+ascat_len)],na.rm=T)
    
    df.invariant_ascat_time_series[2,1] = city_name
    df.invariant_ascat_time_series[2,2] = 'ascat_s0_stdev'
    for (iyear in 1:ascat_len) {
      df.invariant_ascat_time_series[2,(2+iyear)] = sqrt(var(invariant_grid_ascat_s0[,(3+iyear)],use="complete.obs"))
    }

    df.invariant_ascat_time_series[3,1] = city_name
    df.invariant_ascat_time_series[3,2] = 'ascat_pr_mean'
    df.invariant_ascat_time_series[3,3:(2+ascat_len)] = 10^(df.invariant_ascat_time_series[1,3:(2+ascat_len)]/10)  # convert sig0_dB in PR
    
    df.invariant_ascat_time_series[4,1] = city_name
    df.invariant_ascat_time_series[4,2] = 'ascat_pr_mean+1sigma'
    df.invariant_ascat_time_series[4,3:(2+ascat_len)] = 10^((df.invariant_ascat_time_series[1,3:(2+ascat_len)] + df.invariant_ascat_time_series[2,3:(2+ascat_len)])/10)  
    
    df.invariant_ascat_time_series[5,1] = city_name
    df.invariant_ascat_time_series[5,2] = 'ascat_pr_mean-1sigma'
    df.invariant_ascat_time_series[5,3:(2+ascat_len)] = 10^((df.invariant_ascat_time_series[1,3:(2+ascat_len)] - df.invariant_ascat_time_series[2,3:(2+ascat_len)])/10)  
    
    invariant_ascat_pr_linear_fit = lm(as.numeric(df.invariant_ascat_time_series[3,3:(2+ascat_len)]) ~ ascat_years)
    ascat_20_pr_slope = invariant_ascat_pr_linear_fit$coefficients[2]
    ascat_20_pr_r2 = summary(invariant_ascat_pr_linear_fit)$r.squared
    ascat_20_pr_slope_pval = coef(summary(invariant_ascat_pr_linear_fit))[2,4]
    
    df.all_invariant_pr_slopes[icity,8] = ascat_20_pr_slope 
    df.all_invariant_pr_slopes[icity,9] = ascat_20_pr_r2 
    df.all_invariant_pr_slopes[icity,10] = ascat_20_pr_slope_pval 
  
# Compute qscat offset and make arrays of those values

    if (max(invariant_grid_data_input$latitude, na.rm=T) >= 0) { # northern hemisphere, so 'summer' = JAS
      
      kuband.offset_pr = (sum(df.invariant_ers_time_series[3,c(9,10)], na.rm = T) + sum(df.invariant_ascat_time_series[3,c(3,4,5)], na.rm = T)  - 
                            sum(df.invariant_qscat_time_series[3,c(3,4,11,12,13)], na.rm = T)) / 5
       
      df.invariant_qscat_offset_time_series = as.data.frame(array(NA,c(5,(2+qscat_len))))
      colnames(df.invariant_qscat_offset_time_series) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
      df.invariant_qscat_offset_time_series[,1:2] = df.invariant_qscat_time_series[,1:2] 
      df.invariant_qscat_offset_time_series[3:5,3:(2+qscat_len)] = df.invariant_qscat_time_series[3:5,3:(2+qscat_len)] + kuband.offset_pr
      
    } else { # southern hemisphere, so 'summer' = JFM, and Qscat has no summer value for 1999.
      
      kuband.offset_pr = (sum(df.invariant_ers_time_series[3,c(10)], na.rm = T) + sum(df.invariant_ascat_time_series[3,c(3,4,5)], na.rm = T)  - 
                               sum(df.invariant_qscat_time_series[3,c(4,11,12,13)], na.rm = T)) / 4

      df.invariant_qscat_offset_time_series = as.data.frame(array(NA,c(5,(2+qscat_len))))
      colnames(df.invariant_qscat_offset_time_series) = c("city","variable", '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009')
      df.invariant_qscat_offset_time_series[,1:2] = df.invariant_qscat_time_series[,1:2] 
      df.invariant_qscat_offset_time_series[3:5,3:(2+qscat_len)] = df.invariant_qscat_time_series[3:5,3:(2+qscat_len)] + kuband.offset_pr
      
    }
    
# make time series plot for invariant site
    
    pdf(paste(output_dir,city_name,"_","backscatter_pr_timeseries",'.pdf',sep = ''))
    par(mfrow=c(2,1),mar=c(4,4,4,4))
    
    # make plots for backscatter time series in PR for BF_2014 minima of 20% and 10%
    
    xrange = c(1993,2020)
    yrange = c(0,0.8)
  
    main_title =paste(city_name,sep="")
    
    plot(ers_years,df.invariant_ers_time_series[3,3:(2+ers_len)],
         xlim = xrange,
         ylim = yrange,
         xlab = "year",
         ylab = "backscatter PR",
         col = "red",pch = 19,
         #           main = paste(city_name,", BltFrac = ",bltfrac_intervals[ibltfrac]," - ",bltfrac_intervals[ibltfrac+1],", Ngrid=",df.city_bltfrac_backscatter_s0_time_series[ibltfrac,3],sep="")
         main = main_title
    )
    arrows(as.numeric(ers_years), as.numeric(df.invariant_ers_time_series[4,3:(2+ers_len)]), 
           as.numeric(ers_years), as.numeric(df.invariant_ers_time_series[5,3:(2+ers_len)]), 
           length=0.05, col="red", angle=90, code=3)
    text(1997,0.025, paste(as.character(formatC(df.all_invariant_pr_slopes[icity,2],digits=4,format='f')), sep=""), col="red",cex=1)
    
    points(qscat_years,df.invariant_qscat_time_series[3,3:(2+qscat_len)], col = "blue",pch = 19)
    arrows(as.numeric(qscat_years), as.numeric(df.invariant_qscat_time_series[4,3:(2+qscat_len)]), 
           as.numeric(qscat_years), as.numeric(df.invariant_qscat_time_series[5,3:(2+qscat_len)]), 
           length=0.05, col="blue", angle=90, code=3)
    text(2004,0.025, paste(as.character(formatC(df.all_invariant_pr_slopes[icity,5],digits=4,format='f')), sep=""), col="blue",cex=1)
    
    points(ascat_years,df.invariant_ascat_time_series[3,3:(2+ascat_len)], col = "goldenrod",pch = 19)
    arrows(as.numeric(ascat_years), as.numeric(df.invariant_ascat_time_series[4,3:(2+ascat_len)]), 
           as.numeric(ascat_years), as.numeric(df.invariant_ascat_time_series[5,3:(2+ascat_len)]), 
           length=0.05, col="goldenrod", angle=90, code=3)
    text(2015,0.025, paste(as.character(formatC(df.all_invariant_pr_slopes[icity,8],digits=4,format='f')), sep=""), col="goldenrod",cex=1)
    
    points(qscat_years,df.invariant_qscat_offset_time_series[3,3:(2+qscat_len)], col = "cyan4",pch = 19)
    arrows(as.numeric(qscat_years), as.numeric(df.invariant_qscat_offset_time_series[4,3:(2+qscat_len)]), 
           as.numeric(qscat_years), as.numeric(df.invariant_qscat_offset_time_series[5,3:(2+qscat_len)]), 
           length=0.05, col="cyan4", angle=90, code=3)

    legend("topleft",c("ERS","QSCAT","QSCAT + offset","ASCAT"),pch=c(19,7,19,19),col=c("red","blue","cyan4","goldenrod"),cex=0.55)
    
    text(2004,0.75,paste(city_name,sep=""), col="black",cex=1)
    
    
    dev.off()
    
  }  # end 'if city file exists'
  
  
}  # end loop through cities

outfile.name = paste(output_dir,"all_invariant_backscatter_pr_slopes",".csv",sep = "")
write.csv(df.all_invariant_pr_slopes,file = outfile.name,row.names = F)

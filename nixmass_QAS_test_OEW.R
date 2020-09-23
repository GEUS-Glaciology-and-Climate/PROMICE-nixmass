## Test script to calculate snow water equivalent for Promice QAS stations using the library nixmass
# The library is documented in the paper "Snow Water Equivalents exclusively from Snow Heights and their temporal Changes: The ∆SNOW.MODEL" (Winkler, 2020)

# Unresolved issues:
# Are the physical parameters subject to calibration? If so, how to determine and change these?
# Do the results fit with observational data?
# Is HeightSensorBoom the vertical distance from a fixed point to the snow surface?
# 2 requirements for data: first data point has to be 0 and data can not be negative. Solution: find local minimum for each melt season, and calculate until next minimum


# Links:
# Repository 		https://rdrr.io/rforge/nixmass/man/nixmass.html
# Paper 			  https://www.hydrol-earth-syst-sci-discuss.net/hess-2020-152/hess-2020-152.pdf
# Data 			    https://promice.org/PromiceDataPortal/api/download/f24019f7-d586-4465-8181-d4965421e6eb/v03

# Created by OEW 12. june 2020
# Last modified by OEW 22. september 2020

library(nixmass)

datalist = list("QAS_U_day_v03.txt", "QAS_M_day_v03.txt", "QAS_L_day_v03.txt")
for (k in 3:3)
{
  dataset = datalist[k]

  df        = read.table(paste("/Users/owinton/Documents/GEUS/delta_snow_model/", dataset, sep=""), header = TRUE)
  year      = df[["Year"]]
  month     = df[["MonthOfYear"]]
  dom       = df[["DayOfMonth"]]
  hs        = df[["HeightSensorBoom.m."]]
  
  
  
  hs = - hs # data given as negative elevation, input to nixmass is snow thickness
  date  = year # used to store dates
  
  # Convert dates to character with format YYYY-MM-DD
  for (i in 1:nrow(df))
  {
    # Ensure that month 1 is represented 01 etc.
    m = as.character(month[i])
    n = nchar(m)
    m = substr(paste0("0", m), n, n+1)
    
    # Ensure that day 1 is represented 01 etc.
    d = as.character(dom[i])
    n = nchar(d)
    d = substr(paste0("0", d), n, n+2)
    date[i] = paste0(as.character(year[i]), "-", m, "-", d)
    
    # Deal with nans (represented in data as -999, data are reversed and shifted upon loading). Method: constant interpolation forward in time
    if (hs[i] > 100)
    {hs[i] = hs[i-1]}
    
  }
  
  # A subset of data is chosen from date of minimum snow height and 2 years ahead
  hs = hs - min(hs)
  startindex = which.min(hs)
  stopindex = startindex + 3*365
  date = date[startindex:stopindex]
  hs = hs[startindex:stopindex]
  
  ####### Code for linear interpolation for nans. Constant interpolation forward in time used instead
  #hz = zoo(h)
  #index(hz) = hz[,1]
  #hz_approx = na.approx(hz)
  ######
  
  # Date and snow height needed in dataframe for nixmass function
  data = data.frame(date, hs)
  o <- nixmass(data, model="delta.snow",verbose=TRUE)
  plot(o)
  title(paste("Dataset: ", dataset, ". \n Start date: ", date[1], sep=" "))
}
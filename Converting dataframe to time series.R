# CALCULATING BASE LEVEL INDEX
#  Ben Smith   -   07/03/2016
#     Newcastle University

# This script loads all of the river level data from a R.Data file, adds it to a list and then cycles through each gauge station and:
# 1 - converts time and date to POSIXlt format
# 2 - removes all missing dates (e.g. where there is a river level but no associated date)
# 3 - makes all (negative) error values NA's
# 4 - plots the time series of all data and daily data (if prompt is answered TRUE)
# 5 - converts the data into a time series (xts format)
# 6 - averages data to make it daily
# 7 - plots the base level
# 8 - calculates the base level index based on daily values
# 9 - returns values of:
#      Gauge Name
#      Start Date
#      End Date
#      Base Level Index
#      reference for the Base Level Plot

# Setup -------------------------------------------------------------------------
library(xts)
library(lfstat)
setwd(dir="H:/PhD Documents/Task 1/Test Documents") 
load("TEST_River_Level_Data.RData")

# Preamble -------------------------------------------------------------------------

Whole_Dataset  <- setNames(lapply(ls(pattern="tbl"), function(x) get(x)), ls(pattern="tbl"))
Gauge_Names    <- ls(Whole_Dataset)
Gauge_Names    <- lapply(Gauge_Names, function(x) x = substr(x,5,(nchar(x)-8)))

Table = data.frame(
  Gauge_Name = character(0),
  Start_Date = character(0),
  End_Date = character(0),
  Base_Level_Index = numeric(),
  Base_Level_Plot = character(0),
  stringsAsFactors=FALSE)

Plot  = readline(prompt = "You would like to plot the graphs?   (Please answer T / F) \n")

# Function -------------------------------------------------------------------------
for (x in 1:length(Whole_Dataset)){ # make sure you add the end bracket!

Gauge_Name <- Gauge_Names[[x]]
SAMPLE=Whole_Dataset[[x]]

# Change date format:
SAMPLE$ObsDate <- strptime(SAMPLE$ObsDate,"%Y-%m-%d %H:%M:%S")

# Gets rid of any rows which have missing dates:
Dates_to_remove <-  which(is.na(SAMPLE$ObsDate), arr.ind = TRUE)
if (length(Dates_to_remove)>0){
  SAMPLE <-  SAMPLE[-Dates_to_remove,]
  print(paste(length(Dates_to_remove), " missing dates were removed for ", Gauge_Name, sep = ""))
}  else{
  print(paste("There were no missing dates in ", Gauge_Name, sep = ""))
}

# Changes any negative values to NA values:
Missing_data <-  which(SAMPLE$Value <0, arr.ind = TRUE)
if (length(Missing_data)>0){
  SAMPLE$Value[Missing_data] = NA
  print(paste(length(Missing_data), " errors were changed to NA values in ", Gauge_Name, sep = ""))
}  else{
  print(paste("There were no errant river levels in ", Gauge_Name, sep = ""))
}

# Converts the dataframe into a time series:
SAMPLE_ts = xts(SAMPLE$Value,SAMPLE$ObsDate) # converts dataframe to timeseries

# Plots the time series:
if (Plot == TRUE){
  xlabel = paste(substr(start(SAMPLE_ts),1,10)," to ", substr(end(SAMPLE_ts),1,10), sep = "")
  mlabel = paste("River Level \n (up to 15 minute resolution) \n Gauge number: ", Gauge_Name)
  plot(SAMPLE_ts, main = mlabel, xlab = xlabel, ylab = "River Level (meters)")
  }

# Converts and plots hourly data to daily data:
SAMPLE_ts_daily = apply.daily(SAMPLE_ts,mean)
if (Plot == TRUE){
  xlabel_daily = paste(start(SAMPLE_ts_daily), end(SAMPLE_ts_daily), sep = "  to ")
  mlabel_daily = paste("Daily River Level \n Gauge number: ", Gauge_Name)
  plot(SAMPLE_ts_daily, main = mlabel_daily, xlab = xlabel_daily, ylab = "River Level (meters)")
}

# Calculate Base Level -------------------------------------------------------------------------

# 1). Make a dataframe that can be converted to a lfobj (Done in 2 steps):
SAMPLE_ts_daily <-  data.frame(
  "date"        = index(SAMPLE_ts_daily),
  "River Level" = coredata(SAMPLE_ts_daily))
SAMPLE_ts_daily <- data.frame(
  "flow"  = SAMPLE_ts_daily$River.Level,
  "day"   = as.numeric(substr(SAMPLE_ts_daily$date,9,10)),
  "month" = as.numeric (substr(SAMPLE_ts_daily$date,6,7)),
  "year"  = as.numeric(substr(SAMPLE_ts_daily$date,1,4)),
  stringsAsFactors = F) 

# 2). Create a Low Flow object:
SAMPLE_daily_lfobj <- createlfobj(SAMPLE_ts_daily)                                 

# 3). Plot the baseflow:
if (Plot == TRUE){
  bfplot(SAMPLE_daily_lfobj,
     year  = "any",
     col   = "blue",
     bfcol = "red",
     main  = paste("Daily River Level & Base Flow \n Gauge number: ", substr(Gauge_Name),
     xlab  = paste(start(SAMPLE_ts_daily), end(SAMPLE_ts_daily), sep = "  to "),
     ylab  = "River Level (meters)"))
}

# 4). Record the plot:
assign(paste("BL", Gauge_Name, sep = "_"),value=recordPlot())

# Save the data -------------------------------------------------------------------------

START    <- paste(first(SAMPLE_ts_daily$day),first(SAMPLE_ts_daily$month), first(SAMPLE_ts_daily$year), sep = "-")
END      <- paste(last(SAMPLE_ts_daily$day),last(SAMPLE_ts_daily$month), last(SAMPLE_ts_daily$year), sep = "-")
Entry    <- data.frame(
  "Gauge_Name"       = Gauge_Names[[x]],
  "Start_Date"       = START,
  "End_Date"         = END,
  "Removed_Entries"  = Dates_to_remove,
  "Removed_Errors"   = Missing_data,  
  "Base_Level_Index" = (assign(paste("BLI", Gauge_Name, sep= "_"), value= BFI(SAMPLE_daily_lfobj, year= "any", breakdays = NULL, yearly = F))),
  "Base_Level_Plot"  = paste("BL", Gauge_Name, sep = "_"),
  stringsAsFactors = FALSE)

Table = rbind (Table, Entry)
}

View(Entry)

# EXTRA: ----------------------------------------------------------------------------------------------
# load("tbl_River1.RData")
# load("tbl_River2.RData")
# load("tbl_River3.RData")
# River_List = list(tbl_River1[,c(1,2)],tbl_River2[,c(1,2)],tbl_River3[,c(1,2)]) # does this still have the river labels?

# may use strptime at some point
# Small_Dataset = list(tbl_029003SG_River,tbl_033007SG_River)
# Dates_col = list(Gauge_Names[c(1,2)])
# Dates_col = lapply(Small_Dataset, function(x) cbind(x = strptime(x$ObsDate,"%Y-%m-%d %H:%M:%S"))) # the thing must be a list, not a vector.
# lapply (Whole_Dataset, function(x) range(x$QualityCode)) determine range of quality codes
# Test = data.frame(tbl_029003SG_River[1,1000])
# SAMPLE = data.frame(ObsDate= tbl_029003SG_River$ObsDate[c(1,1000)], Value= tbl_029003SG_River$Value[c(1,1000)])
# str(Test)
# SAMPLE = data.frame(ObsDate= (0), Value= (0), QualityCode = (0))

#  # ATTEMPT TO SUSS OUT THE FUNCTION THING
# Table = data.frame(Gauge = character(0), Start_Date = character(0), stringsAsFactors=FALSE)
# Y     = data.frame(Gauge = character(0), Sample = numeric(), stringsAsFactors=FALSE)
# 
# for (x in 1:length(Whole_Dataset)){ # make sure you add the end bracket!
#   z= data.frame(Gauge= Gauge_Names[[x]], Start_Date = (Whole_Dataset[[x]]$ObsDate[1]), stringsAsFactors=F)
#   y = data.frame(Gauge= Gauge_Names[[x]], Sample = sample(x=1:100, size=1,replace=F), stringsAsFactors=F)
#   Table = rbind(Table,z)
#   Y = rbind(Y,y)
# }

# TABLE= cbind(Table, Sample = Y$Sample)

# Calculate the Base LEvel Index
# (assign(paste("BLI", Gauge_Name, sep= "_"), value= BFI(SAMPLE_daily_lfobj, year= "any", breakdays = NULL, yearly = F)))

# NA_continuous <- na.contiguous(object=SAMPLE_ts_daily) # find the longest stretch of consecutive of NA's
# length(NA_continuous); str(NA_continuous)

# install.packages("zoo", "xts","lfstat")
# library(zoo) # need to have this on the start of the script. Before  this you need to install it on the computer

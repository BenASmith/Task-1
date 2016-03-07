# CALCULATING BASE LEVEL INDEX
#  Ben Smith   -   07/03/2016
#     Newcastle University

# This script loads all of the river level data from a R.Data file, adds it to a list and then cycles through each gauge station and:
# 1 - converts time and date to POSIXlt format
# 2 - removes all missing dates (e.g. where there is a river level but no associated date)
# 3 - makes all (*negative*) error values NA's
# 4 - plots the time series of all data and daily data (*if prompt is answered TRUE*)
# 5 - converts the data into a time series (xts format)
# 6 - averages data to make it daily
# 7 - plots the base level
# 8 - calculates the base level index based on daily values
# 9 - returns values of:
#      -Gauge Name
#      -Start Date
#      -End Date
#      -Base Level Index
#      -reference for the Base Level Plot

# Issues with this script - Base Level plots can't be customised and it may be worth checking out different methods of estimating base flow as it can depend on things like rock type. 

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
  Gauge_Name       = character(0),
  Start_Date       = character(0),
  End_Date         = character(0),
  Removed_Entries  = numeric(0),
  Removed_Errors   = numeric(0),
  Base_Level_Index = numeric(0),
  Base_Level_Plot  = character(0),
  stringsAsFactors = FALSE)

Plot = as.logical(readline(prompt = "You would like to plot the graphs?   (Please answer T / F) \n"))

# Function -------------------------------------------------------------------------
for (x in 1:length(Whole_Dataset)){

Gauge_Name <- Gauge_Names[[x]]
SAMPLE=Whole_Dataset[[x]]

# Change date format:
SAMPLE$ObsDate <- strptime(SAMPLE$ObsDate,"%Y-%m-%d %H:%M:%S")

# Remove any rows which have missing dates:
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
if (Plot == T){
  xlabel = paste(substr(start(SAMPLE_ts),1,10)," to ", substr(end(SAMPLE_ts),1,10), sep = "")
  mlabel = paste("River Level \n (up to 15 minute resolution) \n Gauge number: ", Gauge_Name)
  plot(SAMPLE_ts, main = mlabel, xlab = xlabel, ylab = "River Level (meters)")
}

# Converts and plots hourly data to daily data:
SAMPLE_ts_daily = apply.daily(SAMPLE_ts,mean)
if (Plot == T){
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

# 3). Plot the baseflow and record the plot:
if (Plot == T){
  bfplot(SAMPLE_daily_lfobj,
     year  = "any",
     col   = "blue",
     bfcol = "red")
     #main  = paste("Daily River Level & Base Flow \n Gauge number: ", Gauge_Name)
     #xlab  = paste(start(SAMPLE_ts_daily), end(SAMPLE_ts_daily))
     #ylab  = "River Level (meters)")
  assign(paste("BL", Gauge_Name, sep = "_"), value=recordPlot())
}

# Calculate Base Level Index -------------------------------------------------------------------------
BLI = BFI(SAMPLE_daily_lfobj, year= "any", breakdays = NULL, yearly = F)
BLI = round(BLI, digits=4)

# Save the data -------------------------------------------------------------------------

START    <- paste(first(SAMPLE_ts_daily$day),first(SAMPLE_ts_daily$month), first(SAMPLE_ts_daily$year), sep = "-")
END      <- paste(last(SAMPLE_ts_daily$day),last(SAMPLE_ts_daily$month), last(SAMPLE_ts_daily$year), sep = "-")
Entry    <- data.frame(
  "Gauge_Name"       = Gauge_Names[[x]],
  "Start_Date"       = START,
  "End_Date"         = END,
  "Removed_Entries"  = length(Dates_to_remove),
  "Removed_Errors"   = length(Missing_data),
  "Base_Level_Index" = (assign(paste("BLI", Gauge_Name, sep= "_"), value = BLI)),
  "Base_Level_Plot"  = paste("BL", Gauge_Name, sep = "_"),
  stringsAsFactors = FALSE)

Table = rbind (Table, Entry)
}

View(Table)

# EXTRA: ----------------------------------------------------------------------------------------------
# baseflow(SAMPLE_daily_lfobj, tp.factor = 0.9, block.len = 5)
# hydrograph(SAMPLE_daily_lfobj, startdate = NULL, enddate = NULL, amin = FALSE)
# rfa(SAMPLE_daily_lfobj, n = 7, event = 100, dist = c("wei","gev","ln3","gum","pe3"))


# load("tbl_River1.RData")
# load("tbl_River2.RData")
# load("tbl_River3.RData")
# River_List = list(tbl_River1[,c(1,2)],tbl_River2[,c(1,2)],tbl_River3[,c(1,2)]) 

# lapply (Whole_Dataset, function(x) range(x$QualityCode)) determine range of quality codes

# NA_continuous <- na.contiguous(object=SAMPLE_ts_daily) # find the longest stretch of consecutive of NA's
# length(NA_continuous); str(NA_continuous)

# install.packages("zoo", "xts","lfstat")
# library(zoo) # need to have this on the start of the script. Before  this you need to install it on the computer

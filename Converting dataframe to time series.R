#From Code Club (16/02/2016)

install.packages("zoo", "xts","lfstat")
library(zoo) # need to have this on the start of the script. Before  this you need to install it on the computer
library(xts)
library(lfstat)

setwd(dir="H:/PhD Documents/Task 1/Test Documents") 
load("TEST_River_Level_Data.RData")
Whole_Dataset  <- setNames(lapply(ls(pattern="tbl"), function(x) get(x)), ls(pattern="tbl"))
Gauge_Names    <- ls(Whole_Dataset)

load("tbl_River1.RData")
load("tbl_River2.RData")
load("tbl_River3.RData")
River_List = list(tbl_River1[,c(1,2)],tbl_River2[,c(1,2)],tbl_River3[,c(1,2)]) # does this still have the river labels?
# may use strptime at some point
Small_Dataset = list(tbl_029003SG_River,tbl_033007SG_River)
Dates_col = list(Gauge_Name[c(1,2)])
Dates_col = lapply(Small_Dataset, function(x) cbind(x = strptime(x$ObsDate,"%Y-%m-%d %H:%M:%S"))) # the thing must be a list, not a vector.
for x in Whole_Dataset {range(Whole_Dataset$x$QualityCode)}
print(range(Whole_Dataset$tbl_029003SG_River$QualityCode))


# Convert to a time series ---------------------------------------------------------------------
# Do this individually, then try and make it a simple function which you can use in lapply to Whole_Dataset
Plot = readline(prompt = "You would like to plot the graphs?   (Please answer T / F) \n")

for (x in 1:length(Whole_Dataset)){ # make sure you add the end bracket!
Gauge_Name <- Gauge_Names[x]
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
  print(paste("There were no errant river levels in ", Gauge_Name, sep = "")))
}

# Converts the dataframe into a time series:
SAMPLE_ts = xts(SAMPLE$Value,SAMPLE$ObsDate) # converts dataframe to timeseries

# Plots the time series:
if (Plot == TRUE){
  xlabel = paste(substr(start(SAMPLE_ts),1,10)," to ", substr(end(SAMPLE_ts),1,10), sep = "")
  mlabel = paste("River Level \n (up to 15 minute resolution) \n Gauge number: ", substr(Gauge_Name,5,nchar(Gauge_Name)))
  plot(SAMPLE_ts, main = mlabel, xlab = xlabel, ylab = "River Level (meters)")
  }

# Converts and plots hourly data to daily data:
SAMPLE_ts_daily = apply.daily(SAMPLE_ts,mean)
if (Plot == TRUE){
  xlabel_daily = paste(start(SAMPLE_ts_daily), end(SAMPLE_ts_daily), sep = "  to ")
  mlabel_daily = paste("Daily River Level \n Gauge number: ", substr(Gauge_Name,5,nchar(Gauge_Name)))
  plot(SAMPLE_ts_daily, main = mlabel_daily, xlab = xlabel_daily, ylab = "River Level (meters)")

# This is identical but reduces data volume sooner so is presumably faster? --------------------
# # Gets rid of any rows which have missing dates:
# Dates_to_remove_2 <-  which(is.na(tbl_River1[,c(1,2)]), arr.ind = TRUE)[1]
# RIVER_1         <-  tbl_River1[-Dates_to_remove_2,]
 
# # Converts the dataframe into a time series:
# RIVER_1 = xts(RIVER_1$Value,RIVER_1$ObsDate)
# 
# # Converts and plots hourly data to daily data:
# RIVER_1_daily = apply.daily(RIVER_1,mean)
# 
# # Changes any negative values to NA values:
# RIVER_1_daily[RIVER_1_daily<0] = NA
# 
# # Plots the time series:
# plot(RIVER_1_daily)

# NA_continuous <- na.contiguous(object=SAMPLE_ts_daily) # find the longest stretch of consecutive of NA's
# length(NA_continuous); str(NA_continuous)

# Calculate Base Level -------------------------------------------------------------------------

SAMPLE_ts_daily <-  data.frame(date=index(SAMPLE_ts_daily), "River Level" = coredata(SAMPLE_ts_daily))

SAMPLE_ts_daily <- data.frame("flow" = SAMPLE_ts_daily$River.Level, "day"= as.numeric(substr(SAMPLE_ts_daily$date,9,10)), "month" = as.numeric(substr(SAMPLE_ts_daily$date,6,7)), "year"= as.numeric(substr(SAMPLE_ts_daily$date,1,4)), stringsAsFactors=F) #makes a dataframe that can be converted to a lfobj

SAMPLE_daily_lfobj <- createlfobj(SAMPLE_ts_daily)
bfplot(SAMPLE_daily_lfobj, year= "any", col="blue", bfcol="red")

SAMPLE_daily_BFI= BFI(SAMPLE_daily_lfobj, year= "any", breakdays = NULL, yearly = F)

  
# tbl_029003SG_ts    = SAMPLE_ts
# tbl_029003SG_daily = SAMPLE_ts_daily
# Base_Flow_Indexes = list("Gauge" = "tbl_River1", "BFI" = 0.951)
name(Gauge_Name)
  
assign(paste(Gauge_Name, "Time_Series", sep = "_"),value=SAMPLE_ts) return
a = "Tom"
Data = rbind(Data, paste(Gauge_Name, "Time_Series", sep = "_")=  10)
}
b = 10
Gauge_Name[1] = 10

  
  str(Name)
Name.Gauge_Name, "Time_Series", sep = "_") =1
  
Name[1] =SAMPLE_ts 

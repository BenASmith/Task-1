# Script 1 - Process Raw data
#  Ben Smith   -   06/04/2016
#     Newcastle University

# ------------------------ Setup and Preamble ---------------------------------------------
setwd(dir="H:/PhD Documents/Task 1/R Scripts/")  # May need changing depending on computer
load("TEST_River_Level_Data.RData")
Whole_Dataset  <- setNames(lapply(ls(pattern="tbl"), function(x) get(x)), ls(pattern="tbl"))
Gauge_Names    <- ls(Whole_Dataset)
Gauge_Names    <- lapply(Gauge_Names, function(x) x = substr(x,5,(nchar(x)-8)))

# ------------------------ Function --------------------------------------------------------
for (x in 1:length(Whole_Dataset)){
  
  Gauge_Name <- Gauge_Names[[x]]
  SAMPLE     <- Whole_Dataset[[x]]
  SAMPLE$QualityCode = NULL
  
  # ------------------------ Convert dates of raw data into POSTIXlt:------------------------
  SAMPLE$ObsDate <- strptime(SAMPLE$ObsDate,"%Y-%m-%d %H:%M:%S")
  
  
  #------------------------ Remove any rows which have missing dates:------------------------
  Dates_to_remove <-  which(is.na(SAMPLE$ObsDate), arr.ind = TRUE)
  
  if (length(Dates_to_remove)>0){
    SAMPLE <-  SAMPLE[-Dates_to_remove,]
    print(paste(length(Dates_to_remove), " missing dates were removed for ", Gauge_Name, sep = ""))
  }  else{
    print(paste("There were no missing dates in ", Gauge_Name, sep = ""))
  }
  
  # ------------------------ Change any negative river levels to NA values:------------------------
  Missing_data <-  which(SAMPLE$Value <0, arr.ind = TRUE)
  
  if (length(Missing_data)>0){
    SAMPLE$Value[Missing_data] = NA
    print(paste(length(Missing_data), " errors were changed to NA values in ", Gauge_Name, sep = ""))
  }  else{
    print(paste("There were no errant river levels in ", Gauge_Name, sep = ""))
  }

  # ------------------------ Save the Dataframe: ---------------------------------------------------
  Unique_Tag = paste("RL_", Gauge_Name, sep="")
  assign(Unique_Tag, value=SAMPLE)
  filepath=paste("Processed Data/", Unique_Tag,'.Rdata',sep="")
  save(list=Unique_Tag, file =filepath)
}

# ---> Tidy up Workspace:
rm(list=ls(pattern="tbl_"), Unique_Tag, Whole_Dataset, Dates_to_remove, Gauge_Name, Gauge_Names, Missing_data, x, SAMPLE, filepath)
#rm(list=ls(pattern="RL_"))

# ---> List Processed Files:
#Processed_Dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))#
#Gauge_Names        <- ls(Processed_Dataset)
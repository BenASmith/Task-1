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

  
# Save the dataframe ------------------------------------------------------
  Unique_Tag = paste("RL_", Gauge_Name, sep="")
  assign(Unique_Tag, value=SAMPLE)
  filepath=paste("Processed Data/", Unique_Tag,'.Rdata',sep="")
  save(list=Unique_Tag, file =filepath)

}

rm(list=ls(pattern="tbl_"), Unique_Tag, Whole_Dataset, Dates_to_remove, Gauge_Name, Gauge_Names, Missing_data, x, SAMPLE)


TRL_1 = data.frame("ObsDate" =RL_029003$ObsDate[0:20], "Value" = sample(0:5, 20, rep = T))
TRL_2 = data.frame("ObsDate" =RL_029003$ObsDate[0:20], "Value" = sample(0:5, 20, rep = T))
TRL_3 = data.frame("ObsDate" =RL_029003$ObsDate[0:20], "Value" = sample(0:5, 20, rep = T))


# Now save the data:
Processed_Dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
Gauge_Names        <- ls(Processed_Dataset)

filepath = file.path("TEST_", Gauge_Names[x], ".Rdata", fsep = "")
save(TRL_1, file ="TRL.Rdata")   # <--- Works but want to do multiples at once.



# Beyond here is mostly just musings but have good feeling about lapply

lapply(Processed_Dataset, function(x) save(x, file = as.character(x))) # <---- BAD WILL CRASH R
x = 1

for (x in 1:length(Gauge_Names)){
  filepath = file.path("TEST", x, ".Rdata", fsep = "")
  data = Processed_Dataset[[x]]
  save(data, file = filepath)
  
}
load(file="TEST1.Rdata")
load(file="TEST2.Rdata")



# 1
file     <- as.name(paste("RL_", Gauge_Name, sep="")) 
filename <- paste("RL_", Gauge_Name, ".Rdata", sep="")
save(file, file=filename) 
# 2
P = ls(pattern= )
save(list=(paste("RL_", Gauge_Name, sep="")), file="Processed Data/Brian.Rdata")
# 3
save(list = Whole_Dataset[1], file="Brian.Rdata")
# 4
saveRDS(SAMPLE, file = "./Ben.Rdata")
# 5
temp <- get(paste("RL_", Gauge_Name, sep="")) 
# 6
save(get(paste("RL_", Gauge_Name, sep="")),  file = "Brian.RData")
# 7
save(list = Whole_Dataset[1], file = "Brian.RData")
# 8
Unique_Tag = paste("RL_", Gauge_Name, sep="")
assign(Unique_Tag, value=SAMPLE)
save(Unique_Tag, file="Brian.Rdata") # Saves the tag name, not the associated list of variables
#9
save(list=Processed_Dataset, file=filepath, envir=as.environment(Processed_Dataset))

# Load
load("Brian.RData")

# ------------------------ Remove raw data and load processed data:------------------------
rm(RL_029003, SAMPLE,temp)
rm(list = ls(pattern="tbl"), SAMPLE, Dates_to_remove, Gauge_Name, Missing_data, Whole_Dataset)

filenames <- list.files("Processed Data/", pattern="RL*", full.names=T)

Tim = lapply(filenames, function(x) load(x))# , ls(pattern="tbl"))
str(Tim)
lapply(filenames, load("./Processed Data/"), globalenv)

for (x in 1:length(filenames)){
  filenames[x] = load(filenames[x])
}
load(filenames)


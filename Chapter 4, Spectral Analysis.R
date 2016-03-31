# Time Series Analysis and Its Applications - Chapter 4, Spectral Analysis
# Ben Smith - Newcastle University - B.a.h.Smith2@Ncl.ac.uk
# 31/01/2016

# From page 179:
t    <- 1:100
x1   <- 2*cos(2*pi*t*6/100) + 3*sin(2*pi*t*6/100)
x2   <- 4*cos(2*pi*t*10/100) + 5*sin(2*pi*t*10/100)
x3   <- 6*cos(2*pi*t*40/100) + 7*sin(2*pi*t*40/100)
x    <- x1 + x2 + x3

par(mfrow = c(2,2))
plot.ts(x1, ylim=c(-16,16), main="Freq=6/100, amp^2=13")
plot.ts(x2, ylim=c(-16,16), main="Freq=10/100, amp^2=41")
plot.ts(x3, ylim=c(-16,16), main="Freq=40/100, amp^2=85")
plot.ts(x, ylim=c(-16,16), main="sum")
# amp stands for amplitude, this presumably correlates to the strength of the signal in x.


# From page 180:
par(mfrow = c(1,1))
P    <- abs(2*fft(x)/100^2)
f    <- 0:50/100
plot(f, P[1:51], type="o", xlab="Frequency", ylab="Peridogram")
# spikes are produced at 0.06, 0.1 and 0.4 which are the frequencies stated above.

# ------------------------------------------------------------------------------------------------
# From page 188:

dft = fft(x)/sqrt(length(x))
idft  <- fft(dft, inverse=T)/sqrt(length(x))
 # ------------------------------------------------------------------------------------------------

setwd(dir="H:/PhD Documents/Task 1/Test Documents") 
load("tbl_River1.RData")
load("tbl_River2.RData")
load("tbl_River3.RData")
load("TEST_River_Level_Data.RData")
library(xts)

level            <- tbl_River3
Dates_to_remove  <- which(is.na(level$ObsDate), arr.ind = TRUE)
if (length(Dates_to_remove)>0){
  level        <- level[-Dates_to_remove,]}
Missing_data     <- which(level$Value <0, arr.ind = TRUE)
if (length(Missing_data)>0){
    level$Value[Missing_data] = NA}
plot(level$Value, type = "l") #ylim=c(0,0.5),
level_ts         <- xts(level$Value,level$ObsDate)

range(level$ObsDate)
plot(level_ts, xlab=range(level$ObsDate)) #, ylim=c(0,0.5))

# Do fast fourier analysis and plot periodogram:
par(mfrow = c(1,1))
Q    <- abs(2*fft(level_ts)/100^2)
f    <- 1:50/100
plot(f, Q[0:50], type="o", xlab="Frequency", ylab="Peridogram") # Q has [] limits so that it is the same length as f


level_ts.per = spec.pgram(level_ts, taper=0, log="no") # calculates the periodogram using a fft, and optionally smooths the result with a series of modified Daniell smoothers.
level_ts.per = spec.ar(level_ts) # Fits an AR model to x and computes the spectral density of the fitted model.



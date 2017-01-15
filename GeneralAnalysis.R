# Version 1.0

# A function to calculate hours.
hrs <- function(u) {
  x <- u * 3600
  return(x)
}
# The function calculates minutes.
mns <- function(m) {
  x <- m * 60
  return(x)
}

versionNum = readLines("inputs.txt")[1]
paste("The version of this analysis is", versionNum)

wd = readLines("inputs.txt")[2]
setwd(wd)

inputTestCmts = readLines("inputs.txt")[3]
paste(inputTestCmts)
# input test comments:
# Cross-calibration test of 8508As TE0391 and TE0377 using Au wire Zero and USB3 00006 10 kohms standard resistor.
# TE0391 was in it's rack and TE0377 was on the bench next to TE0391's rack in SSEC 518.
#
# 8508A-VIs-Z.rtf
fstInputAssoFile = readLines("inputs.txt")[4]
sndInputAssoFile = readLines("inputs.txt")[5]
srdInputAssoFile = readLines("inputs.txt")[6]
paste(fstInputAssoFile, "\n", sep = "")
paste(sndInputAssoFile, "\n", sep = "")
paste(srdInputAssoFile, "\n", sep = "")
# input associated files:
# Associated Files:
# TE0391-X_OHMS-4W_160915T0931.csv
# TE0377-X_OHMS-4W_160915T0931.csv
# RH520_TE0384_9-16-2016_2-20_PM.csv
#
TEfile = readLines("inputs.txt")[7]
TEfileHd = readLines(TEfile)[2:5]
paste(TEfileHd)
# read from file
# TE0377-X_OHMS-4W_160915T0931.csv Header Information:
# Comments:, Fluke 1 = Au_Zero, Fluke 2 = USB3-06T, Fluke 3 = USB3-06R
# Fluke Model,Fluke S/N,Dataproof Model,Dataproof S/N,Start Channel,Number of Channels,Number of Scans,Bank A/B,Ch. Offset
# 8508A,989158234   ,164,1293,1,3,138,B,Y
# Fluke Settings:,INPUT REAR,GUARD INT,LINEF 60,TRG_SRCE EXT,TRG  DELAY 0.000000,OHMS,
# Range= 10000.00,RESL7,FILT_OFF,FAST_ON,FOUR_WR,LOI_ON,

paste("Conventions: USB3-06 is a shortened version of  USB3 Standard Resistor SN 00006, and ", 
      "USB3-06T = USB3 SN 00006 Thermistor, USB3-06R = USB3 SN 00006 Standard Resistor", 
      "Note, the T stands for Thermistor and its value is measured in ohms and then converted to temperature using the standard 3 term Steinhart and Hart equation. ", 
      "In the Cross Correlation plots, only negative lags are real for these tests.", sep = "\n#")
# Conventions: USB3-06 is a shortened version of  USB3 Standard Resistor SN 00006, and 
# USB3-06T = USB3 SN 00006 Thermistor, USB3-06R = USB3 SN 00006 Standard Resistor
# Note, the T stands for Thermistor and its value is measured in ohms and then converted to temperature using the standard 3 term Steinhart and Hart equation.
# In the Cross Correlation plots, only negative lags are real for these tests.
#

dtName1 = readLines("inputs.txt")[8]
dt1 = read.csv(dtName1, header = T)
dt2 = read.csv(TEfile, skip = 5, header = TRUE)
# Create a file to store the tables.
fstLine = paste("File: ", TEfile, "\n", versionNum, "\n", sep = "")
txt = substr(TEfile, 1, nchar(TEfile)-4)
txt = paste(txt, ".txt", sep = "")
cat(fstLine, file = txt)
tableIndex = 1
paste("input offset:", 
      "Positive and negative offset times mean that the raw data's time stamp is ahead of or lagging behind, respectively with respects to standard time.", 
      "ie, Offset = 10 seconds means that the raw data's time stamp was 2016-09-15 T09:31:17 when the CST time was actually 2016-09-15 T09:31:07. ", 
      "test data time offset: 0 second ", "RH520 time offset: 3600 seconds", sep = "\n#")

offsetRH = readLines("inputs.txt")[9]
offsethr = as.numeric(strsplit(offsetRH, ":")[[1]][1])
offsetmn = as.numeric(strsplit(offsetRH, ":")[[1]][2])
offsetsec = as.numeric(strsplit(offsetRH, ":")[[1]][3])
overall_offset_RH = hrs(offsethr) + mns(offsetmn) + offsetsec

offsetTE = readLines("inputs.txt")[10]
offsethr = as.numeric(strsplit(offsetTE, ":")[[1]][1])
offsetmn = as.numeric(strsplit(offsetTE, ":")[[1]][2])
offsetsec = as.numeric(strsplit(offsetTE, ":")[[1]][3])
overall_offset_TE = hrs(offsethr) + mns(offsetmn) + offsetsec
# input offset:
# Positive and negative offset times mean that the raw data's time stamp is ahead of or lagging behind, respectively with respects to standard time.
# ie, Offset = 10 seconds means that the raw data's time stamp was 2016-09-15 T09:31:17 when the CST time was actually 2016-09-15 T09:31:07.
# test data time offset: 0 second
# RH520 time offset: 3600 seconds
paste(TEfile, "First data record start Date/Time:", dt2$Date[1], dt2$Time[1], sep = " ")
# TE0377-X_OHMS-4W_160915T0931.csv First data record start Date/Time: 9/15/2016 ,  09:31:17 AM
paste(TEfile, "Statistical data Start Date/Time:", dt2$Date[6], dt2$Time[6], sep = " ")
# TE0377-X_OHMS-4W_160915T0931.csv Statistical data Start Date/Time: 9/15/2016 ,  09:32:44 AM
paste(TEfile, "Statistical data End Date/Time:", dt2$Date[133], dt2$Time[133], sep = " ")
# TE0377-X_OHMS-4W_160915T0931.csv Statistical data End Date/Time: 9/15/2016 ,  10:06:07 AM

# RH520_TE0384_9-16-2016_2-20_PM.csv Raw data previous 24 hour Date/Time: 9/14/2016 , 10:32:44
date = as.character(dt1$DATE)
time = as.character(dt1$TIME)
rh_date_time = paste(date, time, sep = " ")
rh_date_time = strptime(rh_date_time, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")
rh_date_time = rh_date_time - overall_offset_RH
date = as.character(dt2$Date)
time = as.character(dt2$Time)
te_date_time = paste(date, time, sep = " ")
te_date_time = strptime(te_date_time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
te_date_time = te_date_time - overall_offset_TE

# this bunch of code only works when the seconds have two digits. 
bgn_date_time = paste(dt2$Date[6], dt2$Time[6], sep = " ")
bgn_date_time = strptime(bgn_date_time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
oneday_prev_bgn_date_time = bgn_date_time - hrs(24)
temp_string = as.character(oneday_prev_bgn_date_time)
temp_string = paste(substr(temp_string, 1, nchar(temp_string)-3), "00", sep = ":")
oneday_prev_bgn_date_time = strptime(temp_string, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")
oneday_prev_bgn = which(oneday_prev_bgn_date_time == rh_date_time)

# The reason I didn't directly use corresponding date and time to find the index is that there are irregular spaces in the date and time strings, so I transform to time object to standardize the format. 
sig_end_date_time = paste(dt2$Date[133], dt2$Time[133], sep = " ")
sig_end_date_time = strptime(sig_end_date_time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
temp_string = as.character(sig_end_date_time)
temp_string = paste(substr(temp_string, 1, nchar(temp_string)-3), "00", sep = ":")
sig_end_date_time = strptime(temp_string, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")
sig_end_date_time = sig_end_date_time + mns(1)
sig_end = which(sig_end_date_time == rh_date_time)
RH520_oneday_prev = dt1[oneday_prev_bgn:sig_end, ]
# Transform the time to characters
time1 = as.character(RH520_oneday_prev$TIME)

# Transform the date to characters
date1 = as.character(RH520_oneday_prev$DATE)
# Join the characters of time and date together
date_time1 = paste(date1[1], time1[1], sep = " ")
for(i in c(2:length(date1))){
  date_time1 = c(date_time1, paste(date1[i], time1[i], sep = " "))
}
# Read the temperature and transform to numbers without losing accuracy.
temperature1 = as.numeric(as.character(RH520_oneday_prev$TEMP))
# Read the humidity and transform to numbers without losing accuracy.
humidity1 = as.numeric(as.character(RH520_oneday_prev$RH)) 
# Time axis for 24 hours previous to test
time_axis1 = strptime(date_time1, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")
# Set the first time point to 0 and change the subsequent time points with the offset.
time_axis1 = time_axis1 - hrs(1)
plot(time_axis1, temperature1, ylim = c(18, 28), xlab = "Time (Day/Time)", ylab = "RH520 Temperature (Celsius Degree)", 
     main = "Plot 1.2.1  RH520 Environment Temperature vs Time\n for 24 hours before test start to end of test.")
paste("Start time: ", time_axis1[1], "End time: ", time_axis1[length(time_axis1)])
plot(time_axis1, temperature1, xlab = "Time (Day/Time)", ylab = "RH520 Temperature (Celsius Degree)", 
     main = "Plot 1.2.2  RH520 Environment Temperature vs Time\n for 24 hours before test start to end of test.")
paste("Start time: ", time_axis1[1], "End time: ", time_axis1[length(time_axis1)])
plot(time_axis1, humidity1, xlab = "Time (Day/Time)", ylab = "RH520 Relative Humidity (%)", ylim = c(20, 70), 
     main = "Plot 1.2.3  RH520 Environment Humidity vs Time\n for 24 hours before test start to end of test.")
paste("Start time: ", time_axis1[1], "End time: ", time_axis1[length(time_axis1)])
plot(time_axis1, humidity1, xlab = "Time (Day/Time)", ylab = "RH520 Relative Humidity (%)", 
     main = "Plot 1.2.4  RH520 Environment Humidity vs Time\n for 24 hours before test start to end of test.")
paste("Start time: ", time_axis1[1], "End time: ", time_axis1[length(time_axis1)])

# RH520_TE0384_9-16-2016_2-20_PM.csv Raw data previous 6 hour Date/Time: 9/15/2016 , 09:07:00
sixHr_prev_bgn_date_time = bgn_date_time - hrs(6)
temp_string = as.character(sixHr_prev_bgn_date_time)
sixHr_prev_bgn_date_time = paste(substr(temp_string, 1, nchar(temp_string)-3), "00", sep = ":")
sixHr_prev_bgn = which(sixHr_prev_bgn_date_time == rh_date_time)
RH520_6hour_prev = dt1[sixHr_prev_bgn:sig_end, ]

# Read in the time and dates and transform to characters.
time2 = as.character(RH520_6hour_prev$TIME)
date2 = as.character(RH520_6hour_prev$DATE)
# Join the time and dates characters together.
date_time2 = paste(date2[1], time2[1], sep = " ")
for (i in c(2:length(date2))) {
  date_time2 = c(date_time2, paste(date2[i], time2[i], sep = " "))
}
# Read the temperature and transform to numbers without losing accuracy.
temperature2 = as.numeric(as.character(RH520_6hour_prev$TEMP))
# Read the humidity and transform to numbers without losing accuracy.
humidity2 = as.numeric(as.character(RH520_6hour_prev$RH)) 
# Time axis for 6 hours previous to the test
time_axis2 = strptime(date_time2, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")
# Set the first time point to 0 and apply the offset to subquent time points.
time_axis2 = time_axis2 - hrs(1)
plot(time_axis2, temperature2, xlab = "Time (Hours).", ylab = "RH520 Temperature (Celsius Degree)", ylim = c(mean(temperature2)-1, mean(temperature2)+1), 
     main = "Plot 1.3.1  RH520 Environment Temperature vs Time\n for 6 hours before test start to end of test.")
paste("Start time: ", time_axis2[1], "End time: ", time_axis2[length(time_axis2)])
plot(time_axis2, humidity2, xlab = "Time (Hours).", ylab = "RH520 Relative Humidity (%)", ylim = c(mean(humidity2)-10, mean(humidity2)+10), 
     main = "Plot 1.3.2  RH520 Environment Humidity vs Time\n for 6 hours before test start to end of test.")
paste("Start time: ", time_axis2[1], "End time: ", time_axis2[length(time_axis2)])
# Plot out the temperature and humidity density in a normalized histogram.
h = hist(temperature2, breaks = 3, plot = FALSE)
h$density = h$counts/length(temperature2)
plot(h, freq = FALSE, xlab = "RH520 Temperature (Celsius Degree)", ylim = c(0,1), main = "Plot 1.3.3  Histogram of RH520 Environment Temperature\n for 6 hours before test start to end of test.")
h = hist(humidity2, breaks = 5, plot = FALSE)
h$density = h$counts/length(humidity2)
plot(h, freq = FALSE, xlab = "RH520 Relative Humidity (%)", ylim = c(0,1), main = "Plot 1.3.4  Histogram of RH520 Environment Humidity\n for 6 hours before test start to end of test.")
# I found that the bin between 53.0 and 53.5 has no height, and I confirmed that there is actually no values within this interval.
summary = matrix(c(round(max(temperature2), digits = 1), round(max(humidity2), digits = 0), round(min(temperature2), digits = 1), round(min(humidity2), digits = 0), 
                   round(max(temperature2)-min(temperature2), digits = 1),
                   round(max(humidity2)-min(humidity2)), round(mean(temperature2), digits = 2), round(mean(humidity2), digits = 1), round(sd(temperature2), digits = 2), round(sd(humidity2), digits = 1), 
                   round(sd(temperature2)/sqrt(length(temperature2)), digits = 2), 
                   round(sd(humidity2)/sqrt(length(humidity2)), digits = 1), length(temperature2), length(humidity2)), ncol = 2, byrow = T)
colnames(summary) = c("temperature", "humidity")
rownames(summary) = c("max", "min", "max - min", "mean", "standard deviation", "SDoM", "sample size")
# Include the information about this table in the file.
info = paste("\n", "(", tableIndex, ")", "# 1.3.5 Statistics for 6 hours before test start to end of test\n")
# Append the information as title
cat(info, file = txt, append = TRUE)
tableIndex = tableIndex + 1
write.table(summary, file = "TE0377-X_OHMS-4W_160915T0931.txt", append = TRUE)
summ = as.table(summary)
print("# 1.3.5 Statistics for 6 hours before test start to end of test")
summ
# RH520_TE0384_9-16-2016_2-20_PM.csv Raw data by start Date/Time: 9/15/2016 , 10:32:00
bystart_bgn_date_time = bgn_date_time
temp_string = as.character(bystart_bgn_date_time)
bystart_bgn_date_time = paste(substr(temp_string, 1, nchar(temp_string)-3), "00", sep = ":")
bystart_bgn = which(bystart_bgn_date_time == rh_date_time)
RH520_bystart = dt1[bystart_bgn:sig_end, ]
temperature3 = as.numeric(as.character(RH520_bystart$TEMP))
humidity3 = as.numeric(as.character(RH520_bystart$RH))
summary = matrix(c(round(max(temperature3), digits = 1), round(max(humidity3), digits = 0), round(min(temperature3), digits = 1), round(min(humidity3), digits = 0), 
                   round(max(temperature3)-min(temperature3), digits = 1),
                   round(max(humidity3)-min(humidity3)), round(mean(temperature3), digits = 2), round(mean(humidity3), digits = 1), round(sd(temperature3), digits = 2), round(sd(humidity3), digits = 1), 
                   round(sd(temperature3)/sqrt(length(temperature3)), digits = 2), 
                   round(sd(humidity3)/sqrt(length(humidity3)), digits = 1), length(temperature3), length(humidity3)), ncol = 2, byrow = T)
colnames(summary) = c("temperature", "humidity")
rownames(summary) = c("max", "min", "max - min", "mean", "standard deviation", "SDoM", "sample size")
info = paste("\n", "(", tableIndex, ")", "# 1.3.6  Statistics for just before test start to just past end of test\n")
cat(info, file = txt, append = TRUE)
tableIndex = tableIndex + 1
write.table(summary, file = "TE0377-X_OHMS-4W_160915T0931.txt", append = TRUE)
summ = as.table(summary)
print("# 1.3.6  Statistics for just before test start to just past end of test")
summ

m = lm(temperature2~(c(-360:(length(temperature2)-361))))
plot(m, which = 4, main = "Plot 1.4  Cook's distance of RH520 Temperature vs\n Sample Number for 6 hours before test start to end of test.")
paste("The x lab is actually the sample number from -360 to 53")
# 2.  Analysis of USB3-06T data.

USB3_06t_full = dt2$Fluke..2
oriLen = nchar(as.character(dt2$Time)[1])
time4 = c(substr(as.character(dt2$Time[1]), 3, 9))
for (i in c(1:length(as.character(dt2$Time)))) {
  if (nchar(as.character(dt2$Time)[i]) == oriLen) {
    time4[i] = substr(as.character(dt2$Time[i]), 3, 9)
  }
  else {
    time4[i] = substr(as.character(dt2$Time[i]), 3, 10)
  }
}
date4 = as.character(dt2$Date)
date_time4 = paste(date4[1], time4[1], sep = " ")
for(i in c(2:length(date4))){
  date_time4 = c(date_time4, paste(date4[i], time4[i], sep = " "))
}
time_axis4 = strptime(date_time4, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
# The time axis for Au_zero, USB3-06T and USB3-06R full data
#time_axis4 = time_axis4 + hrs(12)
# The time axis for Au_zero, USB3-06T and USB3-06R statistically significant data
time_axis3 = time_axis4[6:133]
plot(time_axis4, USB3_06t_full, xlab = "Time (Full Data) (Minutes)", ylab = "USB3-06T Resistance (ohms)", main = "2.1.1 Full data for USB3-06T Resistance vs Time.")
lines(time_axis4, USB3_06t_full)
paste("Start time: ", time_axis4[1], "End time: ", time_axis4[length(time_axis4)])
# 2.2.1 Thermistor Resistance to Temperature conversion.
a = 8.6244827E-4
b = 2.5845615E-4
c = 1.4219472E-7
thermisTemp_full = 1/(a+b*log(USB3_06t_full)+c*(log(USB3_06t_full))^3)-273.15
plot(time_axis4, thermisTemp_full, xlab = "Time (Full Data) (Minutes)", ylab = "USB3-06T Temperature (Celsius Degree)", ylim = c(18,28), main = "2.2.1 Full data for USB3-06T Temperature vs Time.")
paste("Start time: ", time_axis4[1], "End time: ", time_axis4[length(time_axis4)])
USB3_06t_sig = USB3_06t_full[6:133]
thermisTemp_sig = 1/(a+b*log(USB3_06t_sig)+c*(log(USB3_06t_sig))^3)-273.15
sapNum = c(0:(length(USB3_06t_sig)-1))
plot(sapNum, thermisTemp_sig, xlab = "Sample Number.", ylab = "USB3-06T Temperature (Celsius Degree)", ylim = c(mean(thermisTemp_sig)-1, mean(thermisTemp_sig)+1), 
     main = "2.2.2  Statistical data for USB3-06T\n Temperature vs Sample Number.")
# Plot out the normalized histogram of temperature.
h = hist(thermisTemp_full, plot = FALSE)
h$density = h$counts/length(thermisTemp_full)
plot(h, freq = FALSE, xlab = "USB3-06T Temperature (Celsius Degree)", ylim = c(0,1), main = "Plot 2.2.3  Histogram of statistical data for USB3-06T Temperature.")
summary = matrix(c(round(max(thermisTemp_sig), digits = 3), round(min(thermisTemp_sig), digits = 3), round(max(thermisTemp_sig)-min(thermisTemp_sig), digits = 3),
                   round(mean(thermisTemp_sig), digits = 4), round(sd(thermisTemp_sig), digits = 4), 
                   round(sd(thermisTemp_sig)/sqrt(length(thermisTemp_sig)), digits = 4), 
                   length(thermisTemp_sig) ), ncol = 1, byrow = T)
colnames(summary) = c("thermisTemp_sig")
rownames(summary) = c("max", "min", "max - min", "mean", "standard deviation", "SDoM", "sample size")
info = paste("\n", "(", tableIndex, ")", "# 2.2.4  Statistical data for the USB3-06T Temperature.\n")
cat(info, file = txt, append = TRUE)
tableIndex = tableIndex + 1
write.table(summary, file = "TE0377-X_OHMS-4W_160915T0931.txt", append = TRUE)
summ = as.table(summary)
print("# 2.2.4  Statistical data for the USB3-06T Temperature.")
summ
m = lm(thermisTemp_sig~sapNum)
plot(m, which = 4, main = "Plot 2.3  Cook's distance of the statistical data\n for USB3-06T Temperature vs Sample Number.")
paste("x label is Sample Number")

# 3. Cross Correlation between the RH520 and the USB3-06T temperature.
# The time axis for statistically significant data with one point extra.
time_axis5 = time_axis4[6:134]
mean_cadence = (time_axis5[length(time_axis5)] - time_axis5[1]) / (length(time_axis5) - 1)
# Calculate the mean cadence in minutes.
mean_cadence_in_min = as.numeric(mean_cadence)

itpltd_cad = c(time_axis5[1])
# Interpolate the first time point.
new_cad = itpltd_cad - mns(mean_cadence_in_min)
i = 1
# Interpolate all of the following time points.
while(new_cad >= time_axis2[1]){
  new_cad = time_axis5[1] - i*(mns(mean_cadence_in_min))
  itpltd_cad = c(itpltd_cad, new_cad)
  i = i+1
}
itpltd_cad = rev(itpltd_cad)
itpltd_resis = round(rnorm(length(itpltd_cad)-2, mean(USB3_06t_sig), 0.0115), digits = 2)
overall_resis = c(itpltd_resis, USB3_06t_sig)
# sapNum is the sample number with first statistically significant time point set to 0.
sapNum = c(rev(-1 * c(1:length(itpltd_resis))), c(0:(length(USB3_06t_sig)-1)))
# Transform the resistance to temperature.
overall_therm_temp = 1/(a+b*log(overall_resis)+c*(log(overall_resis))^3)-273.15
plot(sapNum, overall_therm_temp, xlab = "Interpolated Sample Number.", ylab = "USB3-06T Temperature (Celsius Degree)", ylim = c(mean(overall_therm_temp)-1, mean(overall_therm_temp)+1), 
     main = "3.1.6  Statistical Data for USB3-06T Temperature with\n previous 6 hours filled in with mean vs Sample Number.")
overall_cad = c(itpltd_cad[1:(length(itpltd_cad)-1)], time_axis5)

f = approxfun(time_axis2 - time_axis2[1], temperature2)
interpolated_TEMP_rh520 = f(overall_cad - time_axis2[1])[2:(length(overall_cad)-1)]
sapNum = c(rev(-1 * c(1:(length(itpltd_cad)-2))), c(0:(length(time_axis5)-2)))
plot(sapNum, interpolated_TEMP_rh520, xlab = "Interpolated Sample Number.", ylab = "Interpolated RH520 Temperature (Celsius Degree)", ylim = c(mean(interpolated_TEMP_rh520)-1, mean(interpolated_TEMP_rh520)+1), 
     main = "3.1.8  Interpolated RH520 Temperature for\n 6 hours before test start to end of test.")
length(interpolated_TEMP_rh520)
length(overall_therm_temp)
ccf(interpolated_TEMP_rh520, overall_therm_temp, lag.max = 100, xlab = "Lag (in sample counts).", 
    main = "3.1.9-B  Cross Corr of Intrpltd RH520 Temp (indpdt vari) vs\n USB3-06T Temp (depdt vari) for 6 hrs bf test start to end")
# This is the plot "3.1.9-B  Cross Correlation of Interpolated RH520 Temperature (independent variable) vs USB3-06T Temperature (dependent variable)\n for 6 hours before test start to end of test."
ccf(interpolated_TEMP_rh520, overall_therm_temp, lag.max = 1000, xlab = "Lag (in sample counts).", 
    main = "3.1.9-B  Cross Corr of Intrpltd RH520 Temp (indpdt vari) vs\n USB3-06T Temp (depdt vari) for 6 hrs bf test start to end")
# This is the plot "3.1.9-B  Cross Correlation of Interpolated RH520\n Temperature (independent variable) vs USB3-06T Temperature (dependent variable)\n for 6 hours before test start to end of test."

# 4. Analysis of the Au_Zero data.
Au_zero_full = as.numeric(as.character(dt2$Fluke..1))
plot(time_axis4, Au_zero_full, xlab = "Time (Minutes)", ylab = "Au_Zero Resistance (ohms)", main = "4.1.1  Au_Zero Full Data vs Time.")
lines(time_axis4, Au_zero_full)
paste("Start time: ", time_axis4[1], "End time: ", time_axis4[length(time_axis4)])
paste("Relatively apparently, the data has an increasing trend")
plot(time_axis4, Au_zero_full, xlab = "Time (Minutes)", ylab = "Au_Zero Resistance (ohms)", ylim = c(-0.25, 0.25), main = "4.1.2 Au_Zero Full Data vs Time.")
paste("Start time: ", time_axis4[1], "End time: ", time_axis4[length(time_axis4)])
Au_zero_sig = Au_zero_full[6:(length(Au_zero_full)-5)]
plot(c(0:(length(Au_zero_sig)-1)), Au_zero_sig, xlab = "Sample Number.", ylab = "Au_Zero Resistance (ohms)",ylim = c(mean(Au_zero_sig)-0.5, mean(Au_zero_sig)+0.5), 
     main = "4.1.3  Statistical data for Au-Zero vs Sample Number.")
h = hist(Au_zero_full, plot = FALSE)
h$density = h$counts/length(Au_zero_full)
plot(h, freq = FALSE, ylim = c(0,1), xlab = "Au_Zero Resistance (ohms)", main = "Plot 4.1.4  Histogram of Statistical Data for Au_Zero.")
summary = matrix(c(round(max(Au_zero_sig), digits = 3), round(min(Au_zero_sig), digits = 3), round(max(Au_zero_sig)-min(Au_zero_sig), digits = 3),
                   round(mean(Au_zero_sig), digits = 4), round(sd(Au_zero_sig), digits = 4), 
                   round(sd(Au_zero_sig)/sqrt(length(Au_zero_sig)), digits = 4), 
                   length(Au_zero_sig)), ncol = 1, byrow = T)
colnames(summary) = c("Au_zero_sig")
rownames(summary) = c("max", "min", "max - min", "mean", "standard deviation", "SDoM", "sample size")
info = paste("\n", "(", tableIndex, ")", "# 4.1.5  Au_Zero Statistical Data\n")
cat(info, file = txt, append = TRUE)
tableIndex = tableIndex + 1
write.table(summary, file = "TE0377-X_OHMS-4W_160915T0931.txt", append = TRUE)
summ = as.table(summary)
print("# 4.1.5  Au_Zero Statistical Data")
summ
m = lm(Au_zero_sig~c(0:(length(Au_zero_sig)-1)))
plot(m, which = 4, main = "4.2  Cook's distance of Statistical data for\n Au_Zero Resistance vs Sample Number.")
paste("The x lab is sample number")
# 4.3 Cross Correlation between RH520 temperature and Au_Zero.
itpltd_Au_0 = round(rnorm(length(itpltd_cad)-2, mean(Au_zero_sig), sd(Au_zero_sig)), digits = 3)
overall_Au_0 = c(itpltd_Au_0, Au_zero_sig)

plot(overall_cad[2:(length(overall_cad)-1)], overall_Au_0, xlab = ("Time (Hours)"), ylim = c(mean(Au_zero_sig)-0.05, mean(Au_zero_sig)+0.05),
     main = "4.3.6  Statistical data for Au_Zero Resistance with previous\n 6 hours filled in with mean vs sample number.")
paste("Start time: ", overall_cad[2], "End time: ", overall_cad[length(overall_cad)-1])
sapNum = c(-1*(length(interpolated_TEMP_rh520)-128):1)
sapNum = c(sapNum, c(0:127))
plot(sapNum, interpolated_TEMP_rh520, ylim = c(mean(interpolated_TEMP_rh520) - 1, mean(interpolated_TEMP_rh520) + 1), xlab = "Sample Number",
     main = "4.3.8  Interpolated RH520 Temperature for\n 6 hours before test start to end of test.")

ccf(interpolated_TEMP_rh520, overall_Au_0, lag.max = 100, xlab = "Lag (in sample counts).", 
    main = "4.3.9-B  Cross Corr of Intrpltd RH520 Temp (indpdt vari)\n vs Au_Zero Resis (depdt vari) for 6 hrs bf test start to end")
# This is the plot "4.3.9-B  Cross Correlation of Interpolated RH520 Temperature (independent variable) vs Au_Zero Resistance (dependent variable) for 6 hours before test start to end of test."
ccf(interpolated_TEMP_rh520, overall_Au_0, lag.max = 1000, xlab = "Lag (in sample counts).", 
    main = "4.3.9  Cross Corr of Intrpltd RH520 Temp (indpdt vari)\n vs Au_Zero Resis (depdt vari) for 6 hrs bf test start to end")
# This is the plot "4.3.9  Cross Correlation of Interpolated RH520 Temperature (independent variable) vs Au_Zero Resistance (dependent variable) for 6 hours before test start to end of test."

ccf(USB3_06t_sig, Au_zero_sig, lag.max = 128, xlab = "Lag (in sample counts).", main = "4.4.1  Cross Corr of stat data for USB3-06T Temp\n (indept vari) vs Au_Zero Resis (dept vari).")
# This is the plot "4.4.1  Cross Correlation of statistical data for USB3-06T Temperature (independent variable) vs Au_Zero Resistance (dependent variable)."

time_axis6 = time_axis4[5:134]
# f = approxfun(time_axis3, Au_zero_sig)
itpltd_TEMP_rh520_sig = interpolated_TEMP_rh520[(length(interpolated_TEMP_rh520)-127):(length(interpolated_TEMP_rh520))]
# f(time_axis6)[2:(length(time_axis6)-1)]
plot(itpltd_TEMP_rh520_sig, Au_zero_sig, xlab = "Interpolated RH520 Temperature (Celsius Degree)", ylab = "Au_Zero Resistance (ohms)", 
     main = "4.5.1  Intrpltd RH520 Temp (indepdt vari) vs stat data for Au_Zero Resis (depdt vari).")
# This is the plot "4.5.1  Interpolated RH520 Temperature (independent variable) vs statistical data for Au_Zero Resistance (dependent variable)."
plot(thermisTemp_sig, Au_zero_sig, xlab = "USB3-06T Temperature (Celsius Degree)", ylab = "Au_Zero Resistance (ohms)", 
     main = "4.5.2  Stat data for USB3-06T Temp (indepdt vari)\n vs Au_Zero Resist (depdt vari).")
# 4.5.2  Statistical data for USB3-06T Temperature (independent variable)\n vs Au_Zero Resistance (dependent variable).

# 5. Analysis of USB3-06R 10 kohms standard resistor.
USB3_06R_full = as.numeric(as.character(dt2$Fluke..3))
plot(time_axis4, USB3_06R_full, xlab = "Time (Minutes)", ylab = "USB3-06R Resistance (ohms)", main = "5.1.1 Full Data for USB3-06R Resistance vs Time.")
lines(time_axis4, USB3_06R_full)
paste("Start time: ", time_axis4[1], "End time: ", time_axis4[length(time_axis4)])
# The first point is significantly higher than others. I did a check that found that in the data file it is the truth, so no error in plotting.
plot(time_axis4, USB3_06R_full, xlab = "Time (Minutes)", ylab = "USB3-06R Resistance (ohms)", ylim = c(9999.650, 9999.850), main = "5.1.2  Full Data for USB3-06R Resistance vs Time.")
paste("Start time: ", time_axis4[1], "End time: ", time_axis4[length(time_axis4)])
USB3_06R_sig = USB3_06R_full[6:133]
plot(c(0:(length(USB3_06R_sig)-1)), USB3_06R_sig, xlab = "Sample Number.", ylab = "USB3-06R Resistance (ohms)", ylim = c(mean(USB3_06R_sig)-0.1, mean(USB3_06R_sig)+0.1), 
     main = "5.1.3  Statistical Data for USB3-06R Resistance  vs Sample Number.")
h = hist(USB3_06R_full, plot = FALSE)
h$density = h$counts/length(USB3_06R_full)
plot(h, freq = FALSE, xlab = "USB3-06R Resistance (ohms)", ylim = c(0,1), main = "Plot 5.1.4  Histogram of USB3-06R Resistance, Full Data.")
# The reason we have two empty bins is that one point is significantly higher than others, leaving the intermediate values blank.
summary = matrix(c(round(max(USB3_06R_sig), digits = 3), round(min(USB3_06R_sig), digits = 3), round(max(USB3_06R_sig)-min(USB3_06R_sig), digits = 3),
                   round(mean(USB3_06R_sig), digits = 4), round(sd(USB3_06R_sig), digits = 4), 
                   round(sd(USB3_06R_sig)/sqrt(length(USB3_06R_sig)), digits = 4), 
                   length(USB3_06R_sig) ), ncol = 1, byrow = T)
colnames(summary) = c("USB3_06R_sig")
rownames(summary) = c("max", "min", "max - min", "mean", "standard deviation", "SDoM", "sample size")
info = paste("\n", "(", tableIndex, ")", "# 5.1.5 USB3-06R Statistical Data\n")
cat(info, file = txt, append = TRUE)
tableIndex = tableIndex + 1
write.table(summary, file = "TE0377-X_OHMS-4W_160915T0931.txt", append = TRUE)
summ = as.table(summary)
print("# 5.1.5 USB3-06R Statistical Data")
summ
m = lm(USB3_06R_sig~c(1:length(USB3_06R_sig)))
plot(m, which = 4, main = "5.2  Cook's distance of Statistical data\n for USB3-06R Resistance vs Sample Number.e")
paste("the x lab is Sample Number")
# 5.3 Cross Correlation between RH520 Temperature and USB3-06R.
itpltd_USB3_06R = round(rnorm(length(itpltd_cad)-2, mean(USB3_06R_sig), sd(USB3_06R_sig)), digits = 3)
overall_USB3_06R = c(itpltd_USB3_06R, USB3_06R_sig)
plot(overall_cad[2:(length(overall_cad)-1)], overall_USB3_06R, xlab = "Time (Hours)", ylab = "5.3.6  USB3-06R Resistance (ohms).", ylim = c(mean(USB3_06R_sig)-0.05, mean(USB3_06R_sig)+0.05), 
     main = "5.3.6  Statistical data for USB3-06R Resistance with previous\n 6 hours filled in with mean vs time.")
lines(overall_cad[2:(length(overall_cad)-1)], overall_USB3_06R)
paste("Start time: ", overall_cad[2], "End time: ", overall_cad[length(overall_cad)-1])
sapNum = -1 * c(1:(length(interpolated_TEMP_rh520)-128))
sapNum = rev(sapNum)
sapNum = c(sapNum, c(0:127))
plot(sapNum, interpolated_TEMP_rh520, ylim = c(mean(interpolated_TEMP_rh520)-1, mean(interpolated_TEMP_rh520)+1), xlab = "Interpolated Sample Number.", ylab = "Interpolated RH520 Temperature (Celsius Degree)", 
     main = "5.3.8  Interpolated RH520 Temperature for 6\n hours before test start to end of test.")
ccf(interpolated_TEMP_rh520, overall_USB3_06R, lag.max = 100, xlab = "Lag (in sample counts).", 
    main = "5.3.9-B  Cross Corr of Intrpltd RH520 Temp (indept vari)\n vs USB3-06R Resis (dept vari) for 6 hrs bf test start to end of test.")
paste("5.3.9-B  Cross Correlation of Interpolated RH520 Temperature (independent variable) vs USB3-06R Resistance (dependent variable) for 6 hours before test start to end of test.")
ccf(interpolated_TEMP_rh520, overall_USB3_06R, lag.max = 1000, xlab = "Lag (in sample counts).", 
    main = "5.3.9-B  Cross Corr of Intrpltd RH520 Temp (indept vari)\n vs USB3-06R Resis (dept vari) for 6 hrs bf test start to end of test.")
paste("5.3.9-B  Cross Correlation of Interpolated RH520 Temperature (independent variable) vs USB3-06R Resistance (dependent variable) for 6 hours before test start to end of test.")

# 5.4 Cross Correlation between USB3-06T and USB-06R.
ccf(thermisTemp_sig, USB3_06R_sig, lag.max = 128, xlab = "Lag (in sample counts).", 
    main = "5.4.1  Cross Correlation of statistical data for USB3-06T Temperature\n (independent variable) vs USB3-06R Resistance (dependent variable).")
plot(interpolated_TEMP_rh520[(length(interpolated_TEMP_rh520)-127):(length(interpolated_TEMP_rh520))], USB3_06R_sig, xlab = "Interpolated RH520 Temperature (Celsius Degree)", ylab = "USB3-06R Resistance (ohms)", 
     main = "5.5.1  Interpolated RH520 Temperature (independent variable)\n vs statistical data for USB3-06R Resistance (dependent variable).")
plot(thermisTemp_sig, USB3_06R_sig, xlab = "USB3-06T Temperature (Celsius Degree)", ylab = "USB3-06R Resistance (ohms)", 
     main = "5.5.2  Statistical data for USB3-06T Temperature (independent variable)\n vs USB3-06R Resistance (dependent variable).")

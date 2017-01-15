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

dt1 = read.csv("U1252A_TE0320_160413T133520-160413T143523.csv", skip = 1, header = T)
for (i in c(ncol(dt1):2)) {
  dt1[, i] = dt1[, i-1]
}
dt1[, 1] = c(1:nrow(dt1))
dt1$Time = strptime(dt1$Time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
dt1$Primary.function = as.character(dt1$Primary.function)
dt1$Primary.range = as.character(dt1$Primary.range)
dt1$Secondary.function = as.character(dt1$Secondary.function)
dt1$Secondary.range = as.character(dt1$Secondary.range)

dt2 = read.csv("U1252A_TE0320_160413T143524-160413T153528.csv", skip = 1, header = T)
for (i in c(ncol(dt2):2)) {
  dt2[, i] = dt2[, i-1]
}
dt2[, 1] = c(1:nrow(dt2))
dt2$Time = strptime(dt2$Time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
dt2$Primary.function = as.character(dt2$Primary.function)
dt2$Primary.range = as.character(dt2$Primary.range)
dt2$Secondary.function = as.character(dt2$Secondary.function)
dt2$Secondary.range = as.character(dt2$Secondary.range)

dt3 = read.csv("U1252A_TE0320_160413T153529-160413T163531.csv", skip = 1, header = T)
for (i in c(ncol(dt3):2)) {
  dt3[, i] = dt3[, i-1]
}
dt3[, 1] = c(1:nrow(dt3))
dt3$Time = strptime(dt3$Time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
dt3$Primary.function = as.character(dt3$Primary.function)
dt3$Primary.range = as.character(dt3$Primary.range)
dt3$Secondary.function = as.character(dt3$Secondary.function)
dt3$Secondary.range = as.character(dt3$Secondary.range)

dt4 = read.csv("U1252A_TE0320_160413T163532-160413T171736.csv", skip = 1, header = T)
for (i in c(ncol(dt4):2)) {
  dt4[, i] = dt4[, i-1]
}
dt4[, 1] = c(1:nrow(dt4))
dt4$Time = strptime(dt4$Time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
dt4$Primary.function = as.character(dt4$Primary.function)
dt4$Primary.range = as.character(dt4$Primary.range)
dt4$Secondary.function = as.character(dt4$Secondary.function)
dt4$Secondary.range = as.character(dt4$Secondary.range)

dt = data.frame(c( 1:(nrow(dt1)+nrow(dt2)+nrow(dt3)+nrow(dt4)) ))
names(dt)[1] = paste("Records")
dt$Time = c(dt1$Time, dt2$Time, dt3$Time, dt4$Time)
dt$Primary.function = c(dt1$Primary.function, dt2$Primary.function, dt3$Primary.function, dt4$Primary.function)
dt$Primary.reading = c(dt1$Primary.reading, dt2$Primary.reading, dt3$Primary.reading, dt4$Primary.reading)
dt$Primary.range = c(dt1$Primary.range, dt2$Primary.range, dt3$Primary.range, dt4$Primary.range)
dt$Primary.resolution = c(dt1$Primary.resolution, dt2$Primary.resolution, dt3$Primary.resolution, dt4$Primary.resolution)
dt$Secondary.function = c(dt1$Secondary.function, dt2$Secondary.function, dt3$Secondary.function, dt4$Secondary.function)
dt$Secondary.reading = c(dt1$Secondary.reading, dt2$Secondary.reading, dt3$Secondary.reading, dt4$Secondary.reading)
dt$Secondary.range = c(dt1$Secondary.range, dt2$Secondary.range, dt3$Secondary.range, dt4$Secondary.range)
dt$Secondary.resolution = c(dt1$Secondary.resolution, dt2$Secondary.resolution, dt3$Secondary.resolution, dt4$Secondary.resolution)

dt$Time = dt$Time+mns(12)+42

a = 1.0165132938E-03
b = 2.4158719804E-04
c = 1.4441812532E-07
dt$Thermistor.temp = 1 / (a + b * log(dt$Primary.reading) + c * (log(dt$Primary.reading)) ^ 3) - 273.15

Agilent = dt

start = "04/13/2016 13:31:19"
start = strptime(start, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
end = "04/13/2016 17:05:27"
end = strptime(end, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
diff = end - start

dt = read.delim2("Thermal_Box_Data_160413T134401-160413T171809.txt", skip = 5, header = T)
rows2dlt = c((nrow(dt) - 2):nrow(dt))
dt = dt[-rows2dlt, ]
cadence = diff / (nrow(dt) - 1)
x = c(1:nrow(dt))
TScomp = c(start + cadence * 9)
k = 1
l = 1
while (k + 20 < nrow(dt)){
  l = l + 1
  k = k + 20
  TScomp[l] = TScomp[l - 1] + cadence * 20
}

TScomp = TScomp + mns(12) + 42
TScomp = round(TScomp, units = "secs")

a = 1
b = 1
gTsCnt = as.numeric(as.character(dt$gTsCnt))
gTsCnt.cmp = c(mean(gTsCnt[a:(a + 19)]))
fgTa1 = as.numeric(as.character(dt$fgTa1))
fgTa1.cmp = c(mean(fgTa1[a:(a + 19)]))
fgTa3 = as.numeric(as.character(dt$fgTa3))
fgTa3.cmp = c(mean(fgTa3[a:(a + 19)]))
fgTr = as.numeric(as.character(dt$fgTr))
fgTr.cmp = c(mean(fgTr[a:(a + 19)]))

while (a + 19 < length(gTsCnt)) {
  b = b + 1
  a = a + 20
  gTsCnt.cmp[b] = mean(gTsCnt[a:(a + 19)])
  fgTa1.cmp[b] = mean(fgTa1[a:(a + 19)])
  fgTa3.cmp[b] = mean(fgTa3[a:(a + 19)])
  fgTr.cmp[b] = mean(fgTr[a:(a + 19)])
}
gTsCnt.cmp[b] = mean(gTsCnt[a:length(gTsCnt)])
fgTa1.cmp[b] = mean(fgTa1[a:length(fgTa1)])
fgTa3.cmp[b] = mean(fgTa3[a:length(fgTa3)])
fgTr.cmp[b] = mean(fgTr[a:length(fgTr)])

Thermal_box_data = data.frame(TScomp, gTsCnt.cmp, fgTa1.cmp, fgTa3.cmp, fgTr.cmp)

dt = read.delim("RH520_295832_160316T122747-160415T194557.txt", header = TRUE)
dt = dt[-1, ]
dateTime = paste(dt$DATE, dt$TIME, sep = " ")
dateTime = strptime(dateTime, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")
dateTime = dateTime - hrs(6) - 47
dt2 = read.delim("AERI_R_Box_160413T150653-160413T171439.txt", skip = 4, header = TRUE, sep = ",")
aeri_date_time.str = paste(dt2$Date, dt2$Time, sep = " ")
aeri_date_time = strptime(aeri_date_time.str, format = "%m/%d/%Y %H:%M:%S", tz = "America/Chicago")
for (i in c(1:length(aeri_date_time.str))) {
  morn_after = strsplit(aeri_date_time.str[i], ' ')[[1]][6]
  if(morn_after == "PM") { 
    aeri_date_time[i] = aeri_date_time[i] + hrs(12)
  }
}
start = as.character(aeri_date_time[1])
sec = as.numeric(strsplit(start, ':')[[1]][3])
min = as.numeric(strsplit(start, ':')[[1]][2])
if (sec < 13) {
  min = min - 1
}
start = paste(strsplit(start, ':')[[1]][1], min, "13", sep = ":")
start = strptime(start, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")
stInd = which(start == dateTime)
end = as.character(aeri_date_time[nrow(dt2)])
sec = as.numeric(strsplit(end, ':')[[1]][3])
min = as.numeric(strsplit(end, ':')[[1]][2])
if (sec > 13) {
  min = min + 1
}
end = paste(strsplit(end, ':')[[1]][1], min, "13", sep = ":")
end = strptime(end, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")
edInd = which(end == dateTime)
six_hr_bf = which((start-hrs(6)) == dateTime)
temp = as.numeric(as.character(dt$TEMP))
hmdt = as.numeric(as.character(dt$RH))
plot(dateTime[six_hr_bf:edInd], temp[six_hr_bf:edInd], xlab = "Time (Hours)", ylab = "RH520 Temperature (Celsius Degree)", 
     main = "RH520 Environment Temperature vs Time\n for 6 hours before test start to end of test.")
plot(dateTime[six_hr_bf:edInd], hmdt[six_hr_bf:edInd], xlab = "Time (Hours)", ylab = "RH520 Humidity (%)",
     main = "RH520 Environment Humidity vs Time\n for 6 hours before test start to end of test.")

half_hr_bf = aeri_date_time[1] - hrs(0.5)
AgiStr = which(half_hr_bf == Agilent$Time)
AgiEnd = which(aeri_date_time[nrow(dt2)] == Agilent$Time)
plot(Agilent$Time[AgiStr:AgiEnd], Agilent$Thermistor.temp[AgiStr:AgiEnd], xlab = "Time (Hours)", 
     ylab = "Agilent Thermistor Temperature (Celsius Degree)",
     main = "Calibrated Agilent Thermistor temperature vs Time \n for 1/2 hours before test starts to end of test")
plot(Agilent$Time[AgiStr:AgiEnd], Agilent$Secondary.reading[AgiStr:AgiEnd], xlab = "Time (Hours)", 
     ylab = "Internal Temperature (Celsius Degree)", 
     main = "Internal Temperature vs Time\n for 1/2 hours before tests starts to end of test")

SCstr = which(half_hr_bf == Thermal_box_data$TScomp)
SCend = which(aeri_date_time[nrow(dt2)] == Thermal_box_data$TScomp)
plot(Thermal_box_data$TScomp[SCstr:SCend], Thermal_box_data$fgTa1.cmp[SCstr:SCend], xlab = "Time (Hours)", 
     ylab = "Control Temperature (Celsius Degree)", 
     main = "Control Temperature vs Time\n for 1/2 hours before tests starts to end of test")
plot(Thermal_box_data$TScomp[SCstr:SCend], Thermal_box_data$fgTa3.cmp[SCstr:SCend], xlab = "Time (Hours)", 
     ylab = "Ambient Temperature (Celsius Degree)", 
     main = "Ambient Temperature vs Time\n for 1/2 hours before tests starts to end of test")
plot(Thermal_box_data$TScomp[SCstr:SCend], Thermal_box_data$fgTr.cmp[SCstr:SCend], xlab = "Time (Hours)", 
     ylab = "Setpoint Temperature (Celsius Degree)", 
     main = "Setpoint Temperature vs Time\n for 1/2 hours before tests starts to end of test")

plot(aeri_date_time, dt2$Fluke..1, xlab = "Time (Hours)", ylab = "Au_Zero Resistance (ohms)",  main = "Au_Zero Full Data vs Time.")
plot(aeri_date_time, dt2$Fluke..2, xlab = "Time (Hours)", ylab = "USB3-06T Resistance (ohms)", main = "Full data for USB3-06T Resistance vs Time.")
plot(aeri_date_time, dt2$Fluke..3, xlab = "Time (Hours)", ylab = "USB3-06T Resistance (ohms)", main = "Full data for USB3-06R Resistance vs Time.")
plot(aeri_date_time, dt2$Fluke..4, xlab = "Time (Hours)", ylab = "HBB RT1 (2.76K ohms)",  main = "HBB RT1 Full Data vs Time.")
plot(aeri_date_time, dt2$Fluke..5, xlab = "Time (Hours)", ylab = "HBB RT2 (9.90K ohms)",  main = "HBB RT2 Full Data vs Time.")
plot(aeri_date_time, dt2$Fluke..6, xlab = "Time (Hours)", ylab = "HBB RT3 (12.5K ohms)",  main = "HBB RT3 Full Data vs Time.")
plot(aeri_date_time, dt2$Fluke..7, xlab = "Time (Hours)", ylab = "ABB RT1 (99.0K ohms)",  main = "ABB RT1 Full Data vs Time.")
plot(aeri_date_time, dt2$Fluke..8, xlab = "Time (Hours)", ylab = "ABB RT2 (125K ohms)",  main = "ABB RT2 Full Data vs Time.")

# plot(aeri_date_time[6:(nrow(dt2)-5)], dt2$Fluke..1[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "Au_Zero Resistance (ohms)", 
#      main = "Statistical Data for Au_Zero vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), dt2$Fluke..1[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "Au_Zero Resistance (ohms)")
m1 = lm(dt2$Fluke..1[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m1)
formula1 = paste("y =", coef(m1)[1], "+", coef(m1)[2], "* x", sep = " ")
paste("The formula of this model is", formula1, sep = " ")

# plot(aeri_date_time[6:(nrow(dt2)-5)], dt2$Fluke..2[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "USB3-06T Temperature (Celsius Degree)", 
#      main = "Statistical data for USB3-06T Resistance vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), dt2$Fluke..2[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "USB3-06T Resistance (ohms)", main = "Statistical data for USB3-06T Resistance vs Time.")
m2 = lm(dt2$Fluke..2[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m2)
formula2 = paste("y =", coef(m2)[1], "+", coef(m2)[2], "* x", sep = " ")
paste("The formula of this model is", formula2, sep = " ")

a = 8.6244827E-4
b = 2.5845615E-4
c = 1.4219472E-7
Therm.temp = 1 / (a + b * log(dt2$Fluke..2) + c * (log(dt2$Fluke..2)) ^ 3) - 273.15

# plot(aeri_date_time[6:(nrow(dt2)-5)], Therm.temp[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "USB3-06T Thermistor Temperature (Celsius Degree)", 
#      main = "Statistical data for USB3-06T Thermistor Temperature vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), Therm.temp[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "USB3-06T Thermistor Temperature (Celsius Degree)", main = "Statistical data for USB3-06T Thermistor Temperature vs Time.")
m3 = lm(Therm.temp[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m3)
formula3 = paste("y =", coef(m3)[1], "+", coef(m3)[2], "* x", sep = " ")
paste("The formula of this model is", formula3, sep = " ")

# plot(aeri_date_time[6:(nrow(dt2)-5)], dt2$Fluke..3[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "USB3-06R Resistance (ohms)", 
#      main = "Statistical data for USB3-06R Resistance vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), dt2$Fluke..3[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "USB3-06R Resistance (ohms)", main = "Statistical data for USB3-06R Resistance vs Time.")
m4 = lm(dt2$Fluke..3[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m4)
formula4 = paste("y =", coef(m4)[1], "+", coef(m4)[2], "* x", sep = " ")
paste("The formula of this model is", formula4, sep = " ")

# plot(aeri_date_time[6:(nrow(dt2)-5)], dt2$Fluke..4[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "HBB RT1 (2.76K ohms)", main = "HBB RT1 Statistical Data vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), dt2$Fluke..4[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "HBB RT1 (2.76K ohms)", main = "HBB RT1 Statistical Data vs Time.")
m5 = lm(dt2$Fluke..4[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m5)
formula5 = paste("y =", coef(m5)[1], "+", coef(m5)[2], "* x", sep = " ")
paste("The formula of this model is", formula5, sep = " ")

# plot(aeri_date_time[6:(nrow(dt2)-5)], dt2$Fluke..5[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "HBB RT2 (9.90K ohms)", main = "HBB RT2 Statistical Data vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), dt2$Fluke..5[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "HBB RT2 (9.90K ohms)", main = "HBB RT2 Statistical Data vs Time.")
m6 = lm(dt2$Fluke..5[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m6)
formula6 = paste("y =", coef(m6)[1], "+", coef(m6)[2], "* x", sep = " ")
paste("The formula of this model is", formula6, sep = " ")

# plot(aeri_date_time[6:(nrow(dt2)-5)], dt2$Fluke..6[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "HBB RT3 (12.5K ohms)", main = "HBB RT3 Statistical Data vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), dt2$Fluke..6[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "HBB RT3 (12.5K ohms)", main = "HBB RT3 Statistical Data vs Time.")
m7 = lm(dt2$Fluke..6[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m7)
formula7 = paste("y =", coef(m7)[1], "+", coef(m7)[2], "* x", sep = " ")
paste("The formula of this model is", formula7, sep = " ")

# plot(aeri_date_time[6:(nrow(dt2)-5)], dt2$Fluke..7[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "ABB RT1 (99.0K ohms)", main = "ABB RT1 Statistical Data vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), dt2$Fluke..7[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "ABB RT1 (99.0K ohms)", main = "ABB RT1 Statistical Data vs Time.")
m8 = lm(dt2$Fluke..7[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m8)
formula8 = paste("y =", coef(m8)[1], "+", coef(m8)[2], "* x", sep = " ")
paste("The formula of this model is", formula8, sep = " ")

# plot(aeri_date_time[6:(nrow(dt2)-5)], dt2$Fluke..8[6:(nrow(dt2)-5)], xlab = "Time (Hours)", ylab = "ABB RT2 (125K ohms)", main = "ABB RT2 Statistical Data vs Time.")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), dt2$Fluke..8[6:(nrow(dt2)-5)], xlab = "Elapsed time in seconds", 
     ylab = "ABB RT2 (125K ohms)", main = "ABB RT2 Statistical Data vs Time.")
m9 = lm(dt2$Fluke..8[6:(nrow(dt2)-5)] ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m9)
formula9 = paste("y =", coef(m9)[1], "+", coef(m9)[2], "* x", sep = " ")
paste("The formula of this model is", formula9, sep = " ")
x = as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6])
y = dt2$Fluke..8[6:(nrow(dt2)-5)]
m10 = lm(log(y) ~ log(x+1))
lines(x, exp(predict(m10)))

statst = aeri_date_time[6]
stated = aeri_date_time[nrow(dt2)-5]
AgiStr = which(statst == Agilent$Time)
AgiEnd = which(stated == Agilent$Time)
f = approxfun(Agilent$Time[AgiStr:AgiEnd] - Agilent$Time[AgiStr], Agilent$Thermistor.temp[AgiStr:AgiEnd])
Intrpl_ther_temp = f(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6])
# plot(aeri_date_time[6:(nrow(dt2)-5)], Intrpl_ther_temp, xlab = "Time (Minutes)", ylab = "Interpolated thermistor temperature (Celsius Degree)",
#      main = "Interpolated thermistor temperature on the statistically significant testing time")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), Intrpl_ther_temp, xlab = "Elapsed time in seconds", 
     ylab = "Interpolated controlled temperature (Celsius Degree)")
x = as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6])
fit = lm(log(Intrpl_ther_temp - min(Intrpl_ther_temp) + 0.1) ~ x)
srt = list(a = exp(coef(fit)[1]), b = coef(fit)[2], c = min(Intrpl_ther_temp) * 0.5)
m11 = nls(Intrpl_ther_temp ~ a*exp(b*x)+c, start = srt)
lines(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), predict(m11))
formula11 = paste("y =", coef(m11)[1], "* exp(", coef(m11)[2], "* x)", "+", coef(m11)[3], sep = " ")
paste("The formula of this model is", formula11, sep = " ")

SCstr = which(statst == Thermal_box_data$TScomp)
SCend = which(stated == Thermal_box_data$TScomp)
f = approxfun(Thermal_box_data$TScomp[SCstr:SCend] - Thermal_box_data$TScomp[SCstr], Thermal_box_data$fgTa1.cmp[SCstr:SCend])
intrpl_control_temp = f(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6])
# plot(aeri_date_time[6:(nrow(dt2)-5)], intrpl_control_temp, xlab = "Time (Minutes)", ylab = "Interpolated controlled temperature (Celsius Degree)",
#      main = "Interpolated controlled temperature\n on the statistically significant testing time")
plot(as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]), intrpl_control_temp, xlab = "Elapsed time in seconds", 
     ylab = "Interpolated controlled temperature (Celsius Degree)")
m12 = lm(intrpl_control_temp ~ as.numeric(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6]))
abline(reg = m12)
formula12 = paste("y =", coef(m12)[1], "+", coef(m12)[2], "* x", sep = " ")
paste("The formula of this model is", formula12, sep = " ")

f = approxfun(Thermal_box_data$TScomp[SCstr:SCend] - Thermal_box_data$TScomp[SCstr], Thermal_box_data$fgTa3.cmp[SCstr:SCend])
intrpl_ambient_temp = f(aeri_date_time[6:(nrow(dt2)-5)] - aeri_date_time[6])
plot(aeri_date_time[6:(nrow(dt2)-5)], intrpl_ambient_temp, xlab = "Time (Minutes)", ylab = "Interpolated ambient temperature (Celsius Degree)",
     main = "Interpolated ambient temperature\n on the statistically significant testing time")
n = 2
left = which(aeri_date_time[(6 - n)] == Thermal_box_data$TScomp)
right = which(aeri_date_time[(nrow(dt2) - 5 + n)] == Thermal_box_data$TScomp)
f = approxfun(Thermal_box_data$TScomp[left:right] - Thermal_box_data$TScomp[left], Thermal_box_data$fgTa3.cmp[left:right])
y = f(aeri_date_time[(6 - n):(nrow(dt2) - 5 + n)] - aeri_date_time[6 - n])
z = c(mean(y[1:(2*n+1)]))
for (i in c((n+2):(length(y)-n))) {
  z = c(z, mean(y[(i-n):(i+n)]))
}
lines(aeri_date_time[6:(nrow(dt2)-5)], z)
paste("In this plot, I used 5 point moving average line. I tried with 7, but it still doesn't look smooth and off away from many points.")

summary = matrix(c(round(max(dt2$Fluke..1), digits = 3), round(max(dt2$Fluke..2), digits = 3), round(max(dt2$Fluke..3), digits = 3), 
                   round(max(dt2$Fluke..4), digits = 3), round(max(dt2$Fluke..5), digits = 3), round(max(dt2$Fluke..6), digits = 3), 
                   round(max(dt2$Fluke..7), digits = 3), round(max(dt2$Fluke..8), digits = 3), round(max(Intrpl_ther_temp), digits = 3), 
                   round(max(intrpl_control_temp), digits = 3), round(max(intrpl_ambient_temp), digits = 3),
                   
                   round(min(dt2$Fluke..1), digits = 3), round(min(dt2$Fluke..2), digits = 3), round(min(dt2$Fluke..3), digits = 3),
                   round(min(dt2$Fluke..4), digits = 3), round(min(dt2$Fluke..5), digits = 3), round(min(dt2$Fluke..6), digits = 3),
                   round(min(dt2$Fluke..7), digits = 3), round(min(dt2$Fluke..8), digits = 3), round(min(Intrpl_ther_temp), digits = 3),
                   round(min(intrpl_control_temp), digits = 3), round(min(intrpl_ambient_temp), digits = 3),
                   
                   round(max(dt2$Fluke..1) - min(dt2$Fluke..1), digits = 3), round(max(dt2$Fluke..2) - min(dt2$Fluke..2), digits = 3),
                   round(max(dt2$Fluke..3) - min(dt2$Fluke..3), digits = 3), round(max(dt2$Fluke..4) - min(dt2$Fluke..4), digits = 3),
                   round(max(dt2$Fluke..5) - min(dt2$Fluke..5), digits = 3), round(max(dt2$Fluke..6) - min(dt2$Fluke..6), digits = 3),
                   round(max(dt2$Fluke..7) - min(dt2$Fluke..7), digits = 3), round(max(dt2$Fluke..8) - min(dt2$Fluke..8), digits = 3),
                   round(max(Intrpl_ther_temp) - min(Intrpl_ther_temp), digits = 3), 
                   round(max(intrpl_control_temp) - min(intrpl_control_temp), digits = 3),
                   round(max(intrpl_ambient_temp) - min(intrpl_ambient_temp), digits = 3),
                   
                   round(mean(dt2$Fluke..1), digits = 4), round(mean(dt2$Fluke..2), digits = 4), round(mean(dt2$Fluke..3), digits = 4), 
                   round(mean(dt2$Fluke..4), digits = 4), round(mean(dt2$Fluke..5), digits = 4), round(mean(dt2$Fluke..6), digits = 4), 
                   round(mean(dt2$Fluke..7), digits = 4), round(mean(dt2$Fluke..8), digits = 4), round(mean(Intrpl_ther_temp), digits = 4), 
                   round(mean(intrpl_control_temp), digits = 4), round(mean(intrpl_ambient_temp), digits = 4), 
                   
                   round(sd(dt2$Fluke..1), digits = 4), round(sd(dt2$Fluke..2), digits = 4), round(sd(dt2$Fluke..3), digits = 4), 
                   round(sd(dt2$Fluke..4), digits = 4), round(sd(dt2$Fluke..5), digits = 4), round(sd(dt2$Fluke..6), digits = 4), 
                   round(sd(dt2$Fluke..7), digits = 4), round(sd(dt2$Fluke..8), digits = 4), round(sd(Intrpl_ther_temp), digits = 4), 
                   round(sd(intrpl_control_temp), digits = 4), round(sd(intrpl_ambient_temp), digits = 4), 
                   
                   round(sd(dt2$Fluke..1)/sqrt(length(dt2$Fluke..1)), digits = 4), round(sd(dt2$Fluke..2)/sqrt(length(dt2$Fluke..2)), digits = 4), 
                   round(sd(dt2$Fluke..3)/sqrt(length(dt2$Fluke..3)), digits = 4), round(sd(dt2$Fluke..4)/sqrt(length(dt2$Fluke..4)), digits = 4), 
                   round(sd(dt2$Fluke..5)/sqrt(length(dt2$Fluke..5)), digits = 4), round(sd(dt2$Fluke..6)/sqrt(length(dt2$Fluke..6)), digits = 4), 
                   round(sd(dt2$Fluke..7)/sqrt(length(dt2$Fluke..7)), digits = 4), round(sd(dt2$Fluke..8)/sqrt(length(dt2$Fluke..8)), digits = 4), 
                   round(sd(Intrpl_ther_temp)/sqrt(length(Intrpl_ther_temp)), digits = 4), 
                   round(sd(intrpl_control_temp)/sqrt(length(intrpl_control_temp)), digits = 4), 
                   round(sd(intrpl_ambient_temp)/sqrt(length(intrpl_ambient_temp)), digits = 4), 
                   
                   length(dt2$Fluke..1), length(dt2$Fluke..2), length(dt2$Fluke..3), length(dt2$Fluke..4), length(dt2$Fluke..5), length(dt2$Fluke..6),
                   length(dt2$Fluke..7), length(dt2$Fluke..8), length(Intrpl_ther_temp), length(intrpl_control_temp), length(intrpl_ambient_temp),
                   
                   formula1, formula2, formula3, formula4, formula5, formula6, formula7, formula8, formula9, formula11, formula12), ncol = 11, byrow = T)
colnames(summary) = c("Au_zero", "USB3-06T Resistance", "USB3-06T Temperature", "HBB RT1", "HBB RT2", "HBB RT3", "ABB RT1", "ABB RT2", 
                      "Interpolated thermistor temperature", "Interpolated controlled temperature", "Interpolated ambient temperature")
rownames(summary) = c("max", "min", "max - min", "mean", "standard deviation", "SDoM", "sample size", "formula")
summ = as.table(summary)
cat("Summaries: \n", file = "BB_Cal_Box_Cal.txt")
write.table(summ, file = "BB_Cal_Box_Cal.txt", append = TRUE)

for(i in c(1:ncol(summ))) {
  print(summ[, i, drop = F])
  print('\n')
}

paste("Please manually change the name of the output files.")
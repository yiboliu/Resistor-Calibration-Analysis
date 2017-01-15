# A function to read in user inputs. 
readString <- function(pmt)
{ 
  n <- readline(prompt = pmt)
  n = paste(n, "\n", sep = "")
  return(as.character(n))
}

Versionpmt = "Please enter the version number of this analysis (e.g. Version 1.0): "
versionNum = readString(Versionpmt)
cat(versionNum, file = "inputs.txt", append = FALSE)

wdPmt = "Please enter the working directory (where your files are located): "
wd = readString(wdPmt)
cat(wd, file = "inputs.txt", append = TRUE)

fstnumPmt = "Please enter the first number of Agilent files you want to input: "
fstnum = readString(fstnumPmt)
cat(fstnum, file = "inputs.txt", append = TRUE)

lstnumPmt = "Please enter the last number of Agilent files you want to input: "
lstnum = readString(lstnumPmt)
cat(lstnum, file = "inputs.txt", append = TRUE)

SuperCoolpmt = "Please enter the name of the SuperCool data file: "
SuperCool = readString(SuperCoolpmt)
cat(SuperCool, file = "inputs.txt", append = TRUE)

RHpmt = "Please enter the name of the RH520 data file: "
RH = readString(RHpmt)
cat(RH, file = "inputs.txt", append = TRUE)

AERIpmt = "Please enter the name of the AERI data file: "
AERI = readString(AERIpmt)
cat(AERI, file = "inputs.txt", append = TRUE)
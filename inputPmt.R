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
inputTestCmtPmt = "Please enter the input test comments: (in a single line with only one enter in the end)"
inputTestCmts = readString(inputTestCmtPmt)
cat(inputTestCmts, file = "inputs.txt", append = TRUE)

fstInputAssoFilePmt = "Please enter the associated files (the files you are going to use) 1: "
fstInputAssoFile = readString(fstInputAssoFilePmt)
cat(fstInputAssoFile, file = "inputs.txt", append = TRUE)
sndInputAssoFilePmt = "Please enter the associated files (the files you are going to use) 2: "
sndInputAssoFile = readString(sndInputAssoFilePmt)
cat(sndInputAssoFile, file = "inputs.txt", append = TRUE)
srdInputAssoFilePmt = "Please enter the associated files (the files you are going to use) 3: "
srdInputAssoFile = readString(srdInputAssoFilePmt)
cat(srdInputAssoFile, file = "inputs.txt", append = TRUE)

TEfilePmt = "Please enter the TE file name again: "
TEfile = readString(TEfilePmt)
cat(TEfile, file = "inputs.txt", append = TRUE)

promptForRH520 = "Please enter the name of RH520 file again (with the extension) : "
dtName1 = readString(promptForRH520)
cat(dtName1, file = "inputs.txt", append = TRUE)

offsetrhpmt = "Please enter the offset of RH520 data with respect to CST (format: HH:MM:SS): "
offsetRH = readString(offsetrhpmt)
cat(offsetRH, file = "inputs.txt", append = TRUE)

offsettepmt = "Please enter the offset of TE data with respect to CST (format: HH:MM:SS): "
offsetTE = readString(offsettepmt)
cat(offsetTE, file = "inputs.txt", append = TRUE)
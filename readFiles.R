setwd("~/Desktop/Business/SSEC")
filename = "TE0391-X_OHMS-4W_160915T1405.txt"
# To determine how many lines are there in the file.
len = length(readLines(filename))
# Create a vector to store the tables.
dts = vector(mode = "list")
i = 2
j = 1
# Recursively read the tables and stores it to the corresponding place of dts vector.
while(i <= len){
  # Read one table
  dt = read.table(filename, skip = (i + 1), nrows = 7)
  # Store the table
  dts[[j]] = dt
  # Update the line to skip
  i = i + 10
  # Update the index of the vector
  j = j + 1
}

dts

filename = "TE0377-X_OHMS-4W_160915T1405.txt"
# To determine how many lines are there in the file.
len = length(readLines(filename))
# Create a vector to store the tables.
dts = vector(mode = "list")
i = 2
j = 1
# Recursively read the tables and stores it to the corresponding place of dts vector.
while(i <= len){
  # Read one table
  dt = read.table(filename, skip = (i + 1), nrows = 7)
  # Store the table
  dts[[j]] = dt
  # Update the line to skip
  i = i + 10
  # Update the index of the vector
  j = j + 1
}

dts

# The title of the tables don't show up, since when we are process the data, we don't use the title but only the contents. 
# To check the title, just open the original text file and follow the index to find it. 

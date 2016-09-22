#!/usr/bin/env Rscript

#R Programming
#Data Science Specialization
#John Hopkins University
#Coursera
#HW 1, uses hw1_data.csv


display_column_names <- function(data) {
  #display the column names from the data set
  print("1. Column Names")
  print(names(data))
}

display_first2_rows <- function(data) {
  print("2. Display the first two rows of data from the data set")
  print(data[(1:2),])
}

how_many_observations <- function(data) {
  print("3. How many observations, or rows, are in the data set?")
  print(nrow(data))
}

display_last2_rows <- function(data) {
  print("4. Display the last 2 rows of the data set")
  last_row <- nrow(data)
  #print((last_row-1):last_row)
  print(data[((last_row - 1):last_row),])
}

display_ozone_47th_row <- function(data) {
  print("5. Display the Ozone value from the 47th row")
  print(data[47,"Ozone"])
}

how_many_missing_ozone <- function(data) {
  print("6. How many missing values are in the Ozone column (i.e. NA)?")
  missing <- data[is.na(data$Ozone),]
  print(nrow(missing))
}

mean_of_ozone_values <- function(data) {
  print("7. What is the mean of the Ozone values exluding the one marked NA?")
  ozone <- data[!is.na(data$Ozone), "Ozone"]
  print(mean(ozone))
}

solarR_mean_in_subset <- function(data) {
  print("8. In the subset where Ozone values are above 31 and Temp values are greater than 90,")
  print("   what is the mean of the solar.R values?")
  ozone32 = data[(data$Ozone > 31), ]
  temp91 = ozone32[(ozone32$Temp > 90), ]
  solarR = temp91[!is.na(temp91$Solar.R), "Solar.R"]
  print(mean(solarR))
}

mean_temp_in_June <- function(data) {
  print("9. What is the mean tempature when the month equal 6?")
  june_temp <- data[(data$Month == 6), "Temp"]
  print(mean(june_temp))
}

max_ozone_in_may <- function(data) {
  print("10. What is the maximum ozone value in the month of May (i.e. Month == 5)?")
  #Get only the Ozone values from the month of May
  #ozoneInMay <- data[data$Month == 5, "Ozone"]
  #print(max(ozoneInMay))
  #This returns NA
  #strip the ozone values that contain NA from the data set
  ozone <- data[!is.na(data$Ozone),]
  ozoneInMay <- ozone[ozone$Month == 5, "Ozone"]
  print(max(ozoneInMay))
}

#--------------------------------------------------------

funcs <- c(display_column_names, 
           display_first2_rows, 
           how_many_observations, 
           display_last2_rows,
           display_ozone_47th_row,
           how_many_missing_ozone,
           mean_of_ozone_values,
           solarR_mean_in_subset,
           mean_temp_in_June,
           max_ozone_in_may)

main <- function() {
  data_file <- read.csv("hw1_data.csv")
  #print(data_file)
  lapply(funcs, function(func) {
    func(data_file)
    print("----------------------")
  })
}

main()

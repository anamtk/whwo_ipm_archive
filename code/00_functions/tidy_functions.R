# Tidying functions
# Ana Miller-ter Kuile
# September 21, 2021

# useful functions for dealing with the data from the 
# WHWO sites

# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", 
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Reading in a list of files from a folder --------------------------------

rdnm_files <- function(files){
  
  # import the list of dataframes in the vector "files", 
  #now a list of dataframes, into R
  data_list <-  lapply(files, 
                       function(x) read_xlsx(x))
  
  # make a shortened name of the files
  files_short <- files %>%
    str_replace(".*/", "") %>% # remove the file path
    str_replace(".xlsx", "") # remove the file extension
  
  # make the list of files have the name of their file
  names(data_list) <- files_short
  
  return(data_list)
}

# Insert a character into a string ----------------------------------------

fun_insert <- function(x, pos, insert) {
  #define the position where the new character shoudld "pos" argument
  gsub(paste0("^(.{", pos, "})(.*)$"), 
       #define what characters to insert at that position with 'insert' arg
       paste0("\\1", insert, "\\2"),
       #what should we add the character to? vector/column/etc
       x)
}

# Scaling for PCA ---------------------------------------------------------
#the function to scale all the numeric variables
scale_fun <- function(x){
  x_scaled <- scale(x, 
                    center = TRUE,
                    scale = TRUE)
  return(x_scaled)
}


# Extract numbers from comment --------------------------------------------

library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 



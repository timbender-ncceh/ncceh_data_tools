####################################
#####.........README...........#####
####################################

# Latest readme at: 
#<<< TODO: CREATE URL ON GITHUB>>>

# Generate Simulated Data from HMIS .csv files

#NOTES:
#
#1) PII will be overwritten by hashing.  All fields to be hashed must be
#identified by column_name within variable "pii.cols"
#
#2) input and output directories must be set accordingly via input_files_dir and
#output_files_dir
#

################################### 
####......../README...........#####
####################################

load.libs <- c("data.table",
               "dplyr",
               "readr",      # data read/write 
               "zip",        # compress and write data to .zip files
               "rstudioapi") # get directory info

# check if libraries/packages are installed
for(l in load.libs){
  # Is package 'l' installed? If not, ask user for permission to install package
  my.pkgs <- packageStatus()$inst$Package # gets a list of all installed packages
  if(!l %in% my.pkgs){
    cat('\n')
    # if run outside an interactive session, automatically install missing packages
    permit.install.pkg <- ifelse(interactive(), readline(prompt = paste("Authorize Script to install package [",l,"] (y/n)?", sep = "")), "y")
    # If authorized, install package
    ifelse(test = tolower(permit.install.pkg) %in% c("y", "yes"), 
           install.packages(l), 
           print("User declined installation of required package; script will not execute properly until installation is permitted. If you receive this message in error contact Tim Bender"))
  }else{
    cat('\n')
    print(paste(l,"already installed"))
  }
  # now that library has been installed if missing, load library
  library(l, character.only = T)
  print(paste(l,"loaded"))
}

rm(l, load.libs, my.pkgs); cat('\f'); gc()


# Vars----
simulated_sample_size <- 100  # set desired number of rows and/or observations to sample from each file
input_files_dir  <- "C:/Users/TimBender/Documents/tableau/DHHS_vaccination_linkage/data"
output_files_dir <- "C:/Users/TimBender/Documents/R/ncceh/projects/codi"


# identify all pii column names: 
pii.cols <- c("UserFirstName", "UserLastName", "UserPhone", "UserEmail", 
              "DOB", "NameSuffix", "MiddleName", "FirstName", "LastName", "SSN")

# Set input file directory location----
setwd(input_files_dir)

# generate sample data by randomly selecting attributes from each column----
temp.files <- list.files(pattern = "csv$") 
new.files <- list()
n_rows <- simulated_sample_size
for(i1 in temp.files){
  try(temp.file <- read_csv(i1))
  # hash pii
  for(i3 in 1:ncol(temp.file)){
    if(names(temp.file[,i3]) %in% pii.cols){
      try(temp.file[,i3] <- openssl::sha256(x = as.character(unlist(as.vector(temp.file[,i3]))), key = "ncceh"))
    }
  }
  temp.df   <- NULL
  if(is.null(temp.df)){
    for(c1 in 1:ncol(temp.file)){
      
      if(is.null(temp.df)){
        try(temp.df <- temp.file[sample(1:nrow(temp.file), size = min(c(n_rows, nrow(temp.file))), replace = F),c1])
      }else{
        try(temp.df <- cbind(temp.df, temp.file[sample(1:nrow(temp.file), size = min(c(n_rows, nrow(temp.file))), replace = F),c1]))
      }
    }
  }
  try(new.files[[i1]] <-  temp.df)
  rm(temp.file)
}

# generate sample data by randomly selecting rows from sample data----
temp.files2 <- list.files(pattern = "csv$") 
new.files2 <- list()
n_rows <- simulated_sample_size
for(i1 in temp.files2){
  # hash pii
  try(temp.file2 <- read_csv(i1))
  for(i3 in 1:ncol(temp.file2)){
    if(names(temp.file2[,i3]) %in% pii.cols){
      try(temp.file2[,i3] <- openssl::sha256(x = as.character(unlist(as.vector(temp.file2[,i3]))), key = "ncceh"))
    }
  }
  try(temp.df2   <- temp.file2[sample(1:nrow(temp.file2), size = min(c(n_rows, nrow(temp.file2))), replace = F),])
  try(new.files2[[i1]] <-  temp.df2)
  rm(temp.file2)
}
rm(list=ls()[!ls() %in% c("new.files", "new.files2")])

# write both datas to new files----
# set dir to desired output location
setwd(output_files_dir)

names(new.files)
for(i in names(new.files)){
  new.name <- paste("RandRows_", i, sep = "", collapse = "")
  write_csv(x = new.files[[i]], file = new.name)
  rm(new.name)
}

names(new.files2)
for(i in names(new.files2)){
  new.name <- paste("RandObs_", i, sep = "", collapse = "")
  write_csv(x = new.files2[[i]], file = new.name)
  rm(new.name)
}

# compress files
zip(zipfile = "simulated_RandRows.zip", 
    files = c(list.files(pattern = "^RandRows.*\\.csv$")), 
    recurse = F, 
    mode = "cherry-pick")

zip(zipfile = "simulated_RandObs.zip", 
    files = c(list.files(pattern = "^RandObs.*\\.csv$")), 
    recurse = F, 
    mode = "cherry-pick")

# remove generated zip files
file.remove(list.files(pattern = "^RandRows.*\\.csv$"))
file.remove(list.files(pattern = "^RandObs.*\\.csv$"))

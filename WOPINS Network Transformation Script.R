#################################################################################
#
# This R script is used to transform simple adjacency matrix data into a wide
# variable and write it to a .CSV file for merging with other statistical software 
# This will allow users to calculate values for a survey respondents 
# 'network alters' with out needing familiarity with
# social network methods. This script was devoloped for the WOPINS data 
# submission to ICPSR.
#
# This script was written in R studio. Users should download R and R Studio 
# before proceeding.
#
# https://www.r-project.org/
# https://rstudio.com/products/rstudio/
# 
# The following steps are coded below;
# 1: Installing and loading the required R packages for the transformation.
# 2: Specifying parameters unique to the end user.
# 3: R code to generate a global function to transform the network.
# 4: Running the function and listing the files in the working directory. 

#===============================================================================
# Section 2 is the only place where user changes need occur.  
#===============================================================================

#################################################################################

# The code included here will *NOT* generate an alter variable for all of the 
# members of the network.
# The output file will only include network members who selected at least one 
# 'network alter' for any given social network measured. 
# Otherwise, they are not included in the 'edgelist' transformation
# within the function below. These 'non-network respondents' can be identified 
# when merging the output file back to the survey data. 
# Likewise, the variables in the output data are only for survey respondents.

# Additionally, this script is intended only for simple adjacency matrices where 
# the cell values are 0 or 1. 

################################################################################# 
#0.0 - Clear the Workspace

rm(list=ls()) 

#1.0 - Run to determine if needed packages are installed; else install them

pacakge_list <- c("tidyr", "dplyr", "igraph")
new_packages <- pacakge_list[!(pacakge_list %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
rm(new_packages, pacakge_list) 


#1.1 - Load the packages into the work space

library(tidyr)
library(dplyr)
library(igraph)

################################################################################# 
# 2.0 - End User Parameters                                                     #
#                                                                               #
#=================Specify 3 Parameters Specific to the End User ================#
                                                                                #
# 1 - User's working directory - use front slashes, not back slashes            #
                                                                                #
myFilePath <- setwd("C:/")
                                                                                #
# 2 - The network to be transformed - change for each new network               #
                                                                                #
myNetwork <- "myFileName.csv" 
                                                                                #
# 3 - The output file name - change for each new network                        #
                                                                                #
myOutputName <- "myOutputFile.csv"
                                                                                #
#===============================================================================#

################################################################################# 
# 3.0 - This block of code creates a global function to use in R called Transform

Transform <- function(df, type = matrix){
  setwd(myFilePath)                                                                 
  network <- read.csv(myNetwork,             
                 header = TRUE,              
                 check.names = FALSE,
                 row.names = 1,
                 stringsAsFactors = FALSE,
                 na.strings = "")
  matrix <- graph.adjacency(as.matrix(network),
                            mode="directed",
                            weighted=NULL)
  edgelist <- get.edgelist(matrix,
                           names = TRUE)
  dataframe <- as.data.frame(edgelist)
  longData <- group_by(dataframe,V1)
  longDataCount <- mutate(longData,alterCount = 1:n())
  wideData <<- spread(longDataCount, alterCount, V2)
  names(wideData)[names(wideData)=="V1"] <<- "ID"
  write.csv(wideData, 
            myOutputName,          
            row.names = FALSE,
            na="")
  cat("\014")
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs[grep("^my",objs)], pos = ".GlobalEnv")
}

# Running the function created above, writes a .csv file to the working 
# directory, and creates a data frame in R called 'wideData' for the user 
# to view.
# The function is designed to remove the user specified objects to prevent over-
# writing the transformed files. Change the three user parameters, and re-run 
# this whole R script. Recommend ctrl-A, ctrl-ENTER. 
 

################################################################################# 
# 4.0 - Transforms the network read in by the function
Transform(network)

# 4.1 - Prints the files in the working directory 
list.files(getwd())

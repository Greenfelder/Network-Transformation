#################################################################################
#
# This R script is used to transform the adjacency matrix data from the 
# three units of women into 'wide' form data that can be easily merged 
# with the survey datafile. This will allow users to calculate values for a 
# survey respondents 'network alters' with out needing familiarity with
# social network methods.
#
# This script was written in R studio. Users should download R and R Studio 
# before proceeding.
#
# https://www.r-project.org/
# https://rstudio.com/products/rstudio/
# 
# The following steps are coded below;
# 1: Installing and loading the required R packages for the transformation.
# 2: R code to generate a global function to transform the network.
# 3: Specifying parameters unique to the end user.
# 4: Running the function and listing the files in the working directory. 

# Section 3 is the only place where user changes need occur.  

#################################################################################

# The code included here will *NOT* generate an alter variable for all of the 
# members of the unit.
# The output file will only include unit members who selected at least one 
# 'network alter'
# for any given social network measured. Otherwise, they are not included in the 
# 'edgelist' transformation
# within the function below. These 'non-network respondents' can be identified 
# when merging the output 
# file back to the survey data. Likewise, the variables in the output data are 
# only for survey respondents.

# Additionally, this script is intended only for the following networks;
# "Get Along With", "Prison Family", "Power and Influence", 
# and "Support During a Dispute".

# The "Prison Family Role" and "Power and Influence Affect" are specialized 
# matrices where the cell value contains information about the network. 
# This script does not handle these networks. 

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
#2.0 - This block of code creates a global function to use in R called Transform

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

################################################################################# 
# 3.0 - End User Parameters                                                     #
#                                                                               #      
#=================Specify 3 Items Specific to the End User =====================#
                                                                                #
# 1 - User's working directory - use front slashes, not back slashes            #
                                                                                #
myFilePath <- setwd("C:/Users/theod/Desktop/testEnviroment")
                                                                                #
# 2 - The network to be transformed - change for each new network               #
                                                                                #
myNetwork <- "Unit3_SDD.csv" 
                                                                                #
# 3 - The output file name - change for each new network                        #
                                                                                #
myOutputName <- "SupportNet.csv"
                                                                                #
#===============================================================================#

# Running the Function created above, writes a .csv file to the working 
# directory, and creates a data frame in R called 'wideData' for the user to view.

# The function is designed to remove the user specified objects to prevent over-
# writing the transformed files. Change the three user parameters, and re-run this
# whole R script. Recommend ctrl-A, ctrl-ENTER

# 4.0
Transform(network)

# 4.1
list.files(getwd())

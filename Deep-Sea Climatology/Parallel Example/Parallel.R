library(parallel) # calls: makeCluster, clusterEvalQ, parLapply
#install.packages("mgcv")
library(mgcv) # Calls: gamSim, gam

# First, send an instance of R to each core on the local machine
# The detectCores() function detects the number of phyiscal cores and sends R to 
# all of them, but one could replace the function with a number to utilize fewer 
# than the maximum number of cores
cl=makeCluster(detectCores()) #Example: cl=makeCluster(2) would use 2 cores

# Load package on all instances of R on all cores
clusterEvalQ(cl,c(library(mgcv)))
# Use function clusterExport() to send dataframes or other objects to each core
# clusterExport(cl,varlist=c("exampledata"))
# Create datasets for analysis
data=gamSim(1,n=10000,dist="normal",scale=2) 
data1=gamSim(1,n=10000,dist="normal",scale=2)
data2=gamSim(1,n=10000,dist="normal",scale=2) 
# Bind datasets in a list
data.list=list(data,data1,data2)

# Use parLapply to run GAM
system.time( parLapply(cl,data.list,function(i) {
  gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=i) } ) )

#Close the cluster
stopCluster(cl)

# For comparison's sake, how long would this take to run using regular lapply and 
# 1 core?

# Use lapply to run GAM
system.time( lapply(data.list,function(i) {
  gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=i) } ) )
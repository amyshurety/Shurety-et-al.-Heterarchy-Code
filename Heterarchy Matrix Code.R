##Libaries needed

library(igraph)
library(NetIndices)
library(readr)
library(likelihood)
library(readr)
library(ggplot2)
library(openxlsx)
library(robustHD)
library(scales)
library(gridExtra)
library(patchwork)
library(ggpubr)
library(magrittr)
library(ggrepel)
library(stats)
library(reshape2)
library(dbplyr)
library(car)
library(circular)
library(CircMLE)

## Step 1: Simulating random networks

#Generating and saving 1000 theoretical networks using the Erdos-Renyi algorithm. 
#The theoretical networks have 100 nodes. 

setwd("C:/Users/") #Set your working directory where you would ike the random networks to save

RN <- vector('list', 1000)
for(i in 1:1000){
  RN[[i]] <- erdos.renyi.game(n = 100, 
                              p.or.m = 0.32, type = "gnp")} #n = number of nodes

for(i in 1:1000){write_graph(RN[[i]],paste("RG",i,".txt"
                                           ,sep=''))}

## Step 2: Network Metric of the random networks

#The network metric, modularity was calculated and saved for all 1000 random networks.

#Read in random graphs

setwd("C:/Users/") #Set your working directory where the random networks are saved

myRN<- list(NULL)
for (i in 1:1000){myRN[[i]]= 
  read_graph((paste("RG", i, ".txt", sep="")), format='edgelist')}

#calculate modularity

cwt<- list(NULL)
for (i in 1:1000){cwt[[i]]<- cluster_walktrap(myRN[[i]])}

ModRN<- list(NULL)
for (i in 1:1000){ModRN[[i]]<- modularity(cwt[[i]])}

#Save RDS

setwd("C:/Users/") #Set your working directory where you would like the modularity RDS to save

ModRN1<-unlist(ModRN)
ModRN2<-as.vector(ModRN1)
round(ModRN, digits=5)
saveRDS(ModRN2, "DataMod")

#Create an Excell Spreadsheet

ModRN<-as.data.frame(readRDS("DataMod"))

setwd("C:/Users/") #Set your working directory where you would like the modularity excell spreadsheet to save

write_xlsx(ModRN, "ModRNfFinal.xlsx")

## Step 3: Hieararchy Metrics of the random networks

#A probability heirarchy score (PHS) developed by Cheng et al. (2016) was calculated for all 1000 random networks.
#The Cheng code calculates the PHS for all stopulated levels. To get one metric value for each network. The Cheng 
#code was run for n/2 levels. The number of levels was changed directly in the source code. The below code is done 
#for 50 levels. A linear regression model of the PHS metric vs the level interation was fitted and the AIC was 
#calculated for each model. The model with the highest AIC was established and the relative PHS value was extracted.

#Note: The network needs to be re-loaded as calculating the hiearachy metrics requires the network to be 
#in a different format. 

setwd("C:/Users/") #Set your working directory where the random networks are saved

myRNH<- list(NULL)

for (i in 1:1000){
  myRNH[[i]] = read.table(paste("RG", i, ".txt", sep=""), 
                          colClasses= rep("character", 2))
}

setwd("C:/Users/") # Set the working directory which contains the Cheng source code

source(file="ChengHirNet_functions.R")

setwd("C:/Users/") #Set your working directory where you would like the Cheng code output to save

## calculate the hierarchical structure with Lev = n/2

HMRN= list(NULL)
for (i in 1:1000){HMRN[[i]] = cal_hier_score26(myRNH[[i]]
                                               , kmax=10000, ptim=100, anneal.coeff=1e-6
                                               ,  myoutf = paste("Hierarchical_Structure_RN"
                                                                 , i, ".txt", sep=""))} 

myHRNresults<- list(NULL)
for (i in 1:1000){myHRNresults[[i]]= read.table(paste("Hierarchical_Structure_RN", i, ".txt", sep=""),sep="\t", fill=TRUE)} # Read in the Cheng code output 

##Gets the needed rows (each level)

HMRNL2<-list(NULL)
for (i in 1:1000){HMRNL2[[i]]<-subset(myHRNresults[[i]], V1=='Lev=2')}

HMRNL3<-list(NULL)
for (i in 1:1000){HMRNL3[[i]]<-subset(myHRNresults[[i]], V1=='Lev=3')}

HMRNL4<-list (NULL)
for (i in 1:1000){HMRNL4[[i]]<-subset(myHRNresults[[i]], V1=='Lev=4')}

HMRNL5<-list(NULL)
for (i in 1:1000){HMRNL5[[i]]<-subset(myHRNresults[[i]], V1=='Lev=5')}

HMRNL6<-list(NULL)
for (i in 1:1000){HMRNL6[[i]]<-subset(myHRNresults[[i]], V1=='Lev=6')}

HMRNL7<-list(NULL)
for (i in 1:1000){HMRNL7[[i]]<-subset(myHRNresults[[i]], V1=='Lev=7')}

HMRNL8<-list(NULL)
for (i in 1:1000){HMRNL8[[i]]<-subset(myHRNresults[[i]], V1=='Lev=8')}

HMRNL9<-list(NULL)
for (i in 1:1000){HMRNL9[[i]]<-subset(myHRNresults[[i]], V1=='Lev=9')}

HMRNL10<-list(NULL)
for (i in 1:1000){HMRNL10[[i]]<-subset(myHRNresults[[i]], V1=='Lev=10')}

HMRNL11<-list(NULL)
for (i in 1:1000){HMRNL11[[i]]<-subset(myHRNresults[[i]], V1=='Lev=11')}

HMRNL12<-list(NULL)
for (i in 1:1000){HMRNL12[[i]]<-subset(myHRNresults[[i]], V1=='Lev=12')}

HMRNL13<-list(NULL)
for (i in 1:1000){HMRNL13[[i]]<-subset(myHRNresults[[i]], V1=='Lev=13')}

HMRNL14<-list(NULL)
for (i in 1:1000){HMRNL14[[i]]<-subset(myHRNresults[[i]], V1=='Lev=14')}

HMRNL15<-list(NULL)
for (i in 1:1000){HML15[[i]]<-subset(myHRNresults[[i]], V1=='Lev=15')}

HMRNL16<-list(NULL)
for (i in 1:1000){HMRNL16[[i]]<-subset(myHRNresults[[i]], V1=='Lev=16')}

HMRNL17<-list(NULL)
for (i in 1:1000){HMRNL17[[i]]<-subset(myHRNresults[[i]], V1=='Lev=17')}

HMRNL18<-list(NULL)
for (i in 1:1000){HMRNL18[[i]]<-subset(myHRNresults[[i]], V1=='Lev=18')}

HMRNL19<-list(NULL)
for (i in 1:1000){HMRNL19[[i]]<-subset(myHRNresults[[i]], V1=='Lev=19')}

HMRNL20<-list(NULL)
for (i in 1:1000){HMRNL20[[i]]<-subset(myHRNresults[[i]], V1=='Lev=20')}

HMRNL21<-list(NULL)
for (i in 1:1000){HMRNL21[[i]]<-subset(myHRNresults[[i]], V1=='Lev=21')}

HMRNL22<-list(NULL)
for (i in 1:1000){HMRNL22[[i]]<-subset(myHRNresults[[i]], V1=='Lev=22')}

HMRNL23<-list(NULL)
for (i in 1:1000){HMRNL23[[i]]<-subset(myHRNresults[[i]], V1=='Lev=23')}

HMRNL24<-list(NULL)
for (i in 1:1000){HMRNL24[[i]]<-subset(myHRNresults[[i]], V1=='Lev=24')}

HMRNL25<-list(NULL)
for (i in 1:1000){HMRNL25[[i]]<-subset(myHRNresults[[i]], V1=='Lev=25')}

HMRNL26<-list(NULL)
for (i in 1:1000){HRNL26[[i]]<-subset(myHRNresults[[i]], V1=='Lev=26')}

HMRNL27<-list(NULL)
for (i in 1:1000){HMRNL27[[i]]<-subset(myHRNresults[[i]], V1=='Lev=27')}

HMRNL28<-list(NULL)
for (i in 1:1000){HMRNL28[[i]]<-subset(myHRNresults[[i]], V1=='Lev=28')}

HMRNL29<-list(NULL)
for (i in 1:1000){HMRNL29[[i]]<-subset(myHRNresults[[i]], V1=='Lev=29')}

HMRNL30<-list(NULL)
for (i in 1:1000){HMRNL30[[i]]<-subset(myHRNresults[[i]], V1=='Lev=30')}

HMRNL31<-list(NULL)
for (i in 1:1000){HMRNL31[[i]]<-subset(myHRNresults[[i]], V1=='Lev=31')}

HMRNL32<-list(NULL)
for (i in 1:1000){HMRNL32[[i]]<-subset(myHRNresults[[i]], V1=='Lev=32')}

HMRNL33<-list(NULL)
for (i in 1:1000){HMRNL33[[i]]<-subset(myHRNresults[[i]], V1=='Lev=33')}

HMRNL34<-list(NULL)
for (i in 1:1000){HMRNL34[[i]]<-subset(myHRNresults[[i]], V1=='Lev=34')}

HMRNL35<-list(NULL)
for (i in 1:1000){HMRNL35[[i]]<-subset(myHRNresults[[i]], V1=='Lev=35')}

HMRNL36<-list(NULL)
for (i in 1:1000){HMRNL36[[i]]<-subset(myHRNresults[[i]], V1=='Lev=36')}

HMRNL37<-list(NULL)
for (i in 1:1000){HMRNL37[[i]]<-subset(myHRNresults[[i]], V1=='Lev=37')}

HMRNL38<-list(NULL)
for (i in 1:1000){HMRNL38[[i]]<-subset(myHRNresults[[i]], V1=='Lev=38')}

HMRNL39<-list(NULL)
for (i in 1:1000){HMRNL39[[i]]<-subset(myHRNresults[[i]], V1=='Lev=39')}

HMRNL40<-list(NULL)
for (i in 1:1000){HML40[[i]]<-subset(myHRNresults[[i]], V1=='Lev=40')}

HMRNL41<-list(NULL)
for (i in 1:1000){HMRNL41[[i]]<-subset(myHRNresults[[i]], V1=='Lev=41')}

HMRNL42<-list(NULL)
for (i in 1:1000){HMRNL42[[i]]<-subset(myHRNresults[[i]], V1=='Lev=42')}

HMRNL43<-list(NULL)
for (i in 1:1000){HMRNL43[[i]]<-subset(myHRNresults[[i]], V1=='Lev=43')}

HMRNL44<-list(NULL)
for (i in 1:1000){HMRNL44[[i]]<-subset(myHRNresults[[i]], V1=='Lev=44')}

HMRNL45<-list(NULL)
for (i in 1:1000){HMRNL45[[i]]<-subset(myHRNresults[[i]], V1=='Lev=45')}

HMRNL46<-list(NULL)
for (i in 1:1000){HMRNL46[[i]]<-subset(myHRNresults[[i]], V1=='Lev=46')}

HMRNL47<-list(NULL)
for (i in 1:1000){HMRNL47[[i]]<-subset(myHRNresults[[i]], V1=='Lev=47')}

HMRNL48<-list(NULL)
for (i in 1:1000){HMRNL48[[i]]<-subset(myHRNresults[[i]], V1=='Lev=48')}

HMRNL49<-list(NULL)
for (i in 1:1000){HMRNL49[[i]]<-subset(myHRNresults[[i]], V1=='Lev=49')}

HMRNL50<-list(NULL)
for (i in 1:1000){HMRNL50[[i]]<-subset(myHRNresults[[i]], V1=='Lev=50')}

#Isolate the PHS metric for each level

HMPHSRNL2<-list(NULL)
for (i in 1:1000){HMPHSRNL2[[i]]<- HMRNL2[[i]]$V4}

HMPHSRNL3<-list(NULL)
for (i in 1:1000){HMRNPHSRNL3[[i]]<- HMRNL3[[i]]$V4}

HMPHSRNL4<-list(NULL)
for (i in 1:1000){HMPHSRNL4[[i]]<- HMRNL4[[i]]$V4}

HMPHSRNL5<-list(NULL)
for (i in 1:1000){HMPHSRNL5[[i]]<- HMRNL5[[i]]$V4}

HMPHSRNL6<-list(NULL)
for (i in 1:1000){HMPHSRNL6[[i]]<- HMRNL6[[i]]$V4}

HMPHSRNL7<-list(NULL)
for (i in 1:1000){HMPHSRNL7[[i]]<- HMRNL7[[i]]$V4}

HMPHSRNL8<-list(NULL)
for (i in 1:1000){HMPHSRNL8[[i]]<- HMRNL8[[i]]$V4}

HMPHSRNL9<-list(NULL)
for (i in 1:1000){HMPHSRNL9[[i]]<- HMRNL9[[i]]$V4}

HMPHSRNL10<-list(NULL)
for (i in 1:1000){HMPHSRNL10[[i]]<- HMRNL10[[i]]$V4}

HMPHSRNL11<-list(NULL)
for (i in 1:1000){HMPHSRNL11[[i]]<- HMRNL11[[i]]$V4}

HMPHSRNL12<-list(NULL)
for (i in 1:1000){HMPHSRNL12[[i]]<- HMRNL12[[i]]$V4}

HMPHSRNL13<-list(NULL)
for (i in 1:1000){HMPHSRNL13[[i]]<- HMRNL13[[i]]$V4}

HMPHSRNL14<-list(NULL)
for (i in 1:1000){HMPHSRNL14[[i]]<- HMRNL14[[i]]$V4}

HMPHSRNL15<-list(NULL)
for (i in 1:1000){HMPHSRNL15[[i]]<- HMRNL15[[i]]$V4}

HMPHSRNL16<-list(NULL)
for (i in 1:1000){HMPHSRNL16[[i]]<- HMRNL16[[i]]$V4}

HMPHSRNL17<-list(NULL)
for (i in 1:1000){HMPHSRNL17[[i]]<- HMRNL17[[i]]$V4}

HMPHSRNL18<-list(NULL)
for (i in 1:1000){HMPHSRNL18[[i]]<- HMRNL18[[i]]$V4}

HMPHSRNL19<-list(NULL)
for (i in 1:1000){HMPHSRNL19[[i]]<- HMRNL19[[i]]$V4}

HMPHSRNL20<-list(NULL)
for (i in 1:1000){HMPHSRNL20[[i]]<- HMRNL20[[i]]$V4}

HMPHSRNL21<-list(NULL)
for (i in 1:1000){HMPHSRNL21[[i]]<- HMRNL21[[i]]$V4}

HMPHSRNL22<-list(NULL)
for (i in 1:1000){HMPHSRNL22[[i]]<- HMRNL22[[i]]$V4}

HMPHSRNL23<-list(NULL)
for (i in 1:1000){HMPHSRNL23[[i]]<- HMRNL23[[i]]$V4}

HMPHSRNL24<-list(NULL)
for (i in 1:1000){HMPHSRNL24[[i]]<- HMRNL24[[i]]$V4}

HMPHSRNL25<-list(NULL)
for (i in 1:1000){HMPHSRNL25[[i]]<- HMRNL25[[i]]$V4}

HMPHSRNL26<-list(NULL)
for (i in 1:1000){HMPHSRNL26[[i]]<- HMRNL26[[i]]$V4}

HMPHSRNL27<-list(NULL)
for (i in 1:1000){HMPHSRNL27[[i]]<- HMRNL27[[i]]$V4}

HMPHSRNL28<-list(NULL)
for (i in 1:1000){HMPHSRNL28[[i]]<- HMRNL28[[i]]$V4}

HMPHSRNL29<-list(NULL)
for (i in 1:1000){HMPHSRNL29[[i]]<- HMRNL29[[i]]$V4}

HMPHSRNL30<-list(NULL)
for (i in 1:1000){HMPHSRNL30[[i]]<- HMRNL30[[i]]$V4}

HMPHSRNL31<-list(NULL)
for (i in 1:1000){HMPHSRNL31[[i]]<- HMRNL31[[i]]$V4}

HMPHSRNL32<-list(NULL)
for (i in 1:1000){HMPHSRNL32[[i]]<- HMRNL32[[i]]$V4}

HMPHSRNL33<-list(NULL)
for (i in 1:1000){HMPHSRNL33[[i]]<- HMRNL33[[i]]$V4}

HMPHSRNL34<-list(NULL)
for (i in 1:1000){HMPHSRNL34[[i]]<- HMRNL34[[i]]$V4}

HMPHSRNL35<-list(NULL)
for (i in 1:1000){HMPHSRNL35[[i]]<- HMRNL35[[i]]$V4}

HMPHSRNL36<-list(NULL)
for (i in 1:1000){HMPHSRNL36[[i]]<- HMRNL36[[i]]$V4}

HMPHSRNL37<-list(NULL)
for (i in 1:1000){HMPHSRNL37[[i]]<- HMRNL37[[i]]$V4}

HMPHSRNL38<-list(NULL)
for (i in 1:1000){HMPHSRNL38[[i]]<- HMRNL38[[i]]$V4}

HMPHSRNL39<-list(NULL)
for (i in 1:1000){HMPHSRNL39[[i]]<- HMRNL39[[i]]$V4}

HMPHSRNL40<-list(NULL)
for (i in 1:1000){HMPHSRNL40[[i]]<- HMRNL40[[i]]$V4}

HMPHSRNL41<-list(NULL)
for (i in 1:1000){HMPHSRNL41[[i]]<- HMRNL41[[i]]$V4}

HMPHSRNL42<-list(NULL)
for (i in 1:1000){HMPHSRNL42[[i]]<- HMRNL42[[i]]$V4}

HMPHSRNL43<-list(NULL)
for (i in 1:1000){HMPHSRNL43[[i]]<- HMRNL43[[i]]$V4}

HMPHSRNL44<-list(NULL)
for (i in 1:1000){HMPHSRNL44[[i]]<- HMRNL44[[i]]$V4}

HMPHSRNL45<-list(NULL)
for (i in 1:1000){HMPHSRNL45[[i]]<- HMRNL45[[i]]$V4}

HMPHSRNL46<-list(NULL)
for (i in 1:1000){HMPHSRNL46[[i]]<- HMRNL46[[i]]$V4}

HMPHSRNL47<-list(NULL)
for (i in 1:1000){HMPHSRNL47[[i]]<- HMRNL47[[i]]$V4}

HMPHSRNL48<-list(NULL)
for (i in 1:1000){HMPHSRNL48[[i]]<- HMRNL48[[i]]$V4}

HMPHSRNL49<-list(NULL)
for (i in 1:1000){HMPHSRNL49[[i]]<- HMRNL49[[i]]$V4}

HMPHSRNL50<-list(NULL)
for (i in 1:1000){HMPHSRNL50[[i]]<- HMRNL50[[i]]$V4}

## Create Vector

HMPHSRNL2v<-list(NULL)
for (i in 1:1000){HMPHSRNL2v[[i]]<-as.vector(HMPHSRNL2[[i]])}

HMPHSRNL3v<-list(NULL)
for (i in 1:1000){HMPHSRNL3v[[i]]<- as.vector(HMPHSRNL3[[i]])}

HMPHSRNL4v<-list(NULL)
for (i in 1:1000){HMPHSRNL4v[[i]]<- as.vector(HMPHSRNL4[[i]])}

HMPHSRNL5v<-list(NULL)
for (i in 1:1000){HMPHSRNL5v[[i]]<- as.vector(HMPHSRNL5[[i]])}

HMPHSRNL6v<-list(NULL)
for (i in 1:1000){HMPHSRNL6v[[i]]<- as.vector(HMPHSRNL6[[i]])}

HMPHSRNL7v<-list(NULL)
for (i in 1:1000){HMPHSRNL7v[[i]]<- as.vector(HMPHSRNL7[[i]])}

HMPHSRNL8v<-list(NULL)
for (i in 1:1000){HMPHSRNL8v[[i]]<- as.vector(HMPHSRNL8[[i]])}

HMPHSRNL9v<-list(NULL)
for (i in 1:1000){HMPHSRNL9v[[i]]<- as.vector(HMPHSRNL9[[i]])}

HMPHSRNL10v<-list(NULL)
for (i in 1:1000){HMPHSRNL10v[[i]]<- as.vector(HMPHSRNL10[[i]])}

HMPHSRNL11v<-list(NULL)
for (i in 1:1000){HMPHSRNL11v[[i]]<- as.vector(HMPHSRNL11[[i]])}

HMPHSRNL12v<-list(NULL)
for (i in 1:1000){HMPHSRNL12v[[i]]<- as.vector(HMPHSRNL12[[i]])}

HMPHSRNL13v<-list(NULL)
for (i in 1:1000){HMPHSRNL13v[[i]]<- as.vector(HMPHSRNL13[[i]])}

HMPHSRNL14v<-list(NULL)
for (i in 1:1000){HMPHSRNL14v[[i]]<- as.vector(HMPHSRNL14[[i]])}

HMPHSRNL15v<-list(NULL)
for (i in 1:1000){HMPHSRNL15v[[i]]<- as.vector(HMPHSRNL15[[i]])}

HMPHSRNL16v<-list(NULL)
for (i in 1:1000){HMPHSRNL16v[[i]]<- as.vector(HMPHSRNL16[[i]])}

HMPHSRNL17v<-list(NULL)
for (i in 1:1000){HMPHSRNL17v[[i]]<- as.vector(HMPHSRNL17[[i]])}

HMPHSRNL18v<-list(NULL)
for (i in 1:1000){HMPHSRNL18v[[i]]<- as.vector(HMPHSRNL18[[i]])}

HMPHSRNL19v<-list(NULL)
for (i in 1:1000){HMPHSRNL19v[[i]]<- as.vector(HMPHSRNL19[[i]])}

HMPHSRNL20v<-list(NULL)
for (i in 1:1000){HMPHSRNL20v[[i]]<- as.vector(HMPHSRNL20[[i]])}

HMPHSRNL21v<-list(NULL)
for (i in 1:1000){HMPHSRNL21v[[i]]<- as.vector(HMPHSRNL21[[i]])}

HMPHSRNL22v<-list(NULL)
for (i in 1:1000){HMPHSRNL22v[[i]]<- as.vector(HMPHSRNL22[[i]])}

HMPHSRNL23v<-list(NULL)
for (i in 1:1000){HMPHSRNL23v[[i]]<- as.vector(HMPHSRNL23[[i]])}

HMPHSRNL24v<-list(NULL)
for (i in 1:1000){HMPHSRNL24v[[i]]<- as.vector(HMPHSRNL24[[i]])}

HMPHSRNL25v<-list(NULL)
for (i in 1:1000){HMPHSRNL25v[[i]]<- as.vector(HMPHSRNL25[[i]])}

HMPHSRNL26v<-list(NULL)
for (i in 1:1000){HMPHSRNL26v[[i]]<- as.vector(HMPHSRNL26[[i]])}

HMPHSRNL27v<-list(NULL)
for (i in 1:1000){HMPHSRNL27v[[i]]<- as.vector(HMPHSRNL27[[i]])}

HMPHSRNL28v<-list(NULL)
for (i in 1:1000){HMPHSRNL28v[[i]]<- as.vector(HMPHSRNL28[[i]])}

HMPHSRNL29v<-list(NULL)
for (i in 1:1000){HMPHSRNL29v[[i]]<- as.vector(HMPHSRNL29[[i]])}

HMPHSRNL30v<-list(NULL)
for (i in 1:1000){HMPHSRNL30v[[i]]<- as.vector(HMPHSRNL30[[i]])}

HMPHSRNL31v<-list(NULL)
for (i in 1:1000){HMPHSRNL31v[[i]]<- as.vector(HMPHSRNL31[[i]])}

HMPHSRNL32v<-list(NULL)
for (i in 1:1000){HMPHSRNL32v[[i]]<- as.vector(HMPHSRNL32[[i]])}

HMPHSRNL33v<-list(NULL)
for (i in 1:1000){HMPHSRNL33v[[i]]<- as.vector(HMPHSRNL33[[i]])}

HMPHSRNL34v<-list(NULL)
for (i in 1:1000){HMPHSRNL34v[[i]]<- as.vector(HMPHSRNL34[[i]])}

HMPHSRNL35v<-list(NULL)
for (i in 1:1000){HMPHSRNL35v[[i]]<- as.vector(HMPHSRNL35[[i]])}

HMPHSRNL36v<-list(NULL)
for (i in 1:1000){HMPHSRNL36v[[i]]<- as.vector(HMPHSRNL36[[i]])}

HMPHSRNL37v<-list(NULL)
for (i in 1:1000){HMPHSRNL37v[[i]]<- as.vector(HMPHSRNL37[[i]])}

HMPHSRNL38v<-list(NULL)
for (i in 1:1000){HMPHSRNL38v[[i]]<- as.vector(HMPHSRNL38[[i]])}

HMPHSRNL39v<-list(NULL)
for (i in 1:1000){HMPHSRNL39v[[i]]<- as.vector(HMPHSRNL39[[i]])}

HMPHSRNL40v<-list(NULL)
for (i in 1:1000){HMPHSRNL40v[[i]]<- as.vector(HMPHSRNL40[[i]])}

HMPHSRNL41v<-list(NULL)
for (i in 1:1000){HMPHSRNL41v[[i]]<- as.vector(HMPHSRNL41[[i]])}

HMPHSRNL42v<-list(NULL)
for (i in 1:1000){HMPHSRNL42v[[i]]<- as.vector(HMPHSRNL42[[i]])}

HMPHSRNL43v<-list(NULL)
for (i in 1:1000){HMPHSRNL43v[[i]]<- as.vector(HMPHSRNL43[[i]])}

HMPHSRNL44v<-list(NULL)
for (i in 1:1000){HMPHSRNL44v[[i]]<- as.vector(HMPHSRNL44[[i]])}

HMPHSRNL45v<-list(NULL)
for (i in 1:1000){HMPHSRNL45v[[i]]<- as.vector(HMPHSRNL45[[i]])}

HMPHSRNL46v<-list(NULL)
for (i in 1:1000){HMPHSRNL46v[[i]]<- as.vector(HMPHSRNL46[[i]])}

HMPHSRNL47v<-list(NULL)
for (i in 1:1000){HMPHSRNL47v[[i]]<- as.vector(HMPHSRNL47[[i]])}

HMPHSRNL48v<-list(NULL)
for (i in 1:1000){HMPHSRNL48v[[i]]<- as.vector(HMPHSRNL48[[i]])}

HMPHSRNL49v<-list(NULL)
for (i in 1:1000){HMPHSRNL49v[[i]]<- as.vector(HMPHSRNL49[[i]])}

HMPHSRNL50v<-list(NULL)
for (i in 1:1000){HMPHSRNL50v[[i]]<- as.vector(HMPHSRNL50[[i]])}


library(readr)

HMPHSRNL2f<-list(NULL)
for (i in 1:1000){HMPHSRNL2f[[i]]<-parse_number(HMPHSRNL2v[[i]])}

HMPHSRNL3f<-list(NULL)
for (i in 1:1000){HMPHSRNL3f[[i]]<- parse_number(HMPHSRNL3v[[i]])}

HMPHSRNL4f<-list(NULL)
for (i in 1:1000){HMPHSRNL4f[[i]]<- parse_number(HMPHSRNL4v[[i]])}

HMPHSRNL5f<-list(NULL)
for (i in 1:1000){HMPHSRNL5f[[i]]<- parse_number(HMPHSRNL5v[[i]])}

HMPHSRNL6f<-list(NULL)
for (i in 1:1000){HMPHSRNL6f[[i]]<- parse_number(HMPHSRNL6v[[i]])}

HMPHSRNL7f<-list(NULL)
for (i in 1:1000){HMPHSRNL7f[[i]]<- parse_number(HMPHSRNL7v[[i]])}

HMPHSRNL8f<-list(NULL)
for (i in 1:1000){HMPHSRNL8f[[i]]<- parse_number(HMPHSRNL8v[[i]])}

HMPHSRNL9f<-list(NULL)
for (i in 1:1000){HMPHSRNL9f[[i]]<- parse_number(HMPHSRNL9v[[i]])}

HMPHSRNL10f<-list(NULL)
for (i in 1:1000){HMPHSRNL10f[[i]]<- parse_number(HMPHSRNL10v[[i]])}

HMPHSRNL11f<-list(NULL)
for (i in 1:1000){HMPHSRNL11f[[i]]<- parse_number(HMPHSRNL11v[[i]])}

HMPHSRNL12f<-list(NULL)
for (i in 1:1000){HMPHSRNL12f[[i]]<- parse_number(HMPHSRNL12v[[i]])}

HMPHSRNL13f<-list(NULL)
for (i in 1:1000){HMPHSRNL13f[[i]]<- parse_number(HMPHSRNL13v[[i]])}

HMPHSRNL14f<-list(NULL)
for (i in 1:1000){HMPHSRNL14f[[i]]<- parse_number(HMPHSRNL14v[[i]])}

HMPHSRNL15f<-list(NULL)
for (i in 1:1000){HMPHSRNL15f[[i]]<- parse_number(HMPHSRNL15v[[i]])}

HMPHSRNL16f<-list(NULL)
for (i in 1:1000){HMPHSRNL16f[[i]]<- parse_number(HMPHSRNL16v[[i]])}

HMPHSRNL17f<-list(NULL)
for (i in 1:1000){HMPHSRNL17f[[i]]<- parse_number(HMPHSRNL17v[[i]])}

HMPHSRNL18f<-list(NULL)
for (i in 1:1000){HMPHSRNL18f[[i]]<- parse_number(HMPHSRNL18v[[i]])}

HMPHSRNL19f<-list(NULL)
for (i in 1:1000){HMPHSRNL19f[[i]]<- parse_number(HMPHSRNL19v[[i]])}

HMPHSRNL20f<-list(NULL[[i]])
for (i in 1:1000){HMPHSRNL20f[[i]]<- parse_number(HMPHSRNL20v[[i]])}

HMPHSRNL21f<-list(NULL)
for (i in 1:1000){HMPHSRNL21f[[i]]<- parse_number(HMPHSRNL21v[[i]])}

HMPHSRNL22f<-list(NULL)
for (i in 1:1000){HMPHSRNL22f[[i]]<- parse_number(HMPHSRNL22v[[i]])}

HMPHSRNL23f<-list(NULL)
for (i in 1:1000){HMPHSRNL23f[[i]]<- parse_number(HMPHSRNL23v[[i]])}

HMPHSRNL24f<-list(NULL)
for (i in 1:1000){HMPHSRNL24f[[i]]<- parse_number(HMPHSRNL24v[[i]])}

HMPHSRNL25f<-list(NULL)
for (i in 1:1000){HMPHSRNL25f[[i]]<- parse_number(HMPHSRNL25v[[i]])}

HMPHSRNL26f<-list(NULL)
for (i in 1:1000){HMPHSRNL26f[[i]]<- parse_number(HMPHSRNL26v[[i]])}

HMPHSRNL27f<-list(NULL)
for (i in 1:1000){HMPHSRNL27f[[i]]<- parse_number(HMPHSRNL27v[[i]])}

HMPHSRNL28f<-list(NULL)
for (i in 1:1000){HMPHSRNL28f[[i]]<- parse_number(HMPHSRNL28v[[i]])}

HMPHSRNL29f<-list(NULL)
for (i in 1:1000){HMPHSRNL29f[[i]]<- parse_number(HMPHSRNL29v[[i]])}

HMPHSRNL30f<-list(NULL)
for (i in 1:1000){HMPHSRNL30f[[i]]<- parse_number(HMPHSRNL30v[[i]])}

HMPHSRNL31f<-list(NULL)
for (i in 1:1000){HMPHSRNL31f[[i]]<- parse_number(HMPHSRNL31v[[i]])}

HMPHSRNL32f<-list(NULL[[i]])
for (i in 1:1000){HMPHSRNL32f[[i]]<- parse_number(HMPHSRNL32v[[i]])}

HMPHSRNL33f<-list(NULL)
for (i in 1:1000){HMPHSRNL33f[[i]]<- parse_number(HMPHSRNL33v[[i]])}

HMPHSRNL34f<-list(NULL)
for (i in 1:1000){HMPHSRNL34f[[i]]<- parse_number(HMPHSRNL34v[[i]])}

HMPHSRNL35f<-list(NULL)
for (i in 1:1000){HMPHSRNL35f[[i]]<- parse_number(HMPHSRNL35v[[i]])}

HMPHSRNL36f<-list(NULL)
for (i in 1:1000){HMPHSRNL36f[[i]]<- parse_number(HMPHSRNL36v[[i]])}

HMPHSRNL37f<-list(NULL)
for (i in 1:1000){HMPHSRNL37f[[i]]<- parse_number(HMPHSRNL37v[[i]])}

HMPHSRNL38f<-list(NULL)
for (i in 1:1000){HMPHSRNL38f[[i]]<- parse_number(HMPHSRNL38v[[i]])}

HMPHSRNL39f<-list(NULL)
for (i in 1:1000){HMPHSRNL39f[[i]]<- parse_number(HMPHSRNL39v[[i]])}

HMPHSRNL40f<-list(NULL)
for (i in 1:1000){HMPHSRNL40f[[i]]<- parse_number(HMPHSRNL40v[[i]])}

HMPHSRNL41f<-list(NULL)
for (i in 1:1000){HMPHSRNL41f[[i]]<- parse_number(HMPHSRNL41v[[i]])}

HMPHSRNL42f<-list(NULL)
for (i in 1:1000){HMPHSRNL42f[[i]]<- parse_number(HMPHSRNL42v[[i]])}

HMPHSRNL43f<-list(NULL)
for (i in 1:1000){HMPHSRNL43f[[i]]<- parse_number(HMPHSRNL43v[[i]])}

HMPHSRNL44f<-list(NULL)
for (i in 1:1000){HMPHSRNL44f[[i]]<- parse_number(HMPHSRNL44v[[i]])}

HMPHSRNL45f<-list(NULL)
for (i in 1:1000){HMPHSRNL45f[[i]]<- parse_number(HMPHSRNL45v[[i]])}

HMPHSRNL46f<-list(NULL)
for (i in 1:1000){HMPHSRNL46f[[i]]<- parse_number(HMPHSRNL46v[[i]])}

HMPHSRNL47f<-list(NULL)
for (i in 1:1000){HMPHSRNL47f[[i]]<- parse_number(HMPHSRNL47v[[i]])}

HMPHSRNL48f<-list(NULL)
for (i in 1:1000){HMPHSRNL48f[[i]]<- parse_number(HMPHSRNL48v[[i]])}

HMPHSRNL49f<-list(NULL)
for (i in 1:1000){HMPHSRNL49f[[i]]<- parse_number(HMPHSRNL49v[[i]])}

HMPHSRNL50f<-list(NULL)
for (i in 1:1000){HMPHSRNL50f[[i]]<- parse_number(HMPHSRNL50v[[i]])}

#Create a vector of each level iteration

HMPHSRNVec2<-list(NULL)
for (i in 1:1000){HMPHSRNVec2[[i]]<-as.vector(c(HMPHSRNL2f[[i]]))}

HMPHSRNVec3<-list(NULL)
for (i in 1:1000){HMPHSRNVec3[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]]))}

HMPHSRNVec4<-list(NULL)
for (i in 1:1000){HMPHSRNVec4[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]]))}

HMPHSRNVec5<-list(NULL)
for (i in 1:1000){HMPHSRNVec5[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]]))}

HMPHSRNVec6<-list(NULL)
for (i in 1:1000){HMPHSRNVec6[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]]))}

HMPHSRNVec7<-list(NULL)
for (i in 1:1000){HMPHSRNVec7[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]]))}

HMPHSRNVec8<-list(NULL)
for (i in 1:1000){HMPHSRNVec8[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]]))}

HMPHSRNVec9<-list(NULL)
for (i in 1:1000){HMPHSRNVec9[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]]))}

HMPHSRNVec10<-list(NULL)
for (i in 1:1000){HMPHSRNVec10[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]]))}

HMPHSRNVec11<-list(NULL)
for (i in 1:1000){HMPHSRNVec11[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]]))}

HMPHSRNVec12<-list(NULL)
for (i in 1:1000){HMPHSRNVec12[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]]))}

HMPHSRNVec13<-list(NULL)
for (i in 1:1000){HMPHSRNVec13[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]]))}

HMPHSRNVec14<-list(NULL)
for (i in 1:1000){HMPHSRNVec14[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]]))}

HMPHSRNVec15<-list(NULL)
for (i in 1:1000){HMPHSRNVec15[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]]))}

HMPHSRNVec16<-list(NULL)
for (i in 1:1000){HMPHSRNVec16[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]]))}

HMPHSRNVec17<-list(NULL)
for (i in 1:1000){HMPHSRNVec17[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]]))}

HMPHSRNVec18<-list(NULL)
for (i in 1:1000){HMPHSRNVec18[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]]))}

HMPHSRNVec19<-list(NULL)
for (i in 1:1000){HMPHSRNVec19[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]]))}

HMPHSRNVec20<-list(NULL)
for (i in 1:1000){HMPHSRNVec20[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]]))}

HMPHSRNVec21<-list(NULL)
for (i in 1:1000){HMPHSRNVec21[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]]))}

HMPHSRNVec22<-list(NULL)
for (i in 1:1000){HMPHSRNVec22[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]]))}

HMPHSRNVec23<-list(NULL)
for (i in 1:1000){HMPHSRNVec23[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]]))}

HMPHSRNVec24<-list(NULL)
for (i in 1:1000){HMPHSRNVec24[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]]))}

HMPHSRNVec25<-list(NULL)
for (i in 1:1000){HMPHSRNVec25[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]]))}

HMPHSRNVec26<-list(NULL)
for (i in 1:1000){HMPHSRNVec26[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]]))}

HMPHSRNVec27<-list(NULL)
for (i in 1:1000){HMPHSRNVec27[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]]))}

HMPHSRNVec28<-list(NULL)
for (i in 1:1000){HMPHSRNVec28[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]]))}

HMPHSRNVec29<-list(NULL)
for (i in 1:1000){HMPHSRNVec29[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]]))}

HMPHSRNVec30<-list(NULL)
for (i in 1:1000){HMPHSRNVec30[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]]))}

HMPHSRNVec31<-list(NULL)
for (i in 1:1000){HMPHSRNVec31[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]]))}

HMPHSRNVec32<-list(NULL)
for (i in 1:1000){HMPHSRNVec32[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]]))}

HMPHSRNVec33<-list(NULL)
for (i in 1:1000){HMPHSRNVec33[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]]))}

HMPHSRNVec34<-list(NULL)
for (i in 1:1000){HMPHSRNVec34[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]]))}

HMPHSRNVec35<-list(NULL)
for (i in 1:1000){HMPHSRNVec35[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]]))}

HMPHSRNVec36<-list(NULL)
for (i in 1:1000){HMPHSRNVec36[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]]))}

HMPHSRNVec37<-list(NULL)
for (i in 1:1000){HMPHSRNVec37[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]]))}

HMPHSRNVec38<-list(NULL)
for (i in 1:1000){HMPHSRNVec38[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]]))}

HMPHSRNVec39<-list(NULL)
for (i in 1:1000){HMPHSRNVec39[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]]))}

HMPHSRNVec40<-list(NULL)
for (i in 1:1000){HMPHSRNVec40[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]],HMPHSRNL40f[[i]]))}

HMPHSRNVec41<-list(NULL)
for (i in 1:1000){HMPHSRNVec41[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]],HMPHSRNL40f[[i]], HMPHSRNL41f[[i]]))}

HMPHSRNVec42<-list(NULL)
for (i in 1:1000){HMPHSRNVec42[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]],HMPHSRNL40f[[i]], HMPHSRNL41f[[i]], HMPHSRNL42f[[i]]))}

HMPHSRNVec43<-list(NULL)
for (i in 1:1000){HMPHSRNVec43[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]],HMPHSRNL40f[[i]], HMPHSRNL41f[[i]], HMPHSRNL42f[[i]], HMPHSRNL43f[[i]]))}

HMPHSRNVec44<-list(NULL)
for (i in 1:1000){HMPHSRNVec44[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]],HMPHSRNL40f[[i]], HMPHSRNL41f[[i]], HMPHSRNL42f[[i]], HMPHSRNL43f[[i]], HMPHSRNL44f[[i]]))}

HMPHSRNVec45<-list(NULL)
for (i in 1:1000){HMPHSRNVec45[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]],HMPHSRNL40f[[i]], HMPHSRNL41f[[i]], HMPHSRNL42f[[i]], HMPHSRNL43f[[i]], HMPHSRNL44f[[i]], HMPHSRNL45f[[i]]))}

HMPHSRNVec46<-list(NULL)
for (i in 1:1000){HMPHSRNVec46[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]],HMPHSRNL40f[[i]], HMPHSRNL41f[[i]], HMPHSRNL42f[[i]], HMPHSRNL43f[[i]], HMPHSRNL44f[[i]], HMPHSRNL45f[[i]], HMPHSRNL46f[[i]]))}

HMPHSRNVec47<-list(NULL)
for (i in 1:1000){HMPHSRNVec47[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]],HMPHSRNL40f[[i]], HMPHSRNL41f[[i]], HMPHSRNL42f[[i]], HMPHSRNL43f[[i]], HMPHSRNL44f[[i]], HMPHSRNL45f[[i]], HMPHSRNL46f[[i]], HMPHSRNL47f[[i]]))}

HMPHSRNVec48<-list(NULL)
for (i in 1:1000){HMPHSRNVec48[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]], HMPHSRNL40f[[i]], HMPHSRNL41f[[i]], HMPHSRNL42f[[i]], HMPHSRNL43f[[i]], HMPHSRNL44f[[i]], HMPHSRNL45f[[i]], HMPHSRNL46f[[i]], HMPHSRNL47f[[i]], HMPHSRNL48f[[i]]))}

HMPHSRNVec49<-list(NULL)
for (i in 1:1000){HMPHSRNVec49[[i]]<-as.vector(c(HMPHSRNL2f[[i]], HMPHSRNL3f[[i]], HMPHSRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]], HMPHSRNRNL40f[[i]], HMPHSRNRNL41f[[i]], HMPHSRNRNL42f[[i]], HMPHSRNRNL43f[[i]], HMPHSRNRNL44f[[i]], HMPHSRNRNL45f[[i]], HMPHSRNRNL46f[[i]], HMPHSRNRNL47f[[i]], HMPHSRNRNL48f[[i]], HMPHSRNRNL49f[[i]]))}

HMPHSRNRNVec50<-list(NULL)
for (i in 1:1000){HMPHSRNRNVec50[[i]]<-as.vector(c(HMPHSRNRNL2f[[i]], HMPHSRNRNL3f[[i]], HMPHSRNRNL4f[[i]], HMPHSRNL5f[[i]], HMPHSRNL6f[[i]], HMPHSRNL7f[[i]], HMPHSRNL8f[[i]], HMPHSRNL9f[[i]], HMPHSRNL10f[[i]], HMPHSRNL11f[[i]], HMPHSRNL12f[[i]], HMPHSRNL13f[[i]], HMPHSRNL14f[[i]], HMPHSRNL15f[[i]], HMPHSRNL16f[[i]], HMPHSRNL17f[[i]], HMPHSRNL18f[[i]], HMPHSRNL19f[[i]], HMPHSRNL20f[[i]], HMPHSRNL21f[[i]], HMPHSRNL22f[[i]], HMPHSRNL23f[[i]], HMPHSRNL24f[[i]], HMPHSRNL25f[[i]], HMPHSRNL26f[[i]], HMPHSRNL27f[[i]], HMPHSRNL28f[[i]], HMPHSRNL29f[[i]], HMPHSRNL30f[[i]], HMPHSRNL31f[[i]], HMPHSRNL32f[[i]], HMPHSRNL33f[[i]], HMPHSRNL34f[[i]], HMPHSRNL35f[[i]], HMPHSRNL36f[[i]], HMPHSRNL37f[[i]], HMPHSRNL38f[[i]], HMPHSRNL39f[[i]], HMPHSRNL40f[[i]], HMPHSRNL41f[[i]], HMPHSRNL42f[[i]], HMPHSRNL43f[[i]], HMPHSRNL44f[[i]], HMPHSRNL45f[[i]], HMPHSRNL46f[[i]], HMPHSRNL47f[[i]], HMPHSRNL48f[[i]], HMPHSRNL49f[[i]], HMPHSRNL50f[[i]]))}

#Create a data frame of PHS values and number of levels as collumns 

L2<-(2)
L3<-(2:3)
L4<-(2:4)
L5<-(2:5)
L6<-(2:6)
L7<-(2:7)
L8<-(2:8)
L9<-(2:9)
L10<-(2:10)
L11<-(2:11)
L12<-(2:12)
L13<-(2:13)
L14<-(2:14)
L15<-(2:15)
L16<-(2:16)
L17<-(2:17)
L18<-(2:18)
L19<-(2:19)
L20<-(2:20)
L21<-(2:21)
L22<-(2:22)
L23<-(2:23)
L24<-(2:24)
L25<-(2:25)
L26<-(2:26)
L27<-(2:27)
L28<-(2:28)
L29<-(2:29)
L30<-(2:30)
L31<-(2:31)
L32<-(2:32)
L33<-(2:33)
L34<-(2:34)
L35<-(2:35)
L36<-(2:36)
L37<-(2:37)
L38<-(2:38)
L39<-(2:39)
L40<-(2:40)
L41<-(2:41)
L42<-(2:42)
L43<-(2:43)
L44<-(2:44)
L45<-(2:45)
L46<-(2:46)
L47<-(2:47)
L48<-(2:48)
L49<-(2:49)
L50<-(2:50)

dfPHSRN2<-list(NULL)
for (i in 1:1000){dfPHSRN2[[i]]<-data.frame(cbind(L2, HMPHSRNVec2[[i]]))}
for (i in 1:1000){colnames(dfPHSRN2[[i]])<-c("Level", "PHS")}

dfPHSRN3<-list(NULL)
for (i in 1:1000){dfPHSRN3[[i]]<-data.frame(cbind(L3, HMPHSRNVec3[[i]]))}
for (i in 1:1000){colnames(dfPHSRN3[[i]])<-c("Level", "PHS")}

dfPHSRN4<-list(NULL)
for (i in 1:1000){dfPHSRN4[[i]]<-data.frame(cbind(L4, HMPHSRNVec4[[i]]))}
for (i in 1:1000){colnames(dfPHSRN4[[i]])<-c("Level", "PHS")}

dfPHSRN5<-list(NULL)
for (i in 1:1000){dfPHSRN5[[i]]<-data.frame(cbind(L5, HMPHSRNVec5[[i]]))}
for (i in 1:1000){colnames(dfPHSRN5[[i]])<-c("Level", "PHS")}

dfPHSRN6<-list(NULL)
for (i in 1:1000){dfPHSRN6[[i]]<-data.frame(cbind(L6, HMPHSRNVec6[[i]]))}
for (i in 1:1000){colnames(dfPHSRN6[[i]])<-c("Level", "PHS")}

dfPHSRN7<-list(NULL)
for (i in 1:1000){dfPHSRN7[[i]]<-data.frame(cbind(L7, HMPHSRNVec7[[i]]))}
for (i in 1:1000){colnames(dfPHSRN7[[i]])<-c("Level", "PHS")}

dfPHSRN8<-list(NULL)
for (i in 1:1000){dfPHSRN8[[i]]<-data.frame(cbind(L8, HMPHSRNVec8[[i]]))}
for (i in 1:1000){colnames(dfPHSRN8[[i]])<-c("Level", "PHS")}

dfPHSRN9<-list(NULL)
for (i in 1:1000){dfPHSRN9[[i]]<-data.frame(cbind(L9, HMPHSRNVec9[[i]]))}
for (i in 1:1000){colnames(dfPHSRN9[[i]])<-c("Level", "PHS")}

dfPHSRN10<-list(NULL)
for (i in 1:1000){dfPHSRN10[[i]]<-data.frame(cbind(L10, HMPHSRNVec10[[i]]))}
for (i in 1:1000){colnames(dfPHSRN10[[i]])<-c("Level", "PHS")}

dfPHSRN11<-list(NULL)
for (i in 1:1000){dfPHSRN11[[i]]<-data.frame(cbind(L11, HMPHSRNVec11[[i]]))}
for (i in 1:1000){colnames(dfPHSRN11[[i]])<-c("Level", "PHS")}

dfPHSRN12<-list(NULL)
for (i in 1:1000){dfPHSRN12[[i]]<-data.frame(cbind(L12, HMPHSRNVec12[[i]]))}
for (i in 1:1000){colnames(dfPHSRN12[[i]])<-c("Level", "PHS")}

dfPHSRN13<-list(NULL)
for (i in 1:1000){dfPHSRN13[[i]]<-data.frame(cbind(L13, HMPHSRNVec13[[i]]))}
for (i in 1:1000){colnames(dfPHSRN13[[i]])<-c("Level", "PHS")}

dfPHSRN14<-list(NULL)
for (i in 1:1000){dfPHSRN14[[i]]<-data.frame(cbind(L14, HMPHSRNVec14[[i]]))}
for (i in 1:1000){colnames(dfPHSRN14[[i]])<-c("Level", "PHS")}

dfPHSRN15<-list(NULL)
for (i in 1:1000){dfPHSRN15[[i]]<-data.frame(cbind(L15, HMPHSRNVec15[[i]]))}
for (i in 1:1000){colnames(dfPHSRN15[[i]])<-c("Level", "PHS")}

dfPHSRN16<-list(NULL)
for (i in 1:1000){dfPHSRN16[[i]]<-data.frame(cbind(L16, HMPHSRNVec16[[i]]))}
for (i in 1:1000){colnames(dfPHSRN16[[i]])<-c("Level", "PHS")}

dfPHSRN17<-list(NULL)
for (i in 1:1000){dfPHSRN17[[i]]<-data.frame(cbind(L17, HMPHSRNVec17[[i]]))}
for (i in 1:1000){colnames(dfPHSRN17[[i]])<-c("Level", "PHS")}

dfPHSRN18<-list(NULL)
for (i in 1:1000){dfPHSRN18[[i]]<-data.frame(cbind(L18, HMPHSRNVec18[[i]]))}
for (i in 1:1000){colnames(dfPHSRN18[[i]])<-c("Level", "PHS")}

dfPHSRN19<-list(NULL)
for (i in 1:1000){dfPHSRN19[[i]]<-data.frame(cbind(L19, HMPHSRNVec19[[i]]))}
for (i in 1:1000){colnames(dfPHSRN19[[i]])<-c("Level", "PHS")}

dfPHSRN20<-list(NULL)
for (i in 1:1000){dfPHSRN20[[i]]<-data.frame(cbind(L20, HMPHSRNVec20[[i]]))}
for (i in 1:1000){colnames(dfPHSRN20[[i]])<-c("Level", "PHS")}

dfPHSRN21<-list(NULL)
for (i in 1:1000){dfPHSRN21[[i]]<-data.frame(cbind(L21, HMPHSRNVec21[[i]]))}
for (i in 1:1000){colnames(dfPHSRN21[[i]])<-c("Level", "PHS")}

dfPHSRN22<-list(NULL)
for (i in 1:1000){dfPHSRN22[[i]]<-data.frame(cbind(L22, HMPHSRNVec22[[i]]))}
for (i in 1:1000){colnames(dfPHSRN22[[i]])<-c("Level", "PHS")}

dfPHSRN23<-list(NULL)
for (i in 1:1000){dfPHSRN23[[i]]<-data.frame(cbind(L23, HMPHSRNVec23[[i]]))}
for (i in 1:1000){colnames(dfPHSRN23[[i]])<-c("Level", "PHS")}

dfPHSRN24<-list(NULL)
for (i in 1:1000){dfPHSRN24[[i]]<-data.frame(cbind(L24, HMPHSRNVec24[[i]]))}
for (i in 1:1000){colnames(dfPHSRN24[[i]])<-c("Level", "PHS")}

dfPHSRN25<-list(NULL)
for (i in 1:1000){dfPHSRN25[[i]]<-data.frame(cbind(L25, HMPHSRNVec25[[i]]))}
for (i in 1:1000){colnames(dfPHSRN25[[i]])<-c("Level", "PHS")}

dfPHSRN26<-list(NULL)
for (i in 1:1000){dfPHSRN26[[i]]<-data.frame(cbind(L26, HMPHSRNVec26[[i]]))}
for (i in 1:1000){colnames(dfPHSRN26[[i]])<-c("Level", "PHS")}

dfPHSRN27<-list(NULL)
for (i in 1:1000){dfPHSRN27[[i]]<-data.frame(cbind(L27, HMPHSRNVec27[[i]]))}
for (i in 1:1000){colnames(dfPHSRN27[[i]])<-c("Level", "PHS")}

dfPHSRN28<-list(NULL)
for (i in 1:1000){dfPHSRN28[[i]]<-data.frame(cbind(L28, HMPHSRNVec28[[i]]))}
for (i in 1:1000){colnames(dfPHSRN28[[i]])<-c("Level", "PHS")}

dfPHSRN29<-list(NULL)
for (i in 1:1000){dfPHSRN29[[i]]<-data.frame(cbind(L29, HMPHSRNVec29[[i]]))}
for (i in 1:1000){colnames(dfPHSRN29[[i]])<-c("Level", "PHS")}

dfPHSRN30<-list(NULL)
for (i in 1:1000){dfPHSRN30[[i]]<-data.frame(cbind(L30, HMPHSRNVec30[[i]]))}
for (i in 1:1000){colnames(dfPHSRN30[[i]])<-c("Level", "PHS")}

dfPHSRN31<-list(NULL)
for (i in 1:1000){dfPHSRN31[[i]]<-data.frame(cbind(L31, HMPHSRNVec31[[i]]))}
for (i in 1:1000){colnames(dfPHSRN31[[i]])<-c("Level", "PHS")}

dfPHSRN32<-list(NULL)
for (i in 1:1000){dfPHSRN32[[i]]<-data.frame(cbind(L32, HMPHSRNVec32[[i]]))}
for (i in 1:1000){colnames(dfPHSRN32[[i]])<-c("Level", "PHS")}

dfPHSRN33<-list(NULL)
for (i in 1:1000){dfPHSRN33[[i]]<-data.frame(cbind(L33, HMPHSRNVec33[[i]]))}
for (i in 1:1000){colnames(dfPHSRN33[[i]])<-c("Level", "PHS")}

dfPHSRN34<-list(NULL)
for (i in 1:1000){dfPHSRN34[[i]]<-data.frame(cbind(L34, HMPHSRNVec34[[i]]))}
for (i in 1:1000){colnames(dfPHSRN34[[i]])<-c("Level", "PHS")}

dfPHSRN35<-list(NULL)
for (i in 1:1000){dfPHSRN35[[i]]<-data.frame(cbind(L35, HMPHSRNVec35[[i]]))}
for (i in 1:1000){colnames(dfPHSRN35[[i]])<-c("Level", "PHS")}

dfPHSRN36<-list(NULL)
for (i in 1:1000){dfPHSRN36[[i]]<-data.frame(cbind(L36, HMPHSRNVec36[[i]]))}
for (i in 1:1000){colnames(dfPHSRN36[[i]])<-c("Level", "PHS")}

dfPHSRN37<-list(NULL)
for (i in 1:1000){dfPHSRN37[[i]]<-data.frame(cbind(L37, HMPHSRNVec37[[i]]))}
for (i in 1:1000){colnames(dfPHSRN37[[i]])<-c("Level", "PHS")}

dfPHSRN38<-list(NULL)
for (i in 1:1000){dfPHSRN38[[i]]<-data.frame(cbind(L38, HMPHSRNVec38[[i]]))}
for (i in 1:1000){colnames(dfPHSRN38[[i]])<-c("Level", "PHS")}

dfPHSRN39<-list(NULL)
for (i in 1:1000){dfPHSRN39[[i]]<-data.frame(cbind(L39, HMPHSRNVec39[[i]]))}
for (i in 1:1000){colnames(dfPHSRN39[[i]])<-c("Level", "PHS")}

dfPHSRN40<-list(NULL)
for (i in 1:1000){dfPHSRN40[[i]]<-data.frame(cbind(L40, HMPHSRNVec40[[i]]))}
for (i in 1:1000){colnames(dfPHSRN40[[i]])<-c("Level", "PHS")}

dfPHSRN41<-list(NULL)
for (i in 1:1000){dfPHSRN41[[i]]<-data.frame(cbind(L41, HMPHSRNVec41[[i]]))}
for (i in 1:1000){colnames(dfPHSRN41[[i]])<-c("Level", "PHS")}

dfPHSRN42<-list(NULL)
for (i in 1:1000){dfPHSRN42[[i]]<-data.frame(cbind(L42, HMPHSRNVec42[[i]]))}
for (i in 1:1000){colnames(dfPHSRN42[[i]])<-c("Level", "PHS")}

dfPHSRN43<-list(NULL)
for (i in 1:1000){dfPHSRN43[[i]]<-data.frame(cbind(L43, HMPHSRNVec43[[i]]))}
for (i in 1:1000){colnames(dfPHSRN43[[i]])<-c("Level", "PHS")}

dfPHSRN44<-list(NULL)
for (i in 1:1000){dfPHSRN44[[i]]<-data.frame(cbind(L44, HMPHSRNVec44[[i]]))}
for (i in 1:1000){colnames(dfPHSRN44[[i]])<-c("Level", "PHS")}

dfPHSRN45<-list(NULL)
for (i in 1:1000){dfPHSRN45[[i]]<-data.frame(cbind(L45, HMPHSRNVec45[[i]]))}
for (i in 1:1000){colnames(dfPHSRN45[[i]])<-c("Level", "PHS")}

dfPHSRN46<-list(NULL)
for (i in 1:1000){dfPHSRN46[[i]]<-data.frame(cbind(L46, HMPHSRNVec46[[i]]))}
for (i in 1:1000){colnames(dfPHSRN46[[i]])<-c("Level", "PHS")}

dfPHSRN47<-list(NULL)
for (i in 1:1000){dfPHSRN47[[i]]<-data.frame(cbind(L47, HMPHSRNVec47[[i]]))}
for (i in 1:1000){colnames(dfPHSRN47[[i]])<-c("Level", "PHS")}

dfPHSRN48<-list(NULL)
for (i in 1:1000){dfPHSRN48[[i]]<-data.frame(cbind(L48, HMPHSRNVec48[[i]]))}
for (i in 1:1000){colnames(dfPHSRN48[[i]])<-c("Level", "PHS")}

dfPHSRN49<-list(NULL)
for (i in 1:1000){dfPHSRN49[[i]]<-data.frame(cbind(L49, HMPHSRNVec49[[i]]))}
for (i in 1:1000){colnames(dfPHSRN49[[i]])<-c("Level", "PHS")}

dfPHSRNRN50<-list(NULL)
for (i in 1:1000){dfPHSRNRN0[[i]]<-data.frame(cbind(L50, HMPHSRNVec50[[i]]))}
for (i in 1:1000){colnames(dfPHSRNRN50[[i]])<-c("Level", "PHS")}


#Create a linear regression model for PHS value vs level iteration

PHSRNlm2<-list(NULL)
for (i in 1:1000){PHSRNlm2[[i]]<- lm(PHS~Level , data = dfPHSRN2[[i]])}

PHSRNlm3<-list(NULL)
for (i in 1:1000){PHSRNlm3[[i]]<- lm(PHS~Level , data = dfPHSRN3[[i]])}

PHSRNlm4<-list(NULL)
for (i in 1:1000){PHSRNlm4[[i]]<- lm(PHS~Level , data = dfPHSRN4[[i]])}

PHSRNlm5<-list(NULL)
for (i in 1:1000){PHSRNlm5[[i]]<- lm(PHS~Level , data = dfPHSRN5[[i]])}

PHSRNlm6<-list(NULL)
for (i in 1:1000){PHSRNlm6[[i]]<- lm(PHS~Level , data = dfPHSRN6[[i]])}

PHSRNlm7<-list(NULL)
for (i in 1:1000){PHSRNlm7[[i]]<- lm(PHS~Level , data = dfPHSRN7[[i]])}

PHSRNlm8<-list(NULL)
for (i in 1:1000){PHSRNlm8[[i]]<- lm(PHS~Level , data = dfPHSRN8[[i]])}

PHSRNlm9<-list(NULL)
for (i in 1:1000){PHSRNlm9[[i]]<- lm(PHS~Level , data = dfPHSRN9[[i]])}

PHSRNlm10<-list(NULL)
for (i in 1:1000){PHSRNlm10[[i]]<- lm(PHS~Level , data = dfPHSRN10[[i]])}

PHSRNlm11<-list(NULL)
for (i in 1:1000){PHSRNlm11[[i]]<- lm(PHS~Level, data = dfPHSRN11[[i]])}

PHSRNlm12<-list(NULL)
for (i in 1:1000){PHSRNlm12[[i]]<- lm(PHS~Level , data = dfPHSRN12[[i]])}

PHSRNlm13<-list(NULL)
for (i in 1:1000){PHSRNlm13[[i]]<- lm(PHS~Level , data = dfPHSRN13[[i]])}

PHSRNlm14<-list(NULL)
for (i in 1:1000){PHSRNlm14[[i]]<- lm(PHS~Level , data = dfPHSRN14[[i]])}

PHSRNlm15<-list(NULL)
for (i in 1:1000){PHSRNlm15[[i]]<- lm(PHS~Level , data = dfPHSRN15[[i]])}

PHSRNlm16<-list(NULL)
for (i in 1:1000){PHSRNlm16[[i]]<- lm(PHS~Level , data = dfPHSRN16[[i]])}

PHSRNlm17<-list(NULL)
for (i in 1:1000){PHSRNlm17[[i]]<- lm(PHS~Level , data = dfPHSRN17[[i]])}

PHSRNlm18<-list(NULL)
for (i in 1:1000){PHSRNlm18[[i]]<- lm(PHS~Level , data = dfPHSRN18[[i]])}

PHSRNlm19<-list(NULL)
for (i in 1:1000){PHSRNlm19[[i]]<- lm(PHS~Level , data = dfPHSRN19[[i]])}

PHSRNlm20<-list(NULL)
for (i in 1:1000){PHSRNlm20[[i]]<- lm(PHS~Level , data = dfPHSRN20[[i]])}

PHSRNlm21<-list(NULL)
for (i in 1:1000){PHSRNlm21[[i]]<- lm(PHS~Level , data = dfPHSRN21[[i]])}

PHSRNlm22<-list(NULL)
for (i in 1:1000){PHSRNlm22[[i]]<- lm(PHS~Level , data = dfPHSRN22[[i]])}

PHSRNlm23<-list(NULL)
for (i in 1:1000){PHSRNlm23[[i]]<- lm(PHS~Level , data = dfPHSRN23[[i]])}

PHSRNlm24<-list(NULL)
for (i in 1:1000){PHSRNlm24[[i]]<- lm(PHS~Level , data = dfPHSRN24[[i]])}

PHSRNlm25<-list(NULL)
for (i in 1:1000){PHSRNlm25[[i]]<- lm(PHS~Level , data = dfPHSRN25[[i]])}

PHSRNlm26<-list(NULL)
for (i in 1:1000){PHSRNlm26[[i]]<- lm(PHS~Level , data = dfPHSRN26[[i]])}

PHSRNlm27<-list(NULL)
for (i in 1:1000){PHSRNlm27[[i]]<- lm(PHS~Level , data = dfPHSRN27[[i]])}

PHSRNlm28<-list(NULL)
for (i in 1:1000){PHSRNlm28[[i]]<- lm(PHS~Level , data = dfPHSRN28[[i]])}

PHSRNlm29<-list(NULL)
for (i in 1:1000){PHSRNlm29[[i]]<- lm(PHS~Level , data = dfPHSRN29[[i]])}

PHSRNlm30<-list(NULL)
for (i in 1:1000){PHSRNlm30[[i]]<- lm(PHS~Level , data = dfPHSRN30[[i]])}

PHSRNlm31<-list(NULL)
for (i in 1:1000){PHSRNlm31[[i]]<- lm(PHS~ . , data = dfPHSRN31[[i]])}

PHSRNlm32<-list(NULL)
for (i in 1:1000){PHSRNlm32[[i]]<- lm(PHS~ . , data = dfPHSRN32[[i]])}

PHSRNlm33<-list(NULL)
for (i in 1:1000){PHSRNlm33[[i]]<- lm(PHS~ . , data = dfPHSRN33[[i]])}

PHSRNlm34<-list(NULL)
for (i in 1:1000){PHSRNlm34[[i]]<- lm(PHS~ . , data = dfPHSRN34[[i]])}

PHSRNlm35<-list(NULL)
for (i in 1:1000){PHSRNlm35[[i]]<- lm(PHS~ . , data = dfPHSRN35[[i]])}

PHSRNlm36<-list(NULL)
for (i in 1:1000){PHSRNlm36[[i]]<- lm(PHS~ . , data = dfPHSRN36[[i]])}

PHSRNlm37<-list(NULL)
for (i in 1:1000){PHSRNlm37[[i]]<- lm(PHS~ . , data = dfPHSRN37[[i]])}

PHSRNlm38<-list(NULL)
for (i in 1:1000){PHSRNlm38[[i]]<- lm(PHS~ . , data = dfPHSRN38[[i]])}

PHSRNlm39<-list(NULL)
for (i in 1:1000){PHSRNlm39[[i]]<- lm(PHS~ . , data = dfPHSRN39[[i]])}

PHSRNlm40<-list(NULL)
for (i in 1:1000){PHSRNlm40[[i]]<- lm(PHS~ . , data = dfPHSRN40[[i]])}

PHSRNlm41<-list(NULL)
for (i in 1:1000){PHSRNlm41[[i]]<- lm(PHS~ . , data = dfPHSRN41[[i]])}

PHSRNlm42<-list(NULL)
for (i in 1:1000){PHSRNlm42[[i]]<- lm(PHS~ . , data = dfPHSRN42[[i]])}

PHSRNlm43<-list(NULL)
for (i in 1:1000){PHSRNlm43[[i]]<- lm(PHS~ . , data = dfPHSRN43[[i]])}

PHSRNlm44<-list(NULL)
for (i in 1:1000){PHSRNlm44[[i]]<- lm(PHS~ . , data = dfPHSRN44[[i]])}

PHSRNlm45<-list(NULL)
for (i in 1:1000){PHSRNlm45[[i]]<- lm(PHS~ . , data = dfPHSRN45[[i]])}

PHSRNlm46<-list(NULL)
for (i in 1:1000){PHSRNlm46[[i]]<- lm(PHS~ . , data = dfPHSRN46[[i]])}

PHSRNlm47<-list(NULL)
for (i in 1:1000){PHSRNlm47[[i]]<- lm(PHS~ . , data = dfPHSRN47[[i]])}

PHSRNlm48<-list(NULL)
for (i in 1:1000){PHSRNlm48[[i]]<- lm(PHS~ . , data = dfPHSRN48[[i]])}

PHSRNlm49<-list(NULL)
for (i in 1:1000){PHSRNlm49[[i]]<- lm(PHS~ . , data = dfPHSRN49[[i]])}

PHSRNlm50<-list(NULL)
for (i in 1:1000){PHSRNlm50[[i]]<- lm(PHS~ . , data = dfPHSRN50[[i]])}

#Calculate the AIC value for each linear regression model 

##Log Liklihood

PHSRNlogLik2<-list(NULL)
for (i in 1:1000){PHSRNlogLik2[[i]]<-logLik(PHSRNlm2[[i]])}

PHSRNlogLik3<-list(NULL)
for (i in 1:1000){PHSRNlogLik3[[i]]<-logLik(PHSRNlm3[[i]])}

PHSRNlogLik4<-list(NULL)
for (i in 1:1000){PHSRNlogLik4[[i]]<-logLik(PHSRNlm4[[i]])}

PHSRNlogLik5<-list(NULL)
for (i in 1:1000){PHSRNlogLik5[[i]]<-logLik(PHSRNlm5[[i]])}

PHSRNlogLik6<-list(NULL)
for (i in 1:1000){PHSRNlogLik6[[i]]<-logLik(PHSRNlm6[[i]])}

PHSRNlogLik7<-list(NULL)
for (i in 1:1000){PHSRNlogLik7[[i]]<-logLik(PHSRNlm7[[i]])}

PHSRNlogLik8<-list(NULL)
for (i in 1:1000){PHSRNlogLik8[[i]]<-logLik(PHSRNlm8[[i]])}

PHSRNlogLik9<-list(NULL)
for (i in 1:1000){PHSRNlogLik9[[i]]<-logLik(PHSRNlm9[[i]])}

PHSRNlogLik10<-list(NULL)
for (i in 1:1000){PHSRNlogLik10[[i]]<-logLik(PHSRNlm10[[i]])}

PHSRNlogLik11<-list(NULL)
for (i in 1:1000){PHSRNlogLik11[[i]]<-logLik(PHSRNlm11[[i]])}

PHSRNlogLik12<-list(NULL)
for (i in 1:1000){PHSRNlogLik12[[i]]<-logLik(PHSRNlm12[[i]])}

PHSRNlogLik13<-list(NULL)
for (i in 1:1000){PHSRNlogLik13[[i]]<-logLik(PHSRNlm13[[i]])}

PHSRNlogLik14<-list(NULL)
for (i in 1:1000){PHSRNlogLik14[[i]]<-logLik(PHSRNlm14[[i]])}

PHSRNlogLik15<-list(NULL)
for (i in 1:1000){PHSRNlogLik15[[i]]<-logLik(PHSRNlm15[[i]])}

PHSRNlogLik16<-list(NULL)
for (i in 1:1000){PHSRNlogLik16[[i]]<-logLik(PHSRNlm16[[i]])}

PHSRNlogLik17<-list(NULL)
for (i in 1:1000){PHSRNlogLik17[[i]]<-logLik(PHSRNlm17[[i]])}

PHSRNlogLik18<-list(NULL)
for (i in 1:1000){PHSRNlogLik18[[i]]<-logLik(PHSRNlm18[[i]])}

PHSRNlogLik19<-list(NULL)
for (i in 1:1000){PHSRNlogLik19[[i]]<-logLik(PHSRNlm19[[i]])}

PHSRNlogLik20<-list(NULL)
for (i in 1:1000){PHSRNlogLik20[[i]]<-logLik(PHSRNlm20[[i]])}

PHSRNlogLik21<-list(NULL)
for (i in 1:1000){PHSRNlogLik21[[i]]<-logLik(PHSRNlm21[[i]])}

PHSRNlogLik22<-list(NULL)
for (i in 1:1000){PHSRNlogLik22[[i]]<-logLik(PHSRNlm22[[i]])}

PHSRNlogLik23<-list(NULL)
for (i in 1:1000){PHSRNlogLik23[[i]]<-logLik(PHSRNlm23[[i]])}

PHSRNlogLik24<-list(NULL)
for (i in 1:1000){PHSRNlogLik24[[i]]<-logLik(PHSRNlm24[[i]])}

PHSRNlogLik25<-list(NULL)
for (i in 1:1000){PHSRNlogLik25[[i]]<-logLik(PHSRNlm25[[i]])}

PHSRNlogLik26<-list(NULL)
for (i in 1:1000){PHSRNlogLik26[[i]]<-logLik(PHSRNlm26[[i]])}

PHSRNlogLik27<-list(NULL)
for (i in 1:1000){PHSRNlogLik27[[i]]<-logLik(PHSRNlm27[[i]])}

PHSRNlogLik28<-list(NULL)
for (i in 1:1000){PHSRNlogLik28[[i]]<-logLik(PHSRNlm28[[i]])}

PHSRNlogLik29<-list(NULL)
for (i in 1:1000){PHSRNlogLik29[[i]]<-logLik(PHSRNlm29[[i]])}

PHSRNlogLik30<-list(NULL)
for (i in 1:1000){PHSRNlogLik30[[i]]<-logLik(PHSRNlm30[[i]])}

PHSRNlogLik31<-list(NULL)
for (i in 1:1000){PHSRNlogLik31[[i]]<-logLik(PHSRNlm31[[i]])}

PHSRNlogLik32<-list(NULL)
for (i in 1:1000){PHSRNlogLik32[[i]]<-logLik(PHSRNlm32[[i]])}

PHSRNlogLik33<-list(NULL)
for (i in 1:1000){PHSRNlogLik33[[i]]<-logLik(PHSRNlm33[[i]])}

PHSRNlogLik34<-list(NULL)
for (i in 1:1000){PHSRNlogLik34[[i]]<-logLik(PHSRNlm34[[i]])}

PHSRNlogLik35<-list(NULL)
for (i in 1:1000){PHSRNlogLik35[[i]]<-logLik(PHSRNlm35[[i]])}

PHSRNlogLik36<-list(NULL)
for (i in 1:1000){PHSRNlogLik36[[i]]<-logLik(PHSRNlm36[[i]])}

PHSRNlogLik37<-list(NULL)
for (i in 1:1000){PHSRNlogLik37[[i]]<-logLik(PHSRNlm37[[i]])}

PHSRNlogLik38<-list(NULL)
for (i in 1:1000){PHSRNlogLik38[[i]]<-logLik(PHSRNlm38[[i]])}

PHSRNlogLik39<-list(NULL)
for (i in 1:1000){PHSRNlogLik39[[i]]<-logLik(PHSRNlm39[[i]])}

PHSRNlogLik40<-list(NULL)
for (i in 1:1000){PHSRNlogLik40[[i]]<-logLik(PHSRNlm40[[i]])}

PHSRNlogLik41<-list(NULL)
for (i in 1:1000){PHSRNlogLik41[[i]]<-logLik(PHSRNlm41[[i]])}

PHSRNlogLik42<-list(NULL)
for (i in 1:1000){PHSRNlogLik42[[i]]<-logLik(PHSRNlm42[[i]])}

PHSRNlogLik43<-list(NULL)
for (i in 1:1000){PHSRNlogLik43[[i]]<-logLik(PHSRNlm43[[i]])}

PHSRNlogLik44<-list(NULL)
for (i in 1:1000){PHSRNlogLik44[[i]]<-logLik(PHSRNlm44[[i]])}

PHSRNlogLik45<-list(NULL)
for (i in 1:1000){PHSRNlogLik45[[i]]<-logLik(PHSRNlm45[[i]])}

PHSRNlogLik46<-list(NULL)
for (i in 1:1000){PHSRNlogLik46[[i]]<-logLik(PHSRNlm46[[i]])}

PHSRNlogLik47<-list(NULL)
for (i in 1:1000){PHSRNlogLik47[[i]]<-logLik(PHSRNlm47[[i]])}

PHSRNlogLik48<-list(NULL)
for (i in 1:1000){PHSRNlogLik48[[i]]<-logLik(PHSRNlm48[[i]])}

PHSRNlogLik49<-list(NULL)
for (i in 1:1000){PHSRNlogLik49[[i]]<-logLik(PHSRNlm49[[i]])}

PHSRNlogLik50<-list(NULL)
for (i in 1:1000){PHSRNlogLik50[[i]]<-logLik(PHSRNlm50[[i]])}

##AIC

PHSRNAIC2<-list(NULL)
for (i in 1:1000){PHSRNAIC2[[i]]<-AIC(PHSRNlogLik2[[i]])}

PHSRNAIC3<-list(NULL)
for (i in 1:1000){PHSRNAIC3[[i]]<-AIC(PHSRNlogLik3[[i]])}

PHSRNAIC4<-list(NULL)
for (i in 1:1000){PHSRNAIC4[[i]]<-AIC(PHSRNlogLik4[[i]])}

PHSRNAIC5<-list(NULL)
for (i in 1:1000){PHSRNAIC5[[i]]<-AIC(PHSRNlogLik5[[i]])}

PHSRNAIC6<-list(NULL)
for (i in 1:1000){PHSRNAIC6[[i]]<-AIC(PHSRNlogLik6[[i]])}

PHSRNAIC7<-list(NULL)
for (i in 1:1000){PHSRNAIC7[[i]]<-AIC(PHSRNlogLik7[[i]])}

PHSRNAIC8<-list(NULL)
for (i in 1:1000){PHSRNAIC8[[i]]<-AIC(PHSRNlogLik8[[i]])}

PHSRNAIC9<-list(NULL)
for (i in 1:1000){PHSRNAIC9[[i]]<-AIC(PHSRNlogLik9[[i]])}

PHSRNAIC10<-list(NULL)
for (i in 1:1000){PHSRNAIC10[[i]]<-AIC(PHSRNlogLik10[[i]])}

PHSRNAIC11<-list(NULL)
for (i in 1:1000){PHSRNAIC11[[i]]<-AIC(PHSRNlogLik11[[i]])}

PHSRNAIC12<-list(NULL)
for (i in 1:1000){PHSRNAIC12[[i]]<-AIC(PHSRNlogLik12[[i]])}

PHSRNAIC13<-list(NULL)
for (i in 1:1000){PHSRNAIC13[[i]]<-AIC(PHSRNlogLik13[[i]])}

PHSRNAIC14<-list(NULL)
for (i in 1:1000){PHSRNAIC14[[i]]<-AIC(PHSRNlogLik14[[i]])}

PHSRNAIC15<-list(NULL)
for (i in 1:1000){PHSRNAIC15[[i]]<-AIC(PHSRNlogLik15[[i]])}

PHSRNAIC16<-list(NULL)
for (i in 1:1000){PHSRNAIC16[[i]]<-AIC(PHSRNlogLik16[[i]])}

PHSRNAIC17<-list(NULL)
for (i in 1:1000){PHSRNAIC17[[i]]<-AIC(PHSRNlogLik17[[i]])}

PHSRNAIC18<-list(NULL)
for (i in 1:1000){PHSRNAIC18[[i]]<-AIC(PHSRNlogLik18[[i]])}

PHSRNAIC19<-list(NULL)
for (i in 1:1000){PHSRNAIC19[[i]]<-AIC(PHSRNlogLik19[[i]])}

PHSRNAIC20<-list(NULL)
for (i in 1:1000){PHSRNAIC20[[i]]<-AIC(PHSRNlogLik20[[i]])}

PHSRNAIC21<-list(NULL)
for (i in 1:1000){PHSRNAIC21[[i]]<-AIC(PHSRNlogLik21[[i]])}

PHSRNAIC22<-list(NULL)
for (i in 1:1000){PHSRNAIC22[[i]]<-AIC(PHSRNlogLik22[[i]])}

PHSRNAIC23<-list(NULL)
for (i in 1:1000){PHSRNAIC23[[i]]<-AIC(PHSRNlogLik23[[i]])}

PHSRNAIC24<-list(NULL)
for (i in 1:1000){PHSRNAIC24[[i]]<-AIC(PHSRNlogLik24[[i]])}

PHSRNAIC25<-list(NULL)
for (i in 1:1000){PHSRNAIC25[[i]]<-AIC(PHSRNlogLik25[[i]])}

PHSRNAIC26<-list(NULL)
for (i in 1:1000){PHSRNAIC26[[i]]<-AIC(PHSRNlogLik26[[i]])}

PHSRNAIC27<-list(NULL)
for (i in 1:1000){PHSRNAIC27[[i]]<-AIC(PHSRNlogLik27[[i]])}

PHSRNAIC28<-list(NULL)
for (i in 1:1000){PHSRNAIC28[[i]]<-AIC(PHSRNlogLik28[[i]])}

PHSRNAIC29<-list(NULL)
for (i in 1:1000){PHSRNAIC29[[i]]<-AIC(PHSRNlogLik29[[i]])}

PHSRNAIC30<-list(NULL)
for (i in 1:1000){PHSRNAIC30[[i]]<-AIC(PHSRNlogLik30[[i]])}

PHSRNAIC31<-list(NULL)
for (i in 1:1000){PHSRNAIC31[[i]]<-AIC(PHSRNlogLik31[[i]])}

PHSRNAIC32<-list(NULL)
for (i in 1:1000){PHSRNAIC32[[i]]<-AIC(PHSRNlogLik32[[i]])}

PHSRNAIC33<-list(NULL)
for (i in 1:1000){PHSRNAIC33[[i]]<-AIC(PHSRNlogLik33[[i]])}

PHSRNAIC34<-list(NULL)
for (i in 1:1000){PHSRNAIC34[[i]]<-AIC(PHSRNlogLik34[[i]])}

PHSRNAIC35<-list(NULL)
for (i in 1:1000){PHSRNAIC35[[i]]<-AIC(PHSRNlogLik35[[i]])}

PHSRNAIC36<-list(NULL)
for (i in 1:1000){PHSRNAIC36[[i]]<-AIC(PHSRNlogLik36[[i]])}

PHSRNAIC37<-list(NULL)
for (i in 1:1000){PHSRNAIC37[[i]]<-AIC(PHSRNlogLik37[[i]])}

PHSRNAIC38<-list(NULL)
for (i in 1:1000){PHSRNAIC38[[i]]<-AIC(PHSRNlogLik38[[i]])}

PHSRNAIC39<-list(NULL)
for (i in 1:1000){PHSRNAIC39[[i]]<-AIC(PHSRNlogLik39[[i]])}

PHSRNAIC40<-list(NULL)
for (i in 1:1000){PHSRNAIC40[[i]]<-AIC(PHSRNlogLik40[[i]])}

PHSRNAIC41<-list(NULL)
for (i in 1:1000){PHSRNAIC41[[i]]<-AIC(PHSRNlogLik41[[i]])}

PHSRNAIC42<-list(NULL)
for (i in 1:1000){PHSRNAIC42[[i]]<-AIC(PHSRNlogLik42[[i]])}

PHSRNAIC43<-list(NULL)
for (i in 1:1000){PHSRNAIC43[[i]]<-AIC(PHSRNlogLik43[[i]])}

PHSRNAIC44<-list(NULL)
for (i in 1:1000){PHSRNAIC44[[i]]<-AIC(PHSRNlogLik44[[i]])}

PHSRNAIC45<-list(NULL)
for (i in 1:1000){PHSRNAIC45[[i]]<-AIC(PHSRNlogLik45[[i]])}

PHSRNAIC46<-list(NULL)
for (i in 1:1000){PHSRNAIC46[[i]]<-AIC(PHSRNlogLik46[[i]])}

PHSRNAIC47<-list(NULL)
for (i in 1:1000){PHSRNAIC47[[i]]<-AIC(PHSRNlogLik47[[i]])}

PHSRNAIC48<-list(NULL)
for (i in 1:1000){PHSRNAIC48[[i]]<-AIC(PHSRNlogLik48[[i]])}

PHSRNAIC49<-list(NULL)
for (i in 1:1000){PHSRNAIC49[[i]]<-AIC(PHSRNlogLik49[[i]])}

PHSRNAIC50<-list(NULL)
for (i in 1:1000){PHSRNAIC50[[i]]<-AIC(PHSRNlogLik50[[i]])}

PHSRNAIC<-list(NULL)
for (i in 1:1000){PHSRNAIC[[i]]<-as.vector(c(PHSRNAIC2[[i]], PHSRNAIC3[[i]], PHSRNAIC4[[i]], PHSRNAIC5[[i]], PHSRNAIC6[[i]], PHSRNAIC7[[i]], PHSRNAIC8[[i]], PHSRNAIC9[[i]], PHSRNAIC10[[i]], PHSRNAIC11[[i]], PHSRNAIC12[[i]], PHSRNAIC13[[i]], PHSRNAIC14[[i]], PHSRNAIC15[[i]], PHSRNAIC16[[i]], PHSRNAIC17[[i]], PHSRNAIC18[[i]], PHSRNAIC19[[i]], PHSRNAIC20[[i]], PHSRNAIC21[[i]], PHSRNAIC22[[i]], PHSRNAIC23[[i]], PHSRNAIC24[[i]], PHSRNAIC25[[i]], PHSRNAIC26[[i]], PHSRNAIC27[[i]], PHSRNAIC28[[i]], PHSRNAIC29[[i]], PHSRNAIC30[[i]], PHSRNAIC31[[i]], PHSRNAIC32[[i]], PHSRNAIC33[[i]], PHSRNAIC34[[i]], PHSRNAIC35[[i]], PHSRNAIC36[[i]], PHSRNAIC37[[i]], PHSRNAIC38[[i]], PHSRNAIC39[[i]], PHSRNAIC40[[i]], PHSRNAIC41[[i]], PHSRNAIC42[[i]],PHSRNAIC43[[i]], PHSRNAIC44[[i]], PHSRNAIC45[[i]], PHSRNAIC46[[i]], PHSRNAIC47[[i]], PHSRNAIC48[[i]], PHSRNAIC49[[i]], PHSRNAIC50[[i]]))}

##Final dataframes

LevelIteration<-(2:50)

dfPHSRNAIC<-list(NULL)
for (i in 1:1000){dfPHSRNRNAIC[[i]]<-data.frame(cbind(LevelIteration, PHSRNAIC[[i]]))}

FinitePHSRNAIC<-list(NULL)
for (i in 1:1000){FinitePHSRNAIC[[i]]<-dfPHSRNAIC[[i]][is.finite(rowSums(dfPHSRNAIC[[i]])),]}

MinAICRN<-list(NULL)
for (i in 1:1000){MinAICRN[[i]]<-FinitePHSRNAIC[[i]][which.min(FinitePHSRNAIC[[i]]$V2),]}

PHSRNFinal=list(NULL)
for (i in 1:1000){PHSRNFinal[[i]]<-(dfPHSRN50[[i]][as.numeric(which(dfPHS50[[i]]$Level == (as.numeric(MinAICRN[[i]]$LevelIteration)))),])$PHS}

setwd("C:/Users/") #Set your working directory where you would like the PHS RDS to save

for (i in 1:1000){saveRDS(PHSRNFinal[[i]], (paste("PHSRNFinal", i, ".txt", sep="")))}

PHSRN<-list(NULL)
for (i in 1:1000){PHSRN[[i]]<-readRDS(paste("PHSRNFinal", i, ".txt", sep=""))}

setwd("C:/Users/") #Set your working directory where you would like the PHS excell spreadsheet to save

PHSRNun<-unlist(PHSRN)
PHSRN<-as.data.frame(PHSRNun)
write_xlsx(PHSRN, "PHSRNFinal.xlsx")

## Step 4: Heterarchy matrix construction 

#The random network metrics were collated into an excell spreadsheet outside of R that also included 
#the number of nodes of each network.

#The Heterarchy matrix was constructed using the modularity and PHS of the 1000 random networks. 
#Before the metrics were ploted on the dual axes they were corrected for both network size and standardised.

#import data and read in 

setwd("C:/Users/") #Set your working directory where you have the collated excell spreadsheet

data<-read.xlsx("Heterarchy Metric Data.xlsx", sheet = 1, startRow = 2, colNames = TRUE);

#Correct for size 

lmPHS = lm(PHS~Number.of.Nodes, data = data) 
resPHS<-lmPHS$residuals

lmMod= lm(Modularity~Number.of.Nodes, data = data)
resMod = lmMod$residuals

theoretical<-as.data.frame(cbind(resPHS, resMod))

#Standardize random data + store mean and SD to apply to real-world networks

data.means=as.data.frame((colMeans(theoretical,na.rm = T)))
data.sds=as.data.frame(apply(theoretical,2,sd))

ModMeanTheo<-data.means$Mod
PHSMeanTheo<-data.means$PHS
ModSDTheo<-data.sds$Mod
PHSSDTheo<-data.sds$PHS

data.s=standardize(theoretical)

PHSTheo=data.s$resPHS
ModTheo=data.s$resMod
datTheo=as.data.frame(cbind(ModTheo,PHSTheo))

Het_Matrix <- ggplot () +
  theme_bw() +
  geom_point(data=datTheo,aes(x=PHS,y=Mod)) +
  theme(axis.text.x=element_text(face="bold",size=12,colour="black")) +
  theme(axis.text.y=element_text(face="bold",size=12,colour="black")) +
  theme(axis.title=element_text(size=14,face="bold")) +
  labs(x="PHS",y="Modularity") +
  geom_hline(colour="red",size=1.5,yintercept=0) +
  geom_vline(colour="red",size=1.5,xintercept=0) +
  annotate("text", x=0, y=-5, label= "Individual", size=5, col="Black") +
  annotate("text", x=0, y=5, label= "Networked", size=5, col="Black") +
  annotate("text", x=1.8, y=0, label= "Hierarchical", size=5, col="Black", angle=-90) +
  annotate("text", x=-1.8, y=0, label= "Flat", size=5, col="Black", angle = 90) +
  annotate("text", x=-1, y=3.75, label= "Reticulated", size=4, col="red") +
  annotate("text", x=1, y=3.75, label= "Polycentric", size=4, col="red") +
  annotate("text", x=1, y=-3.75, label= "Pyramidal", size=4, col="red") +
  annotate("text", x=-1, y=-3.75, label= "Individualistic", size=4, col="red")

windows()
Het_Matrix

setwd("C:/Users/") #Set your working directory where you want the heterarchy matrix to save

ggsave("Heterarchy Matrix.pdf")

```

## Step 5: Calculate Network Metric for a real world network 

Modularity was calculated for a real world network. The real-world network data used in our study was taken from numerous open source repositories.

```{r}

setwd("C:/Users/") #Set your working directory where the real-world network is

RWnet<-read.graph("arenas_email.txt", directed=F) # The way the network is loaded could change depending on its orginal format

cwtrRW<-cluster_walktrap(RWnet)
ModRW<-modularity(RWnet,membership(cwtrRW))

setwd("C:/Users/") #Set your working directory where you want the Modularity RDS to be saved

DataModRW<-unlist(ModRW)
saveRDS(DataModRW, "RW Modularity")

##Step 6: Calculate the PHS for a Real world network

#Using similar steps from the theoretical networks the PHS was calculated for n/2 levels. 
#Ten levels are included in the belwo code. A linear regression and AIC was used to determine the best 
#PHS value to use. The PHS value was then corrected for network size and standardised. 

setwd("C:/Users/") #Set your working directory where the real-world network is

RWNetH<-read.table("MPNetA.txt", sep=" ", header=FALSE,  strip.white=TRUE)

RWNetHedge<-as_edgelist(net)

setwd("C:/Users/") #Set your working directory where the Cheng source code is

source(file="ChengHirNet_functions.R")

setwd("C:/Users/") #Set your working directory where you want the Cheng code output to save

# calculate the hierarchical structure with Lev = n/2

HMRW= cal_hier_score26(RWNetHedge, kmax=10000, ptim=100, anneal.coeff=1e-6,  myoutf = "Hierarchical_Structure_Real_World.txt")

#Read in Cheng Code output

R_WorldHier<-read.table(paste("Hierarchical_Structure_Real_World.txt", sep=""),sep="\t", fill=TRUE)

#Isolate the PHS value

HMRWL2<-subset(R_WorldHier, V1=='Lev=2')

HMRWL3<-subset(R_WorldHier, V1=='Lev=3')

HMRWL4<-subset(R_WorldHier, V1=='Lev=4')

HMRWL5<-subset(R_WorldHier, V1=='Lev=5')

HMRWL6<-subset(R_WorldHier, V1=='Lev=6')

HMRWL7<-subset(R_WorldHier, V1=='Lev=7')

HMRWL8<-subset(R_WorldHier, V1=='Lev=8')

HMRWL9<-subset(R_WorldHier, V1=='Lev=9')

HMRWL10<-subset(R_WorldHier, V1=='Lev=10')

##Create Vector

HMPHSRWL2v<-as.vector(HMRWL2)

HMPHSRWL3v<-as.vector(HMRWL3)

HMPHSRWL4v<-as.vector(HMRWL4)

HMPHSRWL5v<-as.vector(HMRWL5)

HMPHSRWL6v<-as.vector(HMRWL6)

HMPHSRWL7v<-as.vector(HMRWL7)

HMPHSRWL8v<-as.vector(HMRWL8)

HMPHSRWL9v<-as.vector(HMRWL9)

HMPHSRWL10v<-as.vector(HMRWL10)


HMPHSRWL2f<-parse_number(HMPHSRWL2v)

HMPHSRWL3f<-parse_number(HMPHSRWL3v)

HMPHSRWL4f<-parse_number(HMPHSRWL4v)

HMPHSRWL5f<-parse_number(HMPHSRWL5v)

HMPHSRWL6f<-parse_number(HMPHSRWL6v)

HMPHSRWL7f<-parse_number(HMPHSRWL7v)

HMPHSRWL8f<-parse_number(HMPHSRWL8v)

HMPHSRWL9f<-parse_number(HMPHSRWL9v)

HMPHSRWL10f<-parse_number(HMPHSRWL10v)


HMPHSRWVec10<-as.vector(c(HMPHSL2f, HMPHSL3f, HMPHSL4f, HMPHSL5f, HMPHSL6f, HMPHSL7f
                          , HMPHSL8f, HMPHSL9f, HMPHSL10f))

LevelvecRW<-2:10 ##Number of Levels 

dfPHSRW<-data.frame(cbind(LevelvecRW, HMPHSRWVec10))

#Create vectors of level iterations

HMPHSRWVec2<-as.vector(c(HMPHSRWL2f))

HMPHSRWRWVec3<-as.vector(c(HMPHSRWL2f, HMPHSRWL3f))

HMPHSRWRWVec4<-as.vector(c(HMPHSRWL2f, HMPHSRWL3f, HMPHSRWL4f))

HMPHSRWRWVec5<-as.vector(c(HMPHSRWL2f, HMPHSRWL3f, HMPHSRWL4f, HMPHSRWL5f))

HMPHSRWRWVec6<-as.vector(c(HMPHSRWL2f, HMPHSRWL3f, HMPHSRWL4f, HMPHSRWL5f, HMPHSRWL6f))

HMPHSRWRWVec7<-as.vector(c(HMPHSRWL2f, HMPHSRWL3f, HMPHSRWL4f, HMPHSRWL5f, HMPHSRWL6f, HMPHSRWL7f))

HMPHSRWVec8<-as.vector(c(HMPHSRWL2f, HMPHSRWL3f, HMPHSRWL4f, HMPHSRWL5f, HMPHSRWL6f, HMPHSRWL7f, HMPHSRWL8f))

HMPHSRWVec9<-as.vector(c(HMPHSRWL2f, HMPHSRWL3f, HMPHSRWL4f, HMPHSRWL5f, HMPHSRWL6f, HMPHSRWL7, HMPHSRWL8f,
                         HMPHSRWL9f))

HMPHSRWVec10<-as.vector(c(HMPHSRWL2f, HMPHSRWL3f, HMPHSRWL4f, HMPHSRWL5f, HMPHSRWL6f, HMPHSRWL7f, HMPHSRWL8f
                          , HMPHSRWL9f, HMPHSRWL10f))


##data frames
L2<-(2)
L3<-(2:3)
L4<-(2:4)
L5<-(2:5)
L6<-(2:6)
L7<-(2:7)
L8<-(2:8)
L9<-(2:9)
L10<-(2:10)


dfPHSRW2<-data.frame(cbind(L2, HMPHSRWVec2))
colnames(dfPHSRW2)<-c("Level", "PHS")

dfPHSRW3<-data.frame(cbind(L3, HMPHSRWVec3))
colnames(dfPHSRW3)<-c("Level", "PHS")

dfPHSRW4<-data.frame(cbind(L4, HMPHSRWVec4))
colnames(dfPHSRW4)<-c("Level", "PHS")

dfPHSRW5<-data.frame(cbind(L5, HMPHSRWVec5))
colnames(dfPHSRW5)<-c("Level", "PHS")

dfPHSRW6<-data.frame(cbind(L6, HMPHSRWVec6))
colnames(dfPHSRW6)<-c("Level", "PHS")

dfPHSRW7<-data.frame(cbind(L7, HMPHSRWVec7))
colnames(dfPHSRW7)<-c("Level", "PHS")

dfPHSRW8<-data.frame(cbind(L8, HMPHSRWVec8))
colnames(dfPHSRW8)<-c("Level", "PHS")

dfPHSRW9<-data.frame(cbind(L9, HMPHSRWVec9))
colnames(dfPHSRW9)<-c("Level", "PHS")

dfPHSRW10<-data.frame(cbind(L10, HMPHSRWVec10))
colnames(dfPHSRW10)<-c("Level", "PHS")

##linear model 
PHSRWlm2<- lm(PHS~Level , data = dfPHSRW2)

PHSRWlm3<-lm(PHS~Level , data = dfPHSRW3)

PHSRWlm4<-lm(PHS~Level , data = dfPHSRW4)

PHSRWlm5<-lm(PHS~Level , data = dfPHSRW5)

PHSRWlm6<-lm(PHS~Level , data = dfPHSRW6)

PHSRWlm7<-lm(PHS~Level , data = dfPHSRW7)

PHSRWlm8<-lm(PHS~Level , data = dfPHSRW8)

PHSRWlm9<-lm(PHS~Level , data = dfPHSRW9)

PHSRWlm10<-lm(PHS~Level , data = dfPHSRW10)

##Log Liklihood

PHSRWlogLik2<--logLik(PHSRWlm2)

PHSRWlogLik3<-logLik(PHSRWlm3)

PHSRWlogLik4<-logLik(PHSRWlm4)

PHSRWlogLik5<-logLik(PHSRWlm5)

PHSRWlogLik6<-logLik(PHSRWlm6)

PHSRWlogLik7<-logLik(PHSRWlm7)

PHSRWlogLik8<-logLik(PHSRWlm8)

PHSRWlogLik9<-logLik(PHSRWlm9)

PHSRWlogLik10<-logLik(PHSRWlm10)


##AIC
PHSRWAIC2<-AIC(PHSRWlogLik2)

PHSRWAIC3<-AIC(PHSRWlogLik3)

PHSRWAIC4<-AIC(PHSRWlogLik4)

PHSRWAIC5<-AIC(PHSRWlogLik5)

PHSRWAIC6<-AIC(PHSRWlogLik6)

PHSRWAIC7<-AIC(PHSRWlogLik7)

PHSRWAIC8<-AIC(PHSRWlogLik8)

PHSRWAIC9<-AIC(PHSRWlogLik9)

PHSRWAIC10<-AIC(PHSRWlogLik10)


PHSRWAIC10f<-as.vector(c(PHSRWAIC2, PHSRWAIC3, PHSRWAIC4, PHSRWAIC5, PHSRWAIC6, PHSRWAIC7, PHSRWAIC8,
                         PHSRWAIC9, PHSRWAIC10))

##Final dataframes

LevelIteration<-(2:10)

dfPHSAICRWf<-data.frame(cbind(LevelIteration, PHSAICRW10f))

FinitePHSAICRW<-dfPHSAIC[is.finite(rowSums(dfPHSAICRWf)),]

MinAICRW<-FinitePHSAIC[which.min(FinitePHSAIC$PHSAICRW10f),]

PHSFinalRW<-(dfPHS50[as.numeric(which(dfPHSRWf$Level == (as.numeric(MinAICRW$LevelIteration)))),])$PHS

setwd("C:/Users/") #Set your working directory where you would like the PHS RDS to save

saveRDS(PHSFinalRW, (paste("PHSRWFinal.txt", sep="")))

##Step 7: Plot the real-world network on the heterarchy framework 

#The heterarchy metrics of the real-world networks was corrected for size and standardised, 
#and then plotted onto the heterarchy matrix. The metrics were corrected for size by subtracting 
#the predicted value from the linear regression model of the theoretical networks, from the real-world metrics. 
#The moudlarity and PHS metrics were collated in an excell spreadsheet outside of R. 

setwd("C:/Users/") #Set your working directory where the real world excell spread sheet is

data.real<-read.xlsx("Heterarchy Real World Data.xlsx", sheet = 2, startRow = 2, colNames = TRUE)

#Correct for size

Real_World<-data.frame(Number.of.Nodes=10) #Instert the number of nodes here
Real_WorldPHS1<-predict(lmPHS, Real_World, interval = "confidence")
Real_WorldPHS<-as.numeric(Real_WorldPHS1[,1])
Real_WorldMod1<-predict(lmMod, Real_World, interval='confidence')
Real_WorldMod<-as.numeric(Real_WorldMod1[,1])

Real_WorldPHSCor<-(data.real[,]-Real_WorldPHS) #Instert PHS Collumn
Real_WorldModCor<-(data.real[,]-Real_WorldMod) #Insert modularity collumn

#Standardise

Real_WorldModS<-(Real_WorldModCor-ModMeanTheo)/ModSDTheo
Real_WorldPHSS<-(Real_WorldPHSCor-PHSMeanTheo)/PHSSTheo

datReal=as.data.frame(cbind(Real_WorldModS,Real_WorldPHSS))
colnames(datReal)<-c("Mod","PHS")
#Plot the Real wolrds on the heterarchy matrix

RW_Het_Matrix <- ggplot () +
  theme_bw() +
  geom_point(data=datTheo,aes(x=PHS,y=Mod, colour="Theoretical")) +
  geom_point(data=datReal, aes(x=PHS,y=Mod, colour="Real_World"), size=4, show.legend = TRUE) +
  theme(axis.text=element_text(face="bold",size=12,colour="black")) +
  theme(axis.title=element_text(face="bold",size=14,colour="black")) +
  labs(x="PHS",y="Modularity") +
  geom_hline(colour="red",size=1,yintercept=0) +
  geom_vline(colour="red",size=1,xintercept=0) +
  annotate("text", x=0, y=-40, label= "Individual", size=5, col="Black") + 
  annotate("text", x=0, y=76, label= "Networked", size=5, col="Black") +
  annotate("text", x=17, y=0, label= "Hierarchical", size=5, col="Black", angle=-90) +
  annotate("text", x=-17, y=0, label= "Flat", size=5, col="Black", angle = 90) +
  scale_color_manual(name = "Legend", values = c("Theoretical" = "grey48", "Real_World" = "springgreen4")) +
  annotate("text", x=-1.8, y=0, label= "Flat", size=5, col="Black", angle = 90) +
  annotate("text", x=-10, y=63, label= "Reticulated", size=4, col="red") +
  annotate("text", x=10, y=63, label= "Polycentric", size=4, col="red") +
  annotate("text", x=10, y=-28, label= "Pyramidal", size=4, col="red") +
  annotate("text", x=-10, y=-28, label= "Individualistic", size=4, col="red")

windows()
RW_Het_Matrix

##Step 8: Calculate the Euclidean distance of each network from the Centre of the heterarchy matrix and 
#Run Statistical Analyses.

#Theoretical 

mTheo<-as.matrix(datTheo)
colnames(mTheo)<-c("y","x")

cnt<-c(0,0) #Centre of the matrix

Theodist<-apply(mTheo,1,function(x,cnt) {(sqrt((x[1] - cnt[1])^2+(x[2]-cnt[2])^2))},cnt)

#Real-World

mRW<-as.matrix(datReal)
colnames(mFW)<-c("y","x")

cnt<-c(0,0)

RWDist<-apply(mFW,1,function(x,cnt) {(sqrt((x[1] - cnt[1])^2+(x[2]-cnt[2])^2))},cnt)

a <- data.frame(group = "Theoretical", value = Theodist)
b <- data.frame(group = "Real_world", value = RWDist)

Distancedf <- rbind(a, b)
Distancedf<-as.data.frame(Distancedf)

boxDist <- ggplot (Distancedf, aes(x = group, y = value)) +
  theme_bw() +
  geom_boxplot(outlier.shape = NA) + scale_y_continuous(name = "Euclidean distance from centre") +
  scale_x_discrete(name = "Network") +
  theme(axis.text.x=element_text(face="bold",size=12,colour="black", hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=12,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())
windows()
boxDist

setwd("C:/Users/") #Set your working directory where you want the distance box plot to save

ggsave("Distance Box plot.pdf")

#Statistical Analyses

qqplot(Distancedf$value)
normDist<-shapiro.test(Distancedf$value)
LevresultDist = leveneTest(Distancedf$value ~ Distancedf$group, Distancedf)
kw.testDist=kruskal.test(Distancedf$value ~ Distancedf$group, Distancedf1)
dunn.testDist = dunnTest(Distancedf$value ~ Distancedf$group, Distancedf, method="bh")

##Step 9: Calculate the angle (degrees) of each network from the Centre of the heterarchy matrix 
#and run statistical analyses.

#Theoretical 

Theovector1<-atan2(10 - 0, 0 - 0) * 180 / 3.141593  #creating a vector of the axes, numbers were chosen so to include the full range of the axes. The vector is then converted to unit degrees
Theovector2<- atan2((datTheo$Mod) - 0, (datTheo$PHS) -0) * 180 / 3.141593

Theoangle1<- Theovector1 - Theovector2
Theoangle <- ifelse(Theoangle1 < 0, (Theoangle1+360), (Theoangle1+0))

Theoanglev<-as.vector(Theoangle)

##Real World

FWvector1<-atan2(20 - 0, 0 - 0) * 180 / 3.141593
FWvector2<- atan2((datRealRW$Mod) - 0, (datRealRW$PHS) -0) * 180 / 3.141593

FWangle1<- FWvector1 - FWvector2
FWangle<-ifelse(FWangle1<0, (FWangle1+360), (FWangle1+0))

RWAnglev<-as.vector(FWangle)

name<-as.data.frame(Distancedf$group) #using the distance df to get the network grouping names

Anglev<-c(Theoanglev, RWAnglev)

Angledf<- as.data.frame(cbind(name, Anglev))
colnames(Angledf)<-c("group", "value")

boxAngle <- ggplot (Angledf, aes(x = group, y = value), add=TRUE) +
  theme_bw() +
  geom_boxplot(outlier.shape = NA) + scale_y_continuous(name = "Angle from centre (Degrees)") +
  scale_x_discrete(name = "Network") +
  theme(axis.text.x=element_text(face="bold",size=12,colour="black", hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=14,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank()) + geom_hline(yintercept = 180, col= "tomato1", linetype="dashed", size= 1)
+ geom_hline(yintercept = 90, col= "dodgerblue", linetype="dashed", size= 1) + geom_hline(yintercept = 270, col= "springgreen4", linetype="dashed", size= 1) +
  geom_hline(yintercept = 360, col= "gold", linetype="dashed", size= 1) + annotate("text", x=0.515, y=85, label= "Top Right", size=3, col="dodgerblue") + 
  annotate("text", x=0.545, y=175, label= "Bottom Right", size=3, col="tomato1") + annotate("text", x=0.53, y=265, label= "Bottom Left", size=3, col="springgreen4") +
  annotate("text", x=0.5, y=355, label= "Top Left", size=3, col="gold")

windows()
boxAngle


setwd("C:/Users/") #Set your working directory where you want the distance box plot to save

ggsave("Angle Box plot.pdf")

##Statistical Analyses using circular data

Angle<-circular(Angledf$value, type='angles', unit= 'degrees')

HR_test(Angle)  #Hermans-Rasson test

#calculating the absolute average anglular distance between each network groups


AvAngTheo<- mean(Angledf$value[]) #choose the theoretical data
AvAngRW<- mean(Angledf$value[]) #choose the real-world data

AngDistTheoRW<-abs(AvAngTheo-AvAngRW)


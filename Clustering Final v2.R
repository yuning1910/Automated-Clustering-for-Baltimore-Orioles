library(dbscan)
library(ClusterR)
library(readxl)
library(tidyverse)
library(clusterCrit)
library(scatterplot3d)
library(magrittr)
library(WriteXLS)
library(moments)

setwd("C:/Users/Rolandgrad/Documents/MSA Programs/MSA 8350 Analytics Practicum/Orioles Project/Phase 2 clean/NFB2/Cleaned Original Data")

data.org <- read_excel("Cleaned.Clustering.Data.full.xlsx")
head(data.org)


data.org <- data.org[,c(1,4,5,6,3,2)]
head(data.org)

# re-order columns to run the 3d scatterplot function
data <- cbind(Pitcher = data.org[,1],
              break_x = data.org[,3], 
              break_z = data.org[,4],
              velo = data.org[,2],
              throw = data.org[,5],
              ID = data.org[,6])



# generate various list to use is testing
list1 <- c("Carroll, Cody")

list2 <- c("Carroll, Cody", 
           "Lee, Chris", 
           "Wilson, Ryan", 
           "Armstrong, Shawn",
           "Rodriguez, Grayson",
           "Conroy, Ryan",
           "Wright Jr., Mike",
           "Lucas, Josh",
           "Ming, Cameron",
           "Dietz, Matthias",
           "Wells, Alex",
           "Del Rosario, Carlos")
# full player list
list3 <- sort(unique(data.org$Pitcher))
list3



## Warning: Ensure to set working directory folder.  The pitch.happy function will generate plots and a xlsx file for all pitchers. 
setwd("C:/Users/Rolandgrad/Documents/MSA Programs/MSA 8350 Analytics Practicum/Orioles Project/Phase 2 clean/NFB2/outputs")


## Function to cluster pitcher data
pitch.happy <- function(player) {

  data.full <- filter(data, Pitcher == c(player)) #filter player
  data.full.pitch.count <- nrow(data.full) #generate pitch count
  data.full.throw <- data.full[,5] #identify L or R arm
  cluster.data.df <- data.full #data.frame dataset to use
  cluster.data.mx.velo <- as.matrix(cluster.data.df[,c(4)]) #velocity only
  
  # Run PAM k=2
  full.pam2 <- Cluster_Medoids(cluster.data.mx.velo, clusters = 2, distance_metric = "euclidean", swap_phase = TRUE)
  # Generate PAM k=2 c_index
  full.pam2.int <- data.frame(intCriteria(cluster.data.mx.velo, as.integer(full.pam2$clusters),c("c_index")))
  
  #######################################
  ## temp to generate name only (need to review if this function needs to stay)
#  int.name.stop <- names(data.frame(intCriteria(cluster.data.mx.velo, as.integer(full.pam2$clusters),c("all"))))
  
  # Run PAM k=3
  full.pam3 <- Cluster_Medoids(cluster.data.mx.velo, clusters = 3, distance_metric = "euclidean", swap_phase = TRUE)
  # Generate PAM k=3 c_index
  full.pam3.int <- data.frame(intCriteria(cluster.data.mx.velo, as.integer(full.pam3$clusters),c("c_index")))
  
  # Decision step based on c_index
  pam.pick <- if(full.pam2.int > full.pam3.int){full.pam3
    }else{full.pam2}
  # redundent step for output only
  pam.pick2 <- if(full.pam2.int > full.pam3.int){3
    }else{2}
  
  # generate clusters based on c_index Deicision 
  full.data.cluster <- cbind(cluster.data.df, cluster = pam.pick$clusters)
  full.medoid <- data.frame(pam.pick$medoids)
  full.medoid.select <- order(-full.medoid)[1]
  
  # filter out Fastball
  fb.data.cluster.full <- filter(full.data.cluster, cluster == full.medoid.select)
  head(fb.data.cluster.full)
  fb.data.cluster <- fb.data.cluster.full[,c(2,3,4,7)]
  head(fb.data.cluster)
  
  # Filter out Non-Fastball
  nfb.data.cluster.full <- filter(full.data.cluster, cluster != full.medoid.select)
  # Add 10 to Non-Fastballs for easy identification
  nfb.data.cluster.full$cluster <- (nfb.data.cluster.full$cluster + 10)
  head(nfb.data.cluster.full)
  nfb.data.cluster <- nfb.data.cluster.full[,c(2,3,4,7)]
  head(nfb.data.cluster)
  
  # Verify total pitch count
  nrow(fb.data.cluster.full) + nrow(nfb.data.cluster.full)
  data.full.pitch.count

  
################################################################################
#### USER DEFINED FUNCTION SECTION
  
#### Applied to All Sections Begin
  
  output.col.names <- c("Pitcher", "Full_Count", "break_x", "break_z", "velo", "Cluster", "Type", "Count_by_Type")

#### Applied to All Sections End
##############################  

#### Fastball User Defined Funciton Section Begin
  
  ## Fastball hdbscan rule and cluster count
  fb.hdbscan25.rule <- function(go) {
    fb.hdbscan25 <- tryCatch(hdbscan(go[,-4], minPts = 25), error = function(e){0}) #run hdbscan
    fb.hdbscan25.nbr <- tryCatch(max(unique(fb.hdbscan25$cluster)), error = function(e){0}) #generate cluster count
    fb.cluster.nbr.to.use <- if(fb.hdbscan25.nbr > 0){fb.pam.function(fb.hdbscan25.nbr) #hdbscan rule to continue or stop clustering
    } else {fb.single.function()}
    fb.cluster.nbr.to.use #output sent to PAM function(multiple clusters) or to Single function (single cluster) 
  }
  
  ## Fastball Multiple Cluster Output PAM. PAM function is used if fb.hdbscan25.rule function generates a cluster count above zero
  fb.pam.function <- function(go) {
    fb.pam <- Cluster_Medoids(fb.data.cluster[,-4], clusters = go, distance_metric = "euclidean", swap_phase = TRUE) #run pam, k is from the fb.hdbscan25.rule output
    fb.pam.clusters <- fb.pam$clusters #pam clusters
    fb.pam.output <- cbind((player), data.full.pitch.count, fb.data.cluster[,-4], fb.pam.clusters, "FB_M2", fb.count) #combine data and clusters for output
    colnames(fb.pam.output) <- output.col.names #assign uniform column names
    
    # pitcher output
    fb.pam.output
  }
  
  ## Fastball Single Output. Single function is used if the stop rule or hdbscan rule generates a zero cluster. 
  fb.single.function <- function(stop) {
    fb.single.output <- cbind((player), data.full.pitch.count, fb.data.cluster[,-4], 9, "FB_S", fb.count) #combine data and clusters for output
    colnames(fb.single.output) <- output.col.names #assign uniform column names

    # pitcher output
    fb.single.output
  }
#### Fastball User Defined Funciton Section End
  
#### Non-Fastball User Defined Function Section Begin
  
  ## Non-Fastball hdbscan rule and cluster count
  nfb.hdbscan10.rule <- function(go) {
    nfb.hdbscan10 <- tryCatch(hdbscan(go[,-4], minPts = 10), error = function(e){0}) #run hdbscan
    nfb.hdbscan10.nbr <- tryCatch(max(unique(nfb.hdbscan10$cluster)), error = function(e){0}) #generate cluster count
    nfb.cluster.nbr.to.use <- if(nfb.hdbscan10.nbr > 0){nfb2.pam.function(2)} #hdbscan rule to continue or stop clustering
      else {nfb.single.function()}
    nfb.cluster.nbr.to.use #output sent to PAM function(break_x split) or to Single function (single cluster)
  }
  
  ## Non-Fastball Single Output. Single function is used if the stop rule or hdbscan rule generates a zero cluster.
  nfb.single.function <- function(stop) {
    nfb.single.output <- cbind((player), data.full.pitch.count, nfb.data.cluster[,-4], 18, "NFB_S", nfb.count) #combine data and clusters for output
    colnames(nfb.single.output) <- output.col.names #assign uniform column names

    # pitcher output
    nfb.single.output
  }
#### Non-Fastball User Defined Function Section End
  
#### Non-Fastball Stage 2 User Defined Function Section Begin.  
#### Stage two is for pitches with Non-Fastball hdbscan cluster counts above 0  
#### Stage two will split the Non-Fastball along the break_x axis and cluster seperately
    
  ## Non-Fastball Stage 2 split along the break_x
  nfb2.pam.function <- function(go) {

    #### Non-Fastball Stage 2 User Defined Function Begin.  This section has a L & R
    
    #### User defined function NFB2 R Begin  
    
    ## Non-Fastball Stage 2 Right side hdbscan decision rule
    nfb2.R.hdb <- function(go){
      nfb2.R.hdbscan25 <- tryCatch(hdbscan(go[,-4], minPts = 25), error = function(e){0}) #run hdbscan
      nfb2.R.hdbscan25.nbr <- tryCatch(max(unique(nfb2.R.hdbscan25$cluster)), error = function(e){0}) #generate cluster count
      nfb2.R.cluster.nbr.to.use <- if(nfb2.R.hdbscan25.nbr >0){nfb2.R.pam.function(nfb2.R.hdbscan25.nbr)} #hdbscan rule to continue or stop clustering
      else {nfb2.R.single.function()}
      nfb2.R.cluster.nbr.to.use #output sent to PAM function(multiple clusters) or to Single function (single cluster) 
    }
    
    ## Non-Fastball Stage 2 Right side single output    
    nfb2.R.single.function <- function(stop) {
      nfb2.R$cluster <- (nfb2.R$cluster + 23) # assign cluster id, plus 23 used to generate a unique cluster id
      nfb2.R.single.output <- cbind((player), data.full.pitch.count, nfb2.R[,-4], 27, "NFB2.R_S", nfb2.R.count) #combine data and clusters for output
      colnames(nfb2.R.single.output) <- output.col.names #assign uniform column names

      # pitcher output
      nfb2.R.single.output
    }
    
    ## Non-Fastball Stage 2 Right side PAM.  PAM function is used if nfb2.R.hdb function generates a cluster count above zero
    nfb2.R.pam.function <- function(go) {
      nfb2.R.pam <- Cluster_Medoids(nfb2.R[,-4], clusters = go, distance_metric = "euclidean", swap_phase = TRUE) #run pam, k is from the nfb2.R.hdb output
      nfb2.R.pam.clusters <- (nfb2.R.pam$clusters + 23) # assign cluster id, plus 23 used to generate a unique cluster id
      nfb2.R.pam.output <- cbind((player), data.full.pitch.count, nfb2.R[,-4], nfb2.R.pam.clusters, "NFB2.R_M", nfb2.R.count) #combine data and clusters for output
      colnames(nfb2.R.pam.output) <- output.col.names #assign uniform column names
      
      # pitcher output
      nfb2.R.pam.output
    }    
  #### User defined function NFB2 R End  

  #### User defined function NFB2 L Begin
    
    ## Non-Fastball Stage 2 Left side hdbscan decision rule
    nfb2.L.hdb <- function(go){
      nfb2.L.hdbscan25 <- tryCatch(hdbscan(go[,-4], minPts = 25), error = function(e){0}) #run hdbscan
      nfb2.L.hdbscan25.nbr <- tryCatch(max(unique(nfb2.L.hdbscan25$cluster)), error = function(e){0}) #generate cluster count
      nfb2.L.cluster.nbr.to.use <- if(nfb2.L.hdbscan25.nbr >0){nfb2.L.pam.function(nfb2.L.hdbscan25.nbr)} #hdbscan rule to continue or stop clustering
      else {nfb2.L.single.function()}
      nfb2.L.cluster.nbr.to.use #output sent to PAM function(multiple clusters) or to Single function (single cluster) 
    }
    
    ## Non-Fastball Stage 2 Left side single output    
    nfb2.L.single.function <- function(stop) {
      nfb2.L$cluster <- (nfb2.L$cluster + 35) # assign cluster id, plus 35 used to generate a unique cluster id
      nfb2.L.single.output <- cbind((player), data.full.pitch.count, nfb2.L[,-4], 36, "NFB2.L_S", nfb2.L.count)#combine data and clusters for output
      colnames(nfb2.L.single.output) <- output.col.names #assign uniform column names
      
      # pitcher output
      nfb2.L.single.output
    }
    
    ## Non-Fastball Stage 2 Right side PAM. PAM function is used if nfb2.L.hdb function generates a cluster count above zero
    nfb2.L.pam.function <- function(go) {
      nfb2.L.pam <- Cluster_Medoids(nfb2.L[,-4], clusters = go, distance_metric = "euclidean", swap_phase = TRUE) #run pam, k is from the nfb2.L.hdb output
      nfb2.L.pam.clusters <- (nfb2.L.pam$clusters + 35) # assign cluster id, plus 35 used to generate a unique cluster id
      nfb2.L.pam.output <- cbind((player), data.full.pitch.count, nfb2.L[,-4], nfb2.L.pam.clusters, "NFB2.L_M", nfb2.L.count) #combine data and clusters for output
      colnames(nfb2.L.pam.output) <- output.col.names #assign uniform column names
      
      # pitcher output
      nfb2.L.pam.output
    }    
  
    #### User defined function NFB2 L End  
    
    #### Non-Fastball Stage 2 User Defined Function Continuation.  
    #### This section is the main section of nfb2.pam.function <- function(go)
    
    ## This section is run if Non-Fastballs pass through nfb.hdbscan10.rule.  
    nfb2.full.df <- nfb.data.cluster[,-4]
    nfb2.full.df.count <- nrow(nfb2.full.df)
    x.nfb2.mx <- matrix(nfb2.full.df[,1]) # generate a matrix of only break_x

    ## Run pam on Non-Fastball along the break_x axis. go = 2, this is a static number generated in the nfb.hdbscan10.rule function
    x.nfb2.pam <- Cluster_Medoids(x.nfb2.mx, clusters = go, distance_metric = "euclidean", swap_phase = TRUE) 
    nfb2.pam.full <- cbind(nfb2.full.df, cluster = x.nfb2.pam$clusters) #combined Non-Fastball data with clusters based on break_x axis

    ## Identify Right most medoid
    x.nfb2.pam.medoid <- data.frame(x.nfb2.pam$medoids) 
    nfb2.pam.medoid.select <- order(-x.nfb2.pam.medoid)[1]
    
    ## Filter out Non-Fastball Stage 2 Right side
    nfb2.R <- filter(nfb2.pam.full, cluster == nfb2.pam.medoid.select)

    ## Filter out Non-Fastball Stage 2 Left side
    nfb2.L <-filter(nfb2.pam.full, cluster != nfb2.pam.medoid.select)

    #### Non-Fastball Stage 2 Right side decision rule Begin
    
    # Non-Fastball Stage 2 Right side count to be used in the determination metrics
    nfb2.R.count <- nrow(nfb2.R)
    
    # Non-Fastball Stage 2 Right break_x distribution stats and ratios used in the determination metrics
    nfb2.R.x.box.stat <- boxplot.stats(nfb2.R$break_x)
    nfb2.R.x.stat <- data.frame(nfb2.R.x.box.stat$stats)
    nfb2.R.x.median <- nfb2.R.x.stat[3,]
    nfb2.R.x.out <- nfb2.R.x.box.stat$out
    nfb2.R.x.out.above.nbr <- length(nfb2.R.x.out[nfb2.R.x.out > nfb2.R.x.median])
    nfb2.R.x.out.below.nbr <- length(nfb2.R.x.out[nfb2.R.x.out < nfb2.R.x.median])
    nfb2.R.x.col <- rbind(nfb2.R.x.stat, nfb2.R.x.out.above.nbr,nfb2.R.x.out.below.nbr)
    nfb2.R.x.3rd.minus.1st <- (nfb2.R.x.stat[4,] - nfb2.R.x.stat[2,])
    nfb2.R.x.max.minus.min <- (nfb2.R.x.stat[5,] - nfb2.R.x.stat[1,])
    nfb2.R.x.med.minus.1st <- (nfb2.R.x.stat[3,] - nfb2.R.x.stat[2,])
    nfb2.R.x.3rd.minus.med <- (nfb2.R.x.stat[4,] - nfb2.R.x.stat[3,])
    nfb2.R.x.3rd.med.1st.ratio.sqrd <- (max(nfb2.R.x.med.minus.1st, nfb2.R.x.3rd.minus.med)^2 / 
                                       min(nfb2.R.x.med.minus.1st, nfb2.R.x.3rd.minus.med)^2)
    nfb2.R.x.med.minus.min <- (nfb2.R.x.stat[3,] - nfb2.R.x.stat[1,])
    nfb2.R.x.max.minus.med <- (nfb2.R.x.stat[5,] - nfb2.R.x.stat[3,])
    nfb2.R.x.max.med.min.ratio.sqrd <- (max(nfb2.R.x.med.minus.min, nfb2.R.x.max.minus.med)^2 /
                                       min(nfb2.R.x.med.minus.min, nfb2.R.x.max.minus.med)^2)
    nfb2.R.x.out.count.ratio <- (abs(nfb2.R.x.out.above.nbr - nfb2.R.x.out.below.nbr) /
                                nfb2.R.count)
    
    # Non-Fastball Stage 2 Right break_z distribution stats and ratios used in the determination metrics
    nfb2.R.z.box.stat <- boxplot.stats(nfb2.R$break_z)
    nfb2.R.z.stat <- data.frame(nfb2.R.z.box.stat$stats)
    nfb2.R.z.median <- nfb2.R.z.stat[3,]
    nfb2.R.z.out <- nfb2.R.z.box.stat$out
    nfb2.R.z.out.above.nbr <- length(nfb2.R.z.out[nfb2.R.z.out > nfb2.R.z.median])
    nfb2.R.z.out.below.nbr <- length(nfb2.R.z.out[nfb2.R.z.out < nfb2.R.z.median])
    nfb2.R.z.col <- rbind(nfb2.R.z.stat, nfb2.R.z.out.above.nbr, nfb2.R.z.out.below.nbr)
    nfb2.R.z.3rd.minus.1st <- (nfb2.R.z.stat[4,] - nfb2.R.z.stat[2,])
    nfb2.R.z.max.minus.min <- (nfb2.R.z.stat[5,] - nfb2.R.z.stat[1,])
    nfb2.R.z.med.minus.1st <- (nfb2.R.z.stat[3,] - nfb2.R.z.stat[2,])
    nfb2.R.z.3rd.minus.med <- (nfb2.R.z.stat[4,] - nfb2.R.z.stat[3,])
    nfb2.R.z.3rd.med.1st.ratio.sqrd <- (max(nfb2.R.z.med.minus.1st, nfb2.R.z.3rd.minus.med)^2 / 
                                       min(nfb2.R.z.med.minus.1st, nfb2.R.z.3rd.minus.med)^2)
    nfb2.R.z.med.minus.min <- (nfb2.R.z.stat[3,] - nfb2.R.z.stat[1,])
    nfb2.R.z.max.minus.med <- (nfb2.R.z.stat[5,] - nfb2.R.z.stat[3,])
    nfb2.R.z.max.med.min.ratio.sqrd <- (max(nfb2.R.z.med.minus.min, nfb2.R.z.max.minus.med)^2 /
                                       min(nfb2.R.z.med.minus.min, nfb2.R.z.max.minus.med)^2)
    nfb2.R.z.out.count.ratio <- (abs(nfb2.R.z.out.above.nbr - nfb2.R.z.out.below.nbr) /
                                nfb2.R.count)
    
    # Non-Fastball Stage 2 Right velo distribution stats and ratios used in the determination metrics
    nfb2.R.v.box.stat <- boxplot.stats(nfb2.R$velo)
    nfb2.R.v.stat <- data.frame(nfb2.R.v.box.stat$stats)
    nfb2.R.v.median <- nfb2.R.v.stat[3,]
    nfb2.R.v.out <- nfb2.R.v.box.stat$out
    nfb2.R.v.out.above.nbr <- length(nfb2.R.v.out[nfb2.R.v.out > nfb2.R.v.median])
    nfb2.R.v.out.below.nbr <- length(nfb2.R.v.out[nfb2.R.v.out < nfb2.R.v.median])
    nfb2.R.v.col <- rbind(nfb2.R.v.stat, nfb2.R.v.out.above.nbr, nfb2.R.v.out.below.nbr)
    nfb2.R.v.med.minus.1st <- (nfb2.R.v.stat[3,] - nfb2.R.v.stat[2,])
    nfb2.R.v.3rd.minus.med <- (nfb2.R.v.stat[4,] - nfb2.R.v.stat[3,])
    nfb2.R.v.3rd.med.1st.ratio.sqrd <- (max(nfb2.R.v.med.minus.1st, nfb2.R.v.3rd.minus.med)^2 / 
                                       min(nfb2.R.v.med.minus.1st, nfb2.R.v.3rd.minus.med)^2)
    nfb2.R.v.med.minus.min <- (nfb2.R.v.stat[3,] - nfb2.R.v.stat[1,])
    nfb2.R.v.max.minus.med <- (nfb2.R.v.stat[5,] - nfb2.R.v.stat[3,])
    nfb2.R.v.max.med.min.ratio.sqrd <- (max(nfb2.R.v.med.minus.min, nfb2.R.v.max.minus.med)^2 /
                                       min(nfb2.R.v.med.minus.min, nfb2.R.v.max.minus.med)^2)
    nfb2.R.v.out.count.ratio <- (abs(nfb2.R.v.out.above.nbr - nfb2.R.v.out.below.nbr) /
                                nfb2.R.count)
  
    ##### Non-Fastball Stage 2 Right Determination metrics
    
    nfb2.R.x.z.max.minus.min <- max(nfb2.R.z.max.minus.min, nfb2.R.x.max.minus.min)
    nfb2.R.x.z.3rd.minus.1st <- max(nfb2.R.z.3rd.minus.1st, nfb2.R.x.3rd.minus.1st)
    nfb2.R.x.z.max.3rd.med.1st.ratio.sqrd <- max(nfb2.R.x.3rd.med.1st.ratio.sqrd, nfb2.R.z.3rd.med.1st.ratio.sqrd)
    nfb2.R.v.max.3rd.med.1st.ratio.sqrd <- nfb2.R.v.3rd.med.1st.ratio.sqrd
    nfb2.R.x.z.max.3rd.med.1st.ratio.sqrd.ratio <- (max(nfb2.R.x.3rd.med.1st.ratio.sqrd, nfb2.R.z.3rd.med.1st.ratio.sqrd) /
                                                   min(nfb2.R.x.3rd.med.1st.ratio.sqrd, nfb2.R.z.3rd.med.1st.ratio.sqrd))
    nfb2.R.x.z.max.max.med.min.ratio.sqrd <- max(nfb2.R.x.max.med.min.ratio.sqrd, nfb2.R.z.max.med.min.ratio.sqrd)
    nfb2.R.v.max.max.med.min.ratio.sqrd <- nfb2.R.v.max.med.min.ratio.sqrd
    nfb2.R.x.z.max.max.med.min.ratio.sqrd.ratio <- (max(nfb2.R.x.max.med.min.ratio.sqrd, nfb2.R.z.max.med.min.ratio.sqrd) /
                                                   min(nfb2.R.x.max.med.min.ratio.sqrd, nfb2.R.z.max.med.min.ratio.sqrd))
    nfb2.R.x.z.v.max.out.ratio <- max(nfb2.R.x.out.count.ratio, nfb2.R.z.out.count.ratio, nfb2.R.v.out.count.ratio)
    
    ##### Non-Fastball Stage 2 Right Tuning Determination Parameters
    
    maximum.nfb2.R.x.z.max.minus.min <- 25
    maximum.nfb2.R.x.z.3rd.minus.1st <- 25
    maximum.nfb2.R.x.z.max.3rd.med.1st.ratio.sqrd <- 2.2
    maximum.nfb2.R.v.max.3rd.med.1st.ratio.sqrd <- 1.8
    maximum.nfb2.R.x.z.max.3rd.med.1st.ratio.sqrd.ratio <- 2
    maximum.nfb2.R.x.z.max.max.med.min.ratio.sqrd <- 2.2
    maximum.nfb2.R.v.max.max.med.min.ratio.sqrd <- 1.8
    maximum.nfb2.R.x.z.max.max.med.min.ratio.sqrd.ratio <- 2
    maximum.nfb2.R.x.z.v.max.out.ratio <- 0.09
    
    ##### Decision to Stop or Continue clustering Non-Fastball Stage 2 Right
    
    nfb2.R.d1 <- if(nfb2.R.x.z.max.minus.min > maximum.nfb2.R.x.z.max.minus.min){1} else {0}
    nfb2.R.d2 <- if(nfb2.R.x.z.3rd.minus.1st > maximum.nfb2.R.x.z.3rd.minus.1st){1} else {0}
    nfb2.R.d3 <- if(nfb2.R.x.z.max.3rd.med.1st.ratio.sqrd > maximum.nfb2.R.x.z.max.3rd.med.1st.ratio.sqrd){1} else {0}
    nfb2.R.d4 <- if(nfb2.R.v.max.3rd.med.1st.ratio.sqrd > maximum.nfb2.R.v.max.3rd.med.1st.ratio.sqrd){1} else {0}
    nfb2.R.d5 <- if(nfb2.R.x.z.max.3rd.med.1st.ratio.sqrd.ratio > maximum.nfb2.R.x.z.max.3rd.med.1st.ratio.sqrd.ratio){1} else {0}
    nfb2.R.d6 <- if(nfb2.R.x.z.max.max.med.min.ratio.sqrd > maximum.nfb2.R.x.z.max.max.med.min.ratio.sqrd){1} else {0}
    nfb2.R.d7 <- if(nfb2.R.v.max.max.med.min.ratio.sqrd > maximum.nfb2.R.v.max.max.med.min.ratio.sqrd){1} else {0}
    nfb2.R.d8 <- if(nfb2.R.x.z.max.max.med.min.ratio.sqrd.ratio > maximum.nfb2.R.x.z.max.max.med.min.ratio.sqrd.ratio){1} else {0}
    nfb2.R.d9 <- if(nfb2.R.x.z.v.max.out.ratio > maximum.nfb2.R.x.z.v.max.out.ratio){1} else {0}
    
    nfb2.R.stop.go.rule <- if(sum(nfb2.R.d1,nfb2.R.d2,nfb2.R.d3,nfb2.R.d4,nfb2.R.d5,nfb2.R.d6,nfb2.R.d7,nfb2.R.d8,nfb2.R.d9) == 0){nfb2.R.single.function()} else {nfb2.R.hdb(nfb2.R)}
    nfb2.R.stop.go.rule # these rules will feed into the Stage 2 Right side user defined section above
    
    #### Non-Fastball Stage 2 Right side decision rule End

    #### Non-Fastball Stage 2 Left side decision rule Begin
    
    # Non-Fastball Stage 2 Left side count to be used in the determination metrics
    nfb2.L.count <- nrow(nfb2.L)
    
    # Non-Fastball Stage 2 Left break_x distribution stats and ratios used in the determination metrics
    nfb2.L.x.box.stat <- boxplot.stats(nfb2.L$break_x)
    nfb2.L.x.stat <- data.frame(nfb2.L.x.box.stat$stats)
    nfb2.L.x.median <- nfb2.L.x.stat[3,]
    nfb2.L.x.out <- nfb2.L.x.box.stat$out
    nfb2.L.x.out.above.nbr <- length(nfb2.L.x.out[nfb2.L.x.out > nfb2.L.x.median])
    nfb2.L.x.out.below.nbr <- length(nfb2.L.x.out[nfb2.L.x.out < nfb2.L.x.median])
    nfb2.L.x.col <- rbind(nfb2.L.x.stat, nfb2.L.x.out.above.nbr,nfb2.L.x.out.below.nbr)
    nfb2.L.x.3rd.minus.1st <- (nfb2.L.x.stat[4,] - nfb2.L.x.stat[2,])
    nfb2.L.x.max.minus.min <- (nfb2.L.x.stat[5,] - nfb2.L.x.stat[1,])
    nfb2.L.x.med.minus.1st <- (nfb2.L.x.stat[3,] - nfb2.L.x.stat[2,])
    nfb2.L.x.3rd.minus.med <- (nfb2.L.x.stat[4,] - nfb2.L.x.stat[3,])
    nfb2.L.x.3rd.med.1st.ratio.sqrd <- (max(nfb2.L.x.med.minus.1st, nfb2.L.x.3rd.minus.med)^2 / 
                                          min(nfb2.L.x.med.minus.1st, nfb2.L.x.3rd.minus.med)^2)
    nfb2.L.x.med.minus.min <- (nfb2.L.x.stat[3,] - nfb2.L.x.stat[1,])
    nfb2.L.x.max.minus.med <- (nfb2.L.x.stat[5,] - nfb2.L.x.stat[3,])
    nfb2.L.x.max.med.min.ratio.sqrd <- (max(nfb2.L.x.med.minus.min, nfb2.L.x.max.minus.med)^2 /
                                          min(nfb2.L.x.med.minus.min, nfb2.L.x.max.minus.med)^2)
    nfb2.L.x.out.count.ratio <- (abs(nfb2.L.x.out.above.nbr - nfb2.L.x.out.below.nbr) /
                                   nfb2.L.count)
    
    # Non-Fastball Stage 2 Left break_z distribution stats and ratios used in the determination metrics
    nfb2.L.z.box.stat <- boxplot.stats(nfb2.L$break_z)
    nfb2.L.z.stat <- data.frame(nfb2.L.z.box.stat$stats)
    nfb2.L.z.median <- nfb2.L.z.stat[3,]
    nfb2.L.z.out <- nfb2.L.z.box.stat$out
    nfb2.L.z.out.above.nbr <- length(nfb2.L.z.out[nfb2.L.z.out > nfb2.L.z.median])
    nfb2.L.z.out.below.nbr <- length(nfb2.L.z.out[nfb2.L.z.out < nfb2.L.z.median])
    nfb2.L.z.col <- rbind(nfb2.L.z.stat, nfb2.L.z.out.above.nbr, nfb2.L.z.out.below.nbr)
    nfb2.L.z.3rd.minus.1st <- (nfb2.L.z.stat[4,] - nfb2.L.z.stat[2,])
    nfb2.L.z.max.minus.min <- (nfb2.L.z.stat[5,] - nfb2.L.z.stat[1,])
    nfb2.L.z.med.minus.1st <- (nfb2.L.z.stat[3,] - nfb2.L.z.stat[2,])
    nfb2.L.z.3rd.minus.med <- (nfb2.L.z.stat[4,] - nfb2.L.z.stat[3,])
    nfb2.L.z.3rd.med.1st.ratio.sqrd <- (max(nfb2.L.z.med.minus.1st, nfb2.L.z.3rd.minus.med)^2 / 
                                          min(nfb2.L.z.med.minus.1st, nfb2.L.z.3rd.minus.med)^2)
    nfb2.L.z.med.minus.min <- (nfb2.L.z.stat[3,] - nfb2.L.z.stat[1,])
    nfb2.L.z.max.minus.med <- (nfb2.L.z.stat[5,] - nfb2.L.z.stat[3,])
    nfb2.L.z.max.med.min.ratio.sqrd <- (max(nfb2.L.z.med.minus.min, nfb2.L.z.max.minus.med)^2 /
                                          min(nfb2.L.z.med.minus.min, nfb2.L.z.max.minus.med)^2)
    nfb2.L.z.out.count.ratio <- (abs(nfb2.L.z.out.above.nbr - nfb2.L.z.out.below.nbr) /
                                   nfb2.L.count)
    
    # Non-Fastball Stage 2 Left velo distribution stats and ratios used in the determination metrics
    nfb2.L.v.box.stat <- boxplot.stats(nfb2.L$velo)
    nfb2.L.v.stat <- data.frame(nfb2.L.v.box.stat$stats)
    nfb2.L.v.median <- nfb2.L.v.stat[3,]
    nfb2.L.v.out <- nfb2.L.v.box.stat$out
    nfb2.L.v.out.above.nbr <- length(nfb2.L.v.out[nfb2.L.v.out > nfb2.L.v.median])
    nfb2.L.v.out.below.nbr <- length(nfb2.L.v.out[nfb2.L.v.out < nfb2.L.v.median])
    nfb2.L.v.col <- rbind(nfb2.L.v.stat, nfb2.L.v.out.above.nbr, nfb2.L.v.out.below.nbr)
    nfb2.L.v.med.minus.1st <- (nfb2.L.v.stat[3,] - nfb2.L.v.stat[2,])
    nfb2.L.v.3rd.minus.med <- (nfb2.L.v.stat[4,] - nfb2.L.v.stat[3,])
    nfb2.L.v.3rd.med.1st.ratio.sqrd <- (max(nfb2.L.v.med.minus.1st, nfb2.L.v.3rd.minus.med)^2 / 
                                          min(nfb2.L.v.med.minus.1st, nfb2.L.v.3rd.minus.med)^2)
    nfb2.L.v.med.minus.min <- (nfb2.L.v.stat[3,] - nfb2.L.v.stat[1,])
    nfb2.L.v.max.minus.med <- (nfb2.L.v.stat[5,] - nfb2.L.v.stat[3,])
    nfb2.L.v.max.med.min.ratio.sqrd <- (max(nfb2.L.v.med.minus.min, nfb2.L.v.max.minus.med)^2 /
                                          min(nfb2.L.v.med.minus.min, nfb2.L.v.max.minus.med)^2)
    nfb2.L.v.out.count.ratio <- (abs(nfb2.L.v.out.above.nbr - nfb2.L.v.out.below.nbr) /
                                   nfb2.L.count)
    
    ##### Non-Fastball Stage 2 Left Determination metrics
    
    nfb2.L.x.z.max.minus.min <- max(nfb2.L.z.max.minus.min, nfb2.L.x.max.minus.min)
    nfb2.L.x.z.3rd.minus.1st <- max(nfb2.L.z.3rd.minus.1st, nfb2.L.x.3rd.minus.1st)
    nfb2.L.x.z.max.3rd.med.1st.ratio.sqrd <- max(nfb2.L.x.3rd.med.1st.ratio.sqrd, nfb2.L.z.3rd.med.1st.ratio.sqrd)
    nfb2.L.v.max.3rd.med.1st.ratio.sqrd <- nfb2.L.v.3rd.med.1st.ratio.sqrd
    nfb2.L.x.z.max.3rd.med.1st.ratio.sqrd.ratio <- (max(nfb2.L.x.3rd.med.1st.ratio.sqrd, nfb2.L.z.3rd.med.1st.ratio.sqrd) /
                                                      min(nfb2.L.x.3rd.med.1st.ratio.sqrd, nfb2.L.z.3rd.med.1st.ratio.sqrd))
    nfb2.L.x.z.max.max.med.min.ratio.sqrd <- max(nfb2.L.x.max.med.min.ratio.sqrd, nfb2.L.z.max.med.min.ratio.sqrd)
    nfb2.L.v.max.max.med.min.ratio.sqrd <- nfb2.L.v.max.med.min.ratio.sqrd
    nfb2.L.x.z.max.max.med.min.ratio.sqrd.ratio <- (max(nfb2.L.x.max.med.min.ratio.sqrd, nfb2.L.z.max.med.min.ratio.sqrd) /
                                                      min(nfb2.L.x.max.med.min.ratio.sqrd, nfb2.L.z.max.med.min.ratio.sqrd))
    nfb2.L.x.z.v.max.out.ratio <- max(nfb2.L.x.out.count.ratio, nfb2.L.z.out.count.ratio, nfb2.L.v.out.count.ratio)
    
    ##### Non-Fastball Stage 2 Left Tuning Determination Parameters
    
    maximum.nfb2.L.x.z.max.minus.min <- 25
    maximum.nfb2.L.x.z.3rd.minus.1st <- 25
    maximum.nfb2.L.x.z.max.3rd.med.1st.ratio.sqrd <- 2.2
    maximum.nfb2.L.v.max.3rd.med.1st.ratio.sqrd <- 1.8
    maximum.nfb2.L.x.z.max.3rd.med.1st.ratio.sqrd.ratio <- 2
    maximum.nfb2.L.x.z.max.max.med.min.ratio.sqrd <- 2.2
    maximum.nfb2.L.v.max.max.med.min.ratio.sqrd <- 1.8
    maximum.nfb2.L.x.z.max.max.med.min.ratio.sqrd.ratio <- 2
    maximum.nfb2.L.x.z.v.max.out.ratio <- 0.09
    
    ##### Decision to Stop or Continue clustering Non-Fastball Stage 2 Left
    
    nfb2.L.d1 <- if(nfb2.L.x.z.max.minus.min > maximum.nfb2.L.x.z.max.minus.min){1} else {0}
    nfb2.L.d2 <- if(nfb2.L.x.z.3rd.minus.1st > maximum.nfb2.L.x.z.3rd.minus.1st){1} else {0}
    nfb2.L.d3 <- if(nfb2.L.x.z.max.3rd.med.1st.ratio.sqrd > maximum.nfb2.L.x.z.max.3rd.med.1st.ratio.sqrd){1} else {0}
    nfb2.L.d4 <- if(nfb2.L.v.max.3rd.med.1st.ratio.sqrd > maximum.nfb2.L.v.max.3rd.med.1st.ratio.sqrd){1} else {0}
    nfb2.L.d5 <- if(nfb2.L.x.z.max.3rd.med.1st.ratio.sqrd.ratio > maximum.nfb2.L.x.z.max.3rd.med.1st.ratio.sqrd.ratio){1} else {0}
    nfb2.L.d6 <- if(nfb2.L.x.z.max.max.med.min.ratio.sqrd > maximum.nfb2.L.x.z.max.max.med.min.ratio.sqrd){1} else {0}
    nfb2.L.d7 <- if(nfb2.L.v.max.max.med.min.ratio.sqrd > maximum.nfb2.L.v.max.max.med.min.ratio.sqrd){1} else {0}
    nfb2.L.d8 <- if(nfb2.L.x.z.max.max.med.min.ratio.sqrd.ratio > maximum.nfb2.L.x.z.max.max.med.min.ratio.sqrd.ratio){1} else {0}
    nfb2.L.d9 <- if(nfb2.L.x.z.v.max.out.ratio > maximum.nfb2.L.x.z.v.max.out.ratio){1} else {0}
    
    nfb2.L.stop.go.rule <- if(sum(nfb2.L.d1,nfb2.L.d2,nfb2.L.d3,nfb2.L.d4,nfb2.L.d5,nfb2.L.d6,nfb2.L.d7,nfb2.L.d8,nfb2.L.d9) == 0){nfb2.L.single.function()} else {nfb2.L.hdb(nfb2.L)}
    nfb2.L.stop.go.rule # these rules will feed into the Stage 2 Left side user defined section above
    
    #### Non-Fastball Stage 2 Left side decision rule End
    
    ## Combine NFB2.L output and NFB2.R output
    nfb2.L.and.nfb2.R.output <- rbind(nfb2.L.stop.go.rule, nfb2.R.stop.go.rule)
    nfb2.L.and.nfb2.R.row.count <- nrow(nfb2.L.and.nfb2.R.output)
    nfb2.L.and.nfb2.R.cluster.count <- length(unique(nfb2.L.and.nfb2.R.output$Cluster))
    
    ## Non-Fastball Stage 2 output
    nfb2.L.and.nfb2.R.output
  }

  #### Fastball and Non-Fastball Decsion Rule seciton 
  
  ##### Fastball Stop Decision Rule Begin
  
  # Fastbll count to be used in the determination metrics
  fb.count <- nrow(fb.data.cluster)
  
  # Fastball break_x distribution stats and ratios used in the determination metrics
  fb.x.box.stat <- boxplot.stats(fb.data.cluster$break_x)
  fb.x.stat <- data.frame(fb.x.box.stat$stats)
  fb.x.median <- fb.x.stat[3,]
  fb.x.out <- fb.x.box.stat$out
  fb.x.out.above.nbr <- length(fb.x.out[fb.x.out > fb.x.median])
  fb.x.out.below.nbr <- length(fb.x.out[fb.x.out < fb.x.median])
  fb.x.col <- rbind(fb.x.stat, fb.x.out.above.nbr, fb.x.out.below.nbr)
  fb.x.3rd.minus.1st <- (fb.x.stat[4,] - fb.x.stat[2,])
  fb.x.max.minus.min <- (fb.x.stat[5,] - fb.x.stat[1,])
  fb.x.med.minus.1st <- (fb.x.stat[3,] - fb.x.stat[2,])
  fb.x.3rd.minus.med <- (fb.x.stat[4,] - fb.x.stat[3,])
  fb.x.3rd.med.1st.ratio.sqrd <- (max(fb.x.med.minus.1st, fb.x.3rd.minus.med)^2 /
                                    min(fb.x.med.minus.1st, fb.x.3rd.minus.med)^2)
  fb.x.med.minus.min <- (fb.x.stat[3,] - fb.x.stat[1,])
  fb.x.max.minus.med <- (fb.x.stat[5,] - fb.x.stat[3,])
  fb.x.max.med.min.ratio.sqrd <- (max(fb.x.med.minus.min, fb.x.max.minus.med)^2 /
                                    min(fb.x.med.minus.min, fb.x.max.minus.med)^2)
  fb.x.out.count.ratio <- (abs(fb.x.out.above.nbr - fb.x.out.below.nbr) /
                             fb.count)
  
  # Fastball break_z distribution stats and ratios used in the determination metrics
  fb.z.box.stat <- boxplot.stats(fb.data.cluster$break_z)
  fb.z.stat <- data.frame(fb.z.box.stat$stats)
  fb.z.median <- fb.z.stat[3,]
  fb.z.out <- fb.z.box.stat$out
  fb.z.out.above.nbr <- length(fb.z.out[fb.z.out > fb.z.median])
  fb.z.out.below.nbr <- length(fb.z.out[fb.z.out < fb.z.median])
  fb.z.col <- rbind(fb.z.stat, fb.z.out.above.nbr, fb.z.out.below.nbr)
  fb.z.3rd.minus.1st <- (fb.z.stat[4,] - fb.z.stat[2,])
  fb.z.max.minus.min <- (fb.z.stat[5,] - fb.z.stat[1,])
  fb.z.med.minus.1st <- (fb.z.stat[3,] - fb.z.stat[2,])
  fb.z.3rd.minus.med <- (fb.z.stat[4,] - fb.z.stat[3,])
  fb.z.3rd.med.1st.ratio.sqrd <- (max(fb.z.med.minus.1st, fb.z.3rd.minus.med)^2 /
                                    min(fb.z.med.minus.1st, fb.z.3rd.minus.med)^2)
  fb.z.med.minus.min <- (fb.z.stat[3,] - fb.z.stat[1,])
  fb.z.max.minus.med <- (fb.z.stat[5,] - fb.z.stat[3,])
  fb.z.max.med.min.ratio.sqrd <- (max(fb.z.med.minus.min, fb.z.max.minus.med)^2 /
                                    min(fb.z.med.minus.min, fb.z.max.minus.med)^2)
  fb.z.out.count.ratio <- (abs(fb.z.out.above.nbr - fb.z.out.below.nbr) /
                             fb.count)
  
  # Fastball velo distribution stats and ratios used in the determination metrics
  fb.v.box.stat <- boxplot.stats(fb.data.cluster$velo)
  fb.v.stat <- data.frame(fb.v.box.stat$stats)
  fb.v.median <- fb.v.stat[3,]
  fb.v.out <- fb.v.box.stat$out
  fb.v.out.above.nbr <- length(fb.v.out[fb.v.out > fb.v.median])
  fb.v.out.below.nbr <- length(fb.v.out[fb.v.out < fb.v.median])
  fb.v.col <- rbind(fb.v.stat, fb.v.out.above.nbr, fb.v.out.below.nbr)
  fb.v.3rd.minus.1st <- (fb.v.stat[4,] - fb.v.stat[2,])
  fb.v.max.minus.min <- (fb.v.stat[5,] - fb.v.stat[1,])
  fb.v.med.minus.1st <- (fb.v.stat[3,] - fb.v.stat[2,])
  fb.v.3rd.minus.med <- (fb.v.stat[4,] - fb.v.stat[3,])
  fb.v.3rd.med.1st.ratio.sqrd <- (max(fb.v.med.minus.1st, fb.v.3rd.minus.med)^2 /
                                    min(fb.v.med.minus.1st, fb.v.3rd.minus.med)^2)
  fb.v.med.minus.min <- (fb.v.stat[3,] - fb.v.stat[1,])
  fb.v.max.minus.med <- (fb.v.stat[5,] - fb.v.stat[3,])
  fb.v.max.med.min.ratio.sqrd <- (max(fb.v.med.minus.min, fb.v.max.minus.med)^2 /
                                    min(fb.v.med.minus.min, fb.v.max.minus.med)^2)
  fb.v.out.count.ratio <- (abs(fb.v.out.above.nbr - fb.v.out.below.nbr) /
                             fb.count)
  
  ##### Fastball Determination metrics
  fb.x.z.max.minus.min <- max(fb.z.max.minus.min, fb.x.max.minus.min)
  fb.x.z.3rd.minus.1st <- max(fb.z.3rd.minus.1st, fb.x.3rd.minus.1st)
  fb.x.z.max.3rd.med.1st.ratio.sqrd <- max(fb.x.3rd.med.1st.ratio.sqrd, fb.z.3rd.med.1st.ratio.sqrd)
  fb.v.max.3rd.med.1st.ratio.sqrd <- fb.v.3rd.med.1st.ratio.sqrd
  fb.x.z.max.3rd.med.1st.ratio.sqrd.ratio <- (max(fb.x.3rd.med.1st.ratio.sqrd, fb.z.3rd.med.1st.ratio.sqrd) /
                                                min(fb.x.3rd.med.1st.ratio.sqrd, fb.z.3rd.med.1st.ratio.sqrd))
  fb.x.z.max.max.med.min.ratio.sqrd <- max(fb.x.max.med.min.ratio.sqrd, fb.z.max.med.min.ratio.sqrd)
  fb.v.max.max.med.min.ratio.sqrd <- fb.v.max.med.min.ratio.sqrd
  fb.x.z.max.max.med.min.ratio.sqrd.ratio <- (max(fb.x.max.med.min.ratio.sqrd, fb.z.max.med.min.ratio.sqrd) /
                                                min(fb.x.max.med.min.ratio.sqrd, fb.z.max.med.min.ratio.sqrd))
  fb.x.z.v.max.out.ratio <- max(fb.x.out.count.ratio, fb.z.out.count.ratio, fb.v.out.count.ratio)
  
  ##### Fastball Tuning Determination Parameters
  
  maximum.fb.x.z.max.minus.min <- 21
  maximum.fb.x.z.3rd.minus.1st <- 7
  maximum.fb.x.z.max.3rd.med.1st.ratio.sqrd <- 2.2
  maximum.fb.v.max.3rd.med.1st.ratio.sqrd <- 1.8
  maximum.fb.x.z.max.3rd.med.1st.ratio.sqrd.ratio <- 2
  maximum.fb.x.z.max.max.med.min.ratio.sqrd <- 2.2
  maximum.fb.v.max.max.med.min.ratio.sqrd <- 1.8
  maximum.fb.x.z.max.max.med.min.ratio.sqrd.ratio <- 2
  maximum.fb.x.z.v.max.out.ratio <- 0.09
  
  ##### Fastball Decision to Stop or Continue clustering
  
  fb.d1 <- if(fb.x.z.max.minus.min > maximum.fb.x.z.max.minus.min){1} else {0}
  fb.d2 <- if(fb.x.z.3rd.minus.1st > maximum.fb.x.z.3rd.minus.1st){1} else {0}
  fb.d3 <- if(fb.x.z.max.3rd.med.1st.ratio.sqrd > maximum.fb.x.z.max.3rd.med.1st.ratio.sqrd){1} else {0}
  fb.d4 <- if(fb.v.max.3rd.med.1st.ratio.sqrd > maximum.fb.v.max.3rd.med.1st.ratio.sqrd){1} else {0}
  fb.d5 <- if(fb.x.z.max.3rd.med.1st.ratio.sqrd.ratio > maximum.fb.x.z.max.3rd.med.1st.ratio.sqrd.ratio){1} else {0}
  fb.d6 <- if(fb.x.z.max.max.med.min.ratio.sqrd > maximum.fb.x.z.max.max.med.min.ratio.sqrd){1} else {0}
  fb.d7 <- if(fb.v.max.max.med.min.ratio.sqrd > maximum.fb.v.max.max.med.min.ratio.sqrd){1} else {0}
  fb.d8 <- if(fb.x.z.max.max.med.min.ratio.sqrd.ratio > maximum.fb.x.z.max.max.med.min.ratio.sqrd.ratio){1} else {0}
  fb.d9 <- if(fb.x.z.v.max.out.ratio > maximum.fb.x.z.v.max.out.ratio){1} else {0}
  
  # these rules will feed into the Fastball user defined section above
  fb.stop.go.rule <- if(sum(fb.d1,fb.d2,fb.d3,fb.d4,fb.d5,fb.d6,fb.d7,fb.d8,fb.d9) == 0){fb.single.function()} else {fb.hdbscan25.rule(fb.data.cluster)} 
  
  #### Fastball Stop Decision Rule End

  #### Non-Fastball stop decision rule Begin
  
  # Non-Fastball count to be used in the determination metrics
  nfb.count <- nrow(nfb.data.cluster)
  
  # Non-Fastball break_x distribution stats and ratios used in the determination metrics
  nfb.x.box.stat <- boxplot.stats(nfb.data.cluster$break_x)
  nfb.x.stat <- data.frame(nfb.x.box.stat$stats)
  nfb.x.median <- nfb.x.stat[3,]
  nfb.x.out <- nfb.x.box.stat$out
  nfb.x.out.above.nbr <- length(nfb.x.out[nfb.x.out > nfb.x.median])
  nfb.x.out.below.nbr <- length(nfb.x.out[nfb.x.out < nfb.x.median])
  nfb.x.col <- rbind(nfb.x.stat, nfb.x.out.above.nbr, nfb.x.out.below.nbr)
  nfb.x.3rd.minus.1st <- (nfb.x.stat[4,] - nfb.x.stat[2,]) 
  nfb.x.max.minus.min <- (nfb.x.stat[5,] - nfb.x.stat[1,]) 
  nfb.x.med.minus.1st <- (nfb.x.stat[3,] - nfb.x.stat[2,])
  nfb.x.3rd.minus.med <- (nfb.x.stat[4,] - nfb.x.stat[3,])
  nfb.x.3rd.med.1st.ratio.sqrd <- (max(nfb.x.med.minus.1st, nfb.x.3rd.minus.med)^2 / 
                                 min(nfb.x.med.minus.1st, nfb.x.3rd.minus.med)^2)
  nfb.x.med.minus.min <- (nfb.x.stat[3,] - nfb.x.stat[1,])
  nfb.x.max.minus.med <- (nfb.x.stat[5,] - nfb.x.stat[3,])
  nfb.x.max.med.min.ratio.sqrd <- (max(nfb.x.med.minus.min, nfb.x.max.minus.med)^2 /
                                 min(nfb.x.med.minus.min, nfb.x.max.minus.med)^2)
  nfb.x.out.count.ratio <- (abs(nfb.x.out.above.nbr - nfb.x.out.below.nbr) /
                          nfb.count)

  # Non-Fastball break_z distribution stats and ratios used in the determination metrics
  nfb.z.box.stat <- boxplot.stats(nfb.data.cluster$break_z)
  nfb.z.stat <- data.frame(nfb.z.box.stat$stats)
  nfb.z.median <- nfb.z.stat[3,]
  nfb.z.out <- nfb.z.box.stat$out
  nfb.z.out.above.nbr <- length(nfb.z.out[nfb.z.out > nfb.z.median])
  nfb.z.out.below.nbr <- length(nfb.z.out[nfb.z.out < nfb.z.median])
  nfb.z.col <- rbind(nfb.z.stat, nfb.z.out.above.nbr, nfb.z.out.below.nbr)
  nfb.z.3rd.minus.1st <- (nfb.z.stat[4,] - nfb.z.stat[2,])
  nfb.z.max.minus.min <- (nfb.z.stat[5,] - nfb.z.stat[1,])
  nfb.z.med.minus.1st <- (nfb.z.stat[3,] - nfb.z.stat[2,])
  nfb.z.3rd.minus.med <- (nfb.z.stat[4,] - nfb.z.stat[3,])
  nfb.z.3rd.med.1st.ratio.sqrd <- (max(nfb.z.med.minus.1st, nfb.z.3rd.minus.med)^2 / 
                                 min(nfb.z.med.minus.1st, nfb.z.3rd.minus.med)^2)
  nfb.z.med.minus.min <- (nfb.z.stat[3,] - nfb.z.stat[1,])
  nfb.z.max.minus.med <- (nfb.z.stat[5,] - nfb.z.stat[3,])
  nfb.z.max.med.min.ratio.sqrd <- (max(nfb.z.med.minus.min, nfb.z.max.minus.med)^2 /
                                 min(nfb.z.med.minus.min, nfb.z.max.minus.med)^2)
  nfb.z.out.count.ratio <- (abs(nfb.z.out.above.nbr - nfb.z.out.below.nbr) /
                    nfb.count)

  # Non-Fastball velo distribution stats and ratios used in the determination metrics
  nfb.v.box.stat <- boxplot.stats(nfb.data.cluster$velo)
  nfb.v.stat <- data.frame(nfb.v.box.stat$stats)
  nfb.v.median <- nfb.v.stat[3,]
  nfb.v.out <- nfb.v.box.stat$out
  nfb.v.out.above.nbr <- length(nfb.v.out[nfb.v.out > nfb.v.median])
  nfb.v.out.below.nbr <- length(nfb.v.out[nfb.v.out < nfb.v.median])
  nfb.v.col <- rbind(nfb.v.stat, nfb.v.out.above.nbr, nfb.v.out.below.nbr)
  nfb.v.med.minus.1st <- (nfb.v.stat[3,] - nfb.v.stat[2,])
  nfb.v.3rd.minus.med <- (nfb.v.stat[4,] - nfb.v.stat[3,])
  nfb.v.3rd.med.1st.ratio.sqrd <- (max(nfb.v.med.minus.1st, nfb.v.3rd.minus.med)^2 / 
                                 min(nfb.v.med.minus.1st, nfb.v.3rd.minus.med)^2)
  nfb.v.med.minus.min <- (nfb.v.stat[3,] - nfb.v.stat[1,])
  nfb.v.max.minus.med <- (nfb.v.stat[5,] - nfb.v.stat[3,])
  nfb.v.max.med.min.ratio.sqrd <- (max(nfb.v.med.minus.min, nfb.v.max.minus.med)^2 /
                                 min(nfb.v.med.minus.min, nfb.v.max.minus.med)^2)
  nfb.v.out.count.ratio <- (abs(nfb.v.out.above.nbr - nfb.v.out.below.nbr) /
                          nfb.count)
  
  ##### Non-Fastball Determination metrics
  
  nfb.x.z.max.minus.min <- max(nfb.z.max.minus.min, nfb.x.max.minus.min)
  nfb.x.z.3rd.minus.1st <- max(nfb.z.3rd.minus.1st, nfb.x.3rd.minus.1st)
  nfb.x.z.max.3rd.med.1st.ratio.sqrd <- max(nfb.x.3rd.med.1st.ratio.sqrd, nfb.z.3rd.med.1st.ratio.sqrd)
  nfb.v.max.3rd.med.1st.ratio.sqrd <- nfb.v.3rd.med.1st.ratio.sqrd
  nfb.x.z.max.3rd.med.1st.ratio.sqrd.ratio <- (max(nfb.x.3rd.med.1st.ratio.sqrd, nfb.z.3rd.med.1st.ratio.sqrd) /
                                             min(nfb.x.3rd.med.1st.ratio.sqrd, nfb.z.3rd.med.1st.ratio.sqrd))
  nfb.x.z.max.max.med.min.ratio.sqrd <- max(nfb.x.max.med.min.ratio.sqrd, nfb.z.max.med.min.ratio.sqrd)
  nfb.v.max.max.med.min.ratio.sqrd <- nfb.v.max.med.min.ratio.sqrd
  nfb.x.z.max.max.med.min.ratio.sqrd.ratio <- (max(nfb.x.max.med.min.ratio.sqrd, nfb.z.max.med.min.ratio.sqrd) /
                                             min(nfb.x.max.med.min.ratio.sqrd, nfb.z.max.med.min.ratio.sqrd))
  nfb.x.z.v.max.out.ratio <- max(nfb.x.out.count.ratio, nfb.z.out.count.ratio, nfb.v.out.count.ratio)

  ##### Non-Fastball Tuning Determination Parameters
  
  maximum.nfb.x.z.max.minus.min <- 30
  maximum.nfb.x.z.3rd.minus.1st <- 30
  maximum.nfb.x.z.max.3rd.med.1st.ratio.sqrd <- 2.2
  maximum.nfb.v.max.3rd.med.1st.ratio.sqrd <- 1.8
  maximum.nfb.x.z.max.3rd.med.1st.ratio.sqrd.ratio <- 2
  maximum.nfb.x.z.max.max.med.min.ratio.sqrd <- 2.2
  maximum.nfb.v.max.max.med.min.ratio.sqrd <- 1.8
  maximum.nfb.x.z.max.max.med.min.ratio.sqrd.ratio <- 2
  maximum.nfb.x.z.v.max.out.ratio <- 0.09
  
  ##### Decision to Stop or Continue clustering NFB
  
  nfb.d1 <- if(nfb.x.z.max.minus.min > maximum.nfb.x.z.max.minus.min){1} else {0}
  nfb.d2 <- if(nfb.x.z.3rd.minus.1st > maximum.nfb.x.z.3rd.minus.1st){1} else {0}
  nfb.d3 <- if(nfb.x.z.max.3rd.med.1st.ratio.sqrd > maximum.nfb.x.z.max.3rd.med.1st.ratio.sqrd){1} else {0}
  nfb.d4 <- if(nfb.v.max.3rd.med.1st.ratio.sqrd > maximum.nfb.v.max.3rd.med.1st.ratio.sqrd){1} else {0}
  nfb.d5 <- if(nfb.x.z.max.3rd.med.1st.ratio.sqrd.ratio > maximum.nfb.x.z.max.3rd.med.1st.ratio.sqrd.ratio){1} else {0}
  nfb.d6 <- if(nfb.x.z.max.max.med.min.ratio.sqrd > maximum.nfb.x.z.max.max.med.min.ratio.sqrd){1} else {0}
  nfb.d7 <- if(nfb.v.max.max.med.min.ratio.sqrd > maximum.nfb.v.max.max.med.min.ratio.sqrd){1} else {0}
  nfb.d8 <- if(nfb.x.z.max.max.med.min.ratio.sqrd.ratio > maximum.nfb.x.z.max.max.med.min.ratio.sqrd.ratio){1} else {0}
  nfb.d9 <- if(nfb.x.z.v.max.out.ratio > maximum.nfb.x.z.v.max.out.ratio){1} else {0}
  
  # these rules will feed into the Non-Fastball user defined section above
  nfb.stop.go.rule <- if(sum(nfb.d1,nfb.d2,nfb.d3,nfb.d4,nfb.d5,nfb.d6,nfb.d7,nfb.d8,nfb.d9) == 0){nfb.single.function()} else {nfb.hdbscan10.rule(nfb.data.cluster)} 

  ## Combine Fastball and (Non-Fasball or Non-Fastball Stage 2) pitches
  fb.nfb.nfb2.output <- rbind(fb.stop.go.rule, nfb.stop.go.rule)
  fb.nfb.nfb2.row.count <- nrow(fb.nfb.nfb2.output)
  fb.nfb.nfb2.cluster.count <- length(unique(fb.nfb.nfb2.output$Cluster))

  # generate plots
  remove.list <- list(" ",",")
  remove.string <- paste(unlist(remove.list), collapse = "|")
  jpeg(file= gsub(remove.string, "",paste(player,"FB.and.NFB.output.jpeg")))
  plot(fb.nfb.nfb2.output[,c(3,4,5)], col = fb.nfb.nfb2.output[,6], main = paste((player),"clust=",(fb.nfb.nfb2.cluster.count), "count=",(fb.nfb.nfb2.row.count)))
  dev.off()
  
  # generate 3D plots
  remove.list <- list(" ",",")
  remove.string <- paste(unlist(remove.list), collapse = "|")
  jpeg(file= gsub(remove.string, "",paste("3D",player,"FB.and.NFB.output.jpeg")))
  scatterplot3d(fb.nfb.nfb2.output[,c(3,4,5)], color = fb.nfb.nfb2.output[,6], angle = 55, main = paste((player),"clust=",(fb.nfb.nfb2.cluster.count), "count=",(fb.nfb.nfb2.row.count)))
  dev.off()
  
  ## FINAL OUTPUT
  fb.nfb.nfb2.output 
  
  }
  

# run function loop
player.output <- lapply(list3, pitch.happy)

# generate a combined table of all pitchers in the loop function player.output
table <- do.call(rbind, player.output)

# export table
WriteXLS(table, ExcelFileName = "FB.and.NFB.cluster.output.full.xlsx")


  
## summary tables  

summary.table.1 <- table %>%
                      group_by(Pitcher) %>%
                      summarise(Cluster_count = length(unique(Cluster)))
summary.table.1.df <- data.frame(summary.table.1)
summary.table.1.df
  
WriteXLS(summary.table.1.df, ExcelFileName = "Summary.table.1.xlsx")

summary.table.2 <-  table %>%
  group_by(Pitcher,  Cluster) %>%
  summarise(Median_x = median(break_x), 
            Median_z = median(break_z), 
            Median_velo = median(velo),
            Cluster_count = length(Cluster),
            Skew_x = round(abs(skewness(break_x)), digits = 3),
            Skew_z = round(abs(skewness(break_z)), digits = 3),
            Skew_velo = round(abs(skewness(velo)), digits = 3),
            Kurt_x = round(kurtosis(break_x), digits = 3),
            Kurt_z = round(kurtosis(break_z), digits = 3),
            Kurt_velo = round(kurtosis(velo), digits = 3)
            )
summary.table.2.df <- data.frame(summary.table.2) 
summary.table.2.df  

WriteXLS(summary.table.2.df, ExcelFileName = "Summary.table.2.xlsx")



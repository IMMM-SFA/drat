library(tidyverse) # needed for data manipulation.
library(lmerTest)# to get p-value estimations that are not part of the standard lme4 packages
library(sjstats)
library(data.table)
library(dplyr)
library(feather)
library(tibble)
library(ggplot2)
library(rstan)
library(rstanarm)
library(foreach)
library(doParallel)

args = commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
  grouping <- "sum"
} else if (args[1] %in% c("sum", "individual")) {
  grouping <- args[1]
} else {
  grouping <- "sum"
}

num_dates <- 184

xlab <- "Date"
ylab <- "AAC"

# type = simulation type
# n = number of days to select from possible 1840 
create_model_df <- function(type,n){
  set.seed(8675309)
  num<- 1:1840
  if(type == "sum"){
    cat("reading historic file...  \n")
    historic<-fread("historic_JulyUpdate.csv")
    hist_dates <- unique(historic$Date)
    orderdates_hist <- sample(num,n,replace=FALSE)
    df_hist <- subset(historic,Date %in% hist_dates[orderdates_hist]) %>% 
      group_by(method,NCA,gcm,FuelType,Date,CoolingTechnology) %>% 
      summarise(AAC = sum(operational_capacity)/sum(Nameplate))
    df_hist$Period <- "historic"
    rm(historic)
    cat("reading future file...  \n")
    future <-fread("future_JulyUpdate.csv")
    fut_dates <- unique(future$Date)
    orderdates_fut <- sample(num,n,replace=FALSE)
    df_fut <-subset(future,Date %in% fut_dates[orderdates_fut]) %>% 
      group_by(method,NCA,gcm,FuelType,Date,CoolingTechnology) %>% 
      summarise(AAC = sum(operational_capacity)/sum(Nameplate))
    df_fut$Period <- "future"
    all<- rbind(df_hist,df_fut)
    all$Days <- ifelse((all$Period == "historic"),0,1)
    all <- setDF(all)
    keepcols <- c("AAC","Date","FuelType","method","Period","gcm","CoolingTechnology","NCA","Days")
    # keepcols <- c("AAC_all","Date","FuelType","NonFreshwater","method","Period","gcm","CoolingTechnology","NCA","Days")
  } else if(type == "individual"){
      cat("reading historic file...  \n")
      historic<-fread("historic.csv")
      hist_dates <- unique(historic$Date)
      orderdates_hist <- sample(num,n,replace=FALSE)
      hist_dates <- unique(historic$Date)
      orderdates_hist <- sample(num,2,replace=FALSE)
      df_hist <- subset(historic,Date %in% hist_dates[orderdates_hist])
      df_hist$Period <- "historic"
      rm(historic)
      cat("reading future file...  \n")
      future <-fread("future.csv")
      fut_dates <- unique(future$Date)
      orderdates_fut <- sample(num,n,replace=FALSE)
      df_fut <-subset(future,Date %in% fut_dates[orderdates_fut])
      df_fut$Period <- "future"
      rm(future)
      all<- rbind(df_hist,df_fut)
      all$Days <- ifelse((all$Period == "historic"),0,1)
      all$AAC <- all$operational_capacity/all$Nameplate
      all <- setDF(all)
      # keepcols <- c("AAC","EIA_ID","Date","FuelType","NonFreshwater","method","Period","gcm","CoolingTechnology","NCA","Days")
      keepcols <- c("AAC","EIA_ID","Date","FuelType","method","Period","gcm","CoolingTechnology","NCA","Days")
      all <- all %>% 
      as_tibble() %>% 
      mutate(EIA_ID = as.character(EIA_ID))
  } else {
    cat("ERROR")
    final_df = ""
  }
  all<-all[keepcols]
  all <- all %>% 
  as_tibble() %>% 
  mutate(Period = as.character(Period))
  return(all)
}

do_stan <- function(this_df, region, ft, ct, num_dates, out_dir) {
  cat(paste("num_dates: ",num_dates,"     \n",sep=""))
  cat(paste("grouping: ",grouping,"     \n",sep=""))
  cat(paste("region: ",region,"     \n",sep=""))
  cat(paste("fuel type: ",ft,"     \n",sep=""))
  cat(paste("cooling tech: ",ct,"    \n",sep=""))
  b <- stan_glmer(
    AAC ~ Days + (Days | method/gcm),
    family = gaussian(),
    data = this_df,
    prior = normal(0, 0.1, autoscale = TRUE),
    prior_intercept = normal(0, 0.1, autoscale = TRUE),
    prior_covariance = decov(regularization = 2, scale = 0.1),
    prior_aux = cauchy(0, 0.1, autoscale = TRUE), 
    iter = 2000,
    seed = 8675309,
    refresh = 1,
    verbose = TRUE,
    #cores = 1,
  )
  write.csv(VarCorr(b,comp=c("Variance","Std.Dev.")),paste(out_dir,"/",region,ft,ct,"_varcorr.csv",sep=""),row.names=TRUE)
}

options(mc.cores=parallel::detectCores())
rstan_options(auto_write = TRUE)

parallel_tasks = (parallel::detectCores() %/% 4) - 1
#parallel_tasks = parallel::detectCores() - 1

cluster <- makeCluster(parallel_tasks, type="FORK") 
registerDoParallel(cluster)

out_dir <- paste0(grouping,"_stan_fits_n",num_dates)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

all_test <- create_model_df(grouping,num_dates)
foreach(region = unique(all_test$NCA)) %:%
  foreach(ft = unique(all_test$FuelType)) %:%
    foreach(ct = unique(all_test$CoolingTechnology)) %dopar% {
      this_df <- subset(all_test, NCA==region & FuelType==ft & CoolingTechnology==ct)
      if (nrow(this_df) > 0) {
        capture.output(
          do_stan(this_df, region, ft, ct, num_dates, out_dir),
          file = paste0(out_dir,"/",grouping,"_n",num_dates, "_", region, ft, ct, ".log")
        )
      }
      return
    }


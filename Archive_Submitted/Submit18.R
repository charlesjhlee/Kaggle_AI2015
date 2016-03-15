#remove all ls before starting
rm(list=ls())


#load caret library
library(caret)
library(data.table)
library("foreach")
library("doParallel")
#require(MASS)

#read in source train and test
train.source <- read.csv(file = "c:/kaggle/data/train.csv")
test.source <- read.csv(file = "c:/kaggle/data/test.csv")

longlat <- rbind(train.source[,c('Long', 'Lat', 'AREA_SQKM', 'Total_Total')]
                 ,test.source[,c('Long', 'Lat', 'AREA_SQKM', 'Total_Total')])
longlat <- longlat[longlat$AREA_SQKM>0 & longlat$Total_Total>0,]

cities <- kmeans(x=longlat[,c('Long', 'Lat')], centers=6)

#longlat <- longlat[longlat$AREA_SQKM/longlat$Total_Total>10,]

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

closestcity <- function(Long, Lat, cities){
  for (i in 1:nrow(cities)){
    if (i==1){
      d <- data.frame(earth.dist(Long, Lat
                                 ,cities[i,'Long']
                                 , cities[i,'Lat']))
    }else{
      d <- cbind(d,earth.dist(Long, Lat
                              ,cities[i,'Long']
                              , cities[i,'Lat']))
    }
  }
  names(d) <- as.character(1:nrow(cities))
  apply(d, 1, min)
}

train.source$min.dist <- closestcity(train.source$Long, train.source$Lat, cities$centers)
test.source$min.dist <- closestcity(test.source$Long, test.source$Lat, cities$centers)

train.source$birth_elsewhere = (train.source$Birthplace_Elsewhere_M + train.source$Birthplace_Elsewhere_F) / (train.source$Total_Total + 1)
test.source$birth_elsewhere = (test.source$Birthplace_Elsewhere_M + test.source$Birthplace_Elsewhere_F) / (test.source$Total_Total + 1)

train.source$christian = (train.source$Christianity_Tot_M + train.source$Christianity_Tot_F) / (train.source$Total_Total + 1)
test.source$christian = (test.source$Christianity_Tot_M + test.source$Christianity_Tot_F) / (test.source$Total_Total + 1)

train.source$buddhism = (train.source$Buddhism_M + train.source$Buddhism_F) / (train.source$Total_Total + 1)
test.source$buddhism = (test.source$Buddhism_M + test.source$Buddhism_F) / (test.source$Total_Total + 1)

train.source$hinduism = (train.source$Hinduism_M + train.source$Hinduism_F) / (train.source$Total_Total + 1)
test.source$hinduism = (test.source$Hinduism_M + test.source$Hinduism_F) / (test.source$Total_Total + 1)

train.source$judaism = (train.source$Judaism_M + train.source$Judaism_F) / (train.source$Total_Total + 1)
test.source$judaism = (test.source$Judaism_M + test.source$Judaism_F) / (test.source$Total_Total + 1)

train.source$islam = (train.source$Islam_M + train.source$Islam_F) / (train.source$Total_Total + 1)
test.source$islam = (test.source$Islam_M + test.source$Islam_F) / (test.source$Total_Total + 1)

train.source$noreligion = (train.source$No_Religion_M + train.source$No_Religion_F) / (train.source$Total_Total + 1)
test.source$noreligion = (test.source$No_Religion_M + test.source$No_Religion_F) / (test.source$Total_Total + 1)

train.source$unemployment = (train.source$lfs_Unmplyed_lookng_for_wrk_M + train.source$lfs_Unmplyed_lookng_for_wrk_F) / (train.source$lfs_Tot_LF_M + train.source$lfs_Tot_LF_F + 1)
test.source$unemployment = (test.source$lfs_Unmplyed_lookng_for_wrk_M + test.source$lfs_Unmplyed_lookng_for_wrk_F) / (test.source$lfs_Tot_LF_M + test.source$lfs_Tot_LF_F + 1)

train.source$density = (train.source$Total_Total) / (train.source$Total_Total + 1)
test.source$density = (test.source$Total_Total) / (test.source$Total_Total + 1)

#divide training dataset into partitions. one for training and another for validating
set.seed(123)
train_ind <- createDataPartition(train.source$DEATHS, p=0.6)[[1]]
traindata <- as.data.table(train.source[train_ind, ])
validdata <- as.data.table(train.source[-train_ind, ])
testdata <- test.source

#use multithreading
#cl <- makeCluster(detectCores() - 1)
#registerDoParallel(cl)

# define training control
train_control <- trainControl(method="cv", number=5)

# fit model on training data
fit.all <- train(DEATHS ~ #AREA_SQKM
                   #+ Long
                   #+ Lat
                   Age_0_4_yr_M
                 + Age_0_4_yr_F
                 #+ Age_5_14_yr_M
                 #+ Age_5_14_yr_F
                 #+ Age_15_19_yr_M
                 #+ Age_15_19_yr_F
                 #+ Age_20_24_yr_M
                 #+ Age_20_24_yr_F
                 #+ Age_25_34_yr_M
                 #+ Age_25_34_yr_F
                 #+ Age_35_44_yr_M
                 #+ Age_35_44_yr_F
                 #+ Age_45_54_yr_M
                 #+ Age_45_54_yr_F
                 + Age_55_64_yr_M
                 + Age_55_64_yr_F
                 + Age_65_74_yr_M
                 + Age_65_74_yr_F
                 + Age_75_84_yr_M
                 + Age_75_84_yr_F
                 + Age_85ov_M
                 + Age_85ov_F
                 + Counted_Census_Night_home_M
                 + Counted_Census_Night_home_F
                 #+ Count_Census_Nt_Ewhere_Aust_M
                 #+ Count_Census_Nt_Ewhere_Aust_F
                 #+ Indigenous_psns_Aboriginal_M
                 #+ Indigenous_psns_Aboriginal_F
                 #+ Indig_psns_Torres_Strait_Is_M
                 #+ Indig_psns_Torres_Strait_Is_F
                 #+ Indig_Bth_Abor_Torres_St_Is_M
                 #+ Indig_Bth_Abor_Torres_St_Is_F
                 #+ Indigenous_P_Tot_M
                 #+ Indigenous_P_Tot_F
                 #+ Birthplace_Australia_M
                 #+ Birthplace_Australia_F
                 + Birthplace_Elsewhere_M
                 + Birthplace_Elsewhere_F
                 #+ Lang_spoken_home_Eng_only_M
                 #+ Lang_spoken_home_Eng_only_F
                 #+ Lang_spoken_home_Oth_Lang_M
                 #+ Lang_spoken_home_Oth_Lang_F
                 #+ Australian_citizen_M
                 #+ Australian_citizen_F
                 + High_yr_schl_comp_Yr_12_eq_M
                 + High_yr_schl_comp_Yr_12_eq_F
                 + Count_psns_occ_priv_dwgs_M
                 + Count_psns_occ_priv_dwgs_F
                 #+ Count_Persons_other_dwgs_M
                 #+ Count_Persons_other_dwgs_F
                 + Median_mortgage_repay_monthly
                 + Median_Tot_prsnl_inc_weekly
                 #+ Median_Tot_fam_inc_weekly
                 + Average_num_psns_per_bedroom
                 #+ Median_Tot_hhd_inc_weekly
                 #+ Average_household_size
                 #+ Total_Total
                 #+ M_Tot_Married
                 + M_Tot_Separated
                 + M_Tot_Divorced
                 + M_Tot_Widowed
                 + M_Tot_Never_Married
                 #+ F_Tot_Married
                 + F_Tot_Separated
                 + F_Tot_Divorced
                 + F_Tot_Widowed
                 + F_Tot_Never_Married
                 #+ Tot_Indigenous_M
                 #+ Tot_Indigenous_F
                 #+ Tot_Non_Indigenous_M
                 #+ Tot_Non_Indigenous_F
                 #+ Tot_Indig_status_ns_M
                 + Tot_Indig_status_ns_F
                 #+ SEO_Males
                 + SEO_Females
                 #+ Buddhism_M
                 #+ Buddhism_F
                 #+ Christianity_Tot_M
                 #+ Christianity_Tot_F
                 #+ Hinduism_M
                 #+ Hinduism_F
                 #+ Islam_M
                 #+ Islam_F
                 #+ Judaism_M
                 + Judaism_F
                 #+ Other_Religions_Tot_M
                 + Other_Religions_Tot_F
                 + No_Religion_M
                 + No_Religion_F
                 #+ M_Tot_Volunteer
                 #+ M_Tot_N_a_volunteer
                 + F_Tot_Volunteer
                 #+ F_Tot_N_a_volunteer
                 + Total_PDs_Dwellings
                 #+ Total_PDs_Persons
                 + O_OR_Total
                 #+ O_MTG_Total
                 #+ R_Tot_Total
                 + lfs_Unmplyed_lookng_for_wrk_M
                 + lfs_Unmplyed_lookng_for_wrk_F
                 #+ lfs_Tot_LF_M
                 #+ lfs_Tot_LF_F
                 #+ M_PGrad_Deg_Total
                 + M_BachDeg_Total #----> could include (submit 3)
                 #+ F_PGrad_Deg_Total
                 + F_BachDeg_Total
                 + STATE #----> could include (submit 3)
                 + birth_elsewhere
                 + unemployment
                 + density
                 #+ christian
                 #+ buddhism
                 #+ hinduism
                 #+ judaism
                 #+ islam 
                 #+ noreligion
                 + min.dist
                 
                 , method = 'glm', data = traindata, trControl = train_control)

#stop multithreading
#stopCluster(cl)

# run model on training data
pred.train.glm <- predict(fit.all, newdata = traindata)
pred.train.glm[pred.train.glm<0] <- 0

# run model on validation data and test data
pred.valid.glm <- predict(fit.all, newdata = validdata)
pred.valid.glm[pred.valid.glm<0] <- 0
pred.test.glm <- predict(fit.all, newdata = testdata)
pred.test.glm[pred.test.glm<0] <- 0

# calculate the root mean squared number (i.e. submission score)
sqrt(mean((pred.train.glm-traindata$DEATHS)^2))
sqrt(mean((pred.valid.glm-validdata$DEATHS)^2))

fit.all

#convert result to data frame and then create submission csv file
submit <- as.data.frame(pred.test.glm)
submit$rownumber = 1:nrow(submit)
colnames(submit) <- c("DEATHS","id")
write.table (submit[c("id","DEATHS")], sep=",", file = "c:/kaggle/data/submit18.csv", col.names=TRUE, row.names = FALSE)

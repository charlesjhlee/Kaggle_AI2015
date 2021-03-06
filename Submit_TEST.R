
#remove all ls before starting
rm(list=ls())


#load caret library
library(caret)
library(data.table)
library("foreach")
library("doParallel")

#read in source train and test
train.source <- read.csv(file = "c:/kaggle/data/newtrain.csv")
test.source <- read.csv(file = "c:/kaggle/data/newtest.csv")

AUS_MORT <- c(
  'Age_0_4_yr_M'=0.000990 
  ,'Age_5_14_yr_M'=0.000109 
  ,'Age_15_19_yr_M'=0.000433 
  ,'Age_20_24_yr_M'=0.000624 
  ,'Age_25_34_yr_M'=0.000814 
  ,'Age_35_44_yr_M'=0.001337 
  ,'Age_45_54_yr_M'=0.002833 
  ,'Age_55_64_yr_M'=0.006557 
  ,'Age_65_74_yr_M'=0.016610 
  ,'Age_75_84_yr_M'=0.049428 
  ,'Age_85ov_M'=0.152133 
  ,'Age_0_4_yr_F'=0.000804 
  ,'Age_5_14_yr_F'=0.000086 
  ,'Age_15_19_yr_F'=0.000231 
  ,'Age_20_24_yr_F'=0.000262 
  ,'Age_25_34_yr_F'=0.000350 
  ,'Age_35_44_yr_F'=0.000769 
  ,'Age_45_54_yr_F'=0.001759 
  ,'Age_55_64_yr_F'=0.003950 
  ,'Age_65_74_yr_F'=0.010248 
  ,'Age_75_84_yr_F'=0.032722 
  ,'Age_85ov_F'=0.130800 )

train.source$AUS_MORT_DTH <-as.vector((as.matrix(train.source[,names(AUS_MORT)]) %*% AUS_MORT))
test.source$AUS_MORT_DTH <-as.vector((as.matrix(test.source[,names(AUS_MORT)]) %*% AUS_MORT))

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


#divide training dataset into partitions. one for training and another for validating
set.seed(123)
train_ind <- createDataPartition(train.source$DEATHS, p=1)[[1]]
traindata <- as.data.table(train.source[train_ind, ])
validdata <- as.data.table(train.source[-train_ind, ])
testdata <- test.source

# define training control
train_control <- trainControl(method="cv", number=5)

#use multithreading
#cl <- makeCluster(detectCores() - 1)
#registerDoParallel(cl)

# fit model on training data
fit.all <- train(DEATHS ~ +High_yr_schl_comp_Yr_12_eq_F
                 +F_BachDeg_Total
                 +O_OR_Total
                 +Judaism_F
                 +No_Religion_M
                 +lfs_Unmplyed_lookng_for_wrk_M
                 +Hinduism_M
                 +Buddhism_M
                 +Other_Religions_Tot_F
                 +Judaism_M
                 +Lang_spoken_home_Oth_Lang_M
                 +Accidental_Mort
                 +F_Tot_Divorced
                 +F_Tot_Never_Married
                 +F_Tot_Married
                 +Total_PDs_Dwellings
                 +Median_Tot_prsnl_inc_weekly
                 +noreligion
                 +Islam_F
                 +Counted_Census_Night_home_F
                 +judaism
                 +M_Tot_N_a_volunteer
                 +F_PGrad_Deg_Total
                 +Infant_Mort
                 +Christianity_Tot_F
                 +lfs_Unmplyed_lookng_for_wrk_F
                 +Birthplace_Australia_F
                 +Average_num_psns_per_bedroom
                 +Count_Census_Nt_Ewhere_Aust_F
                 +Lang_spoken_home_Oth_Lang_F
                 +Tot_Indig_status_ns_M
                 +Male_Ratio
                 +Density
                 +Tot_Non_Indigenous_M
                 +lfs_Tot_LF_F
                 +Christianity_Tot_M
                 +STATE
                 +F_Tot_Separated
                 +Aboriginal
                 +Age_5_14_yr_M
                 +Median_Tot_hhd_inc_weekly
                 +Age_0_4_yr_M
                 +unemployment
                 +Age_35_44_yr_M
                 +F_Tot_Widowed
                 +Age_15_19_yr_M
                 +Birthplace_Elsewhere_F
                 +Tot_Indigenous_M
                 +Age_45_54_yr_M
                 +SEO_Females
                 +Age_20_24_yr_M
                 +High_yr_schl_comp_Yr_12_eq_M
                 +Age_25_34_yr_M
                 +M_BachDeg_Total
                 +Age_55_64_yr_M
                 +Tot_Indig_status_ns_F
                 +Average_household_size
                 +Lang_spoken_home_Eng_only_F
                 +christian
                 +Age_75_84_yr_M
                 +Lang_spoken_home_Eng_only_M
                 +M_Tot_Volunteer
                 +hinduism
                 +Indigenous_psns_Aboriginal_M
                 +Indig_psns_Torres_Strait_Is_M
                 +Indig_Bth_Abor_Torres_St_Is_M
                 +Indigenous_P_Tot_M
                 +buddhism
                 +Age_65_74_yr_M
                 +Age_85ov_M
                 +Stay_at_home
                 +M_PGrad_Deg_Total
                 +SEO_Males
                 +Tot_Non_Indigenous_F
                 +Buddhism_F
                 +F_Tot_Volunteer
                 +M_Tot_Widowed
                 +Age_5_14_yr_F
                 +Count_psns_occ_priv_dwgs_M
                 +Hinduism_F
                 +Age_0_4_yr_F
                 +Median_mortgage_repay_monthly
                 +AREA_SQKM
                 +lfs_Tot_LF_M
                 +Indigenous_psns_Aboriginal_F
                 +Indig_psns_Torres_Strait_Is_F
                 +Indig_Bth_Abor_Torres_St_Is_F
                 +Indigenous_P_Tot_F
                 +Other_Religions_Tot_M
                 +Count_Persons_other_dwgs_M
                 +Total_Total
                 +F_Tot_N_a_volunteer
                 +Australian_citizen_M
                 +Count_Persons_other_dwgs_F
                 +Age_85ov_F
                
                 , method = 'glm', data = traindata, trControl = train_control)
                 #, method = 'lasso', data = traindata, tuneLength = 10)

#stop multithreading
#stopCluster(cl)

# run model on training data
pred.train.glm <- predict(fit.all, newdata = traindata)

# run model on validation data and test data
pred.valid.glm <- predict(fit.all, newdata = validdata)
pred.test.glm <- predict(fit.all, newdata = testdata)

# calculate the root mean squared number (i.e. submission score)
sqrt(mean((pred.train.glm-traindata$DEATHS)^2))
sqrt(mean((pred.valid.glm-validdata$DEATHS)^2))

fit.all

pred.ens.all <- pred.test.glm

#convert result to data frame and then create submission csv file
submit <- as.data.frame(pred.ens.all)
submit$rownumber = 1:nrow(submit)
colnames(submit) <- c("DEATHS","id")
#write.table (submit[c("id","DEATHS")], sep=",", file = "c:/kaggle/data/submit_fteng1.csv", col.names=TRUE, row.names = FALSE)

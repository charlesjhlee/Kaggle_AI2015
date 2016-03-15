
#remove all ls before starting
rm(list=ls())


#load caret library
library(caret)
library(data.table)
library(mlbench)
library("foreach")
library("doParallel")

#read in source train and test
train.source <- read.csv(file = "c:/kaggle/data/newtrain.csv")
test.source <- read.csv(file = "c:/kaggle/data/newtest.csv")

#cut data 2 ways
train.source$Aboriginal2 <- as.numeric(cut2(train.source$Aboriginal, g=2))
train.source$Accidental_Mort2 <- as.numeric(cut2(train.source$Accidental_Mort, g=2))
train.source$Age_0_4_yr_F2 <- as.numeric(cut2(train.source$Age_0_4_yr_F, g=2))
train.source$Age_0_4_yr_M2 <- as.numeric(cut2(train.source$Age_0_4_yr_M, g=2))
train.source$Age_15_19_yr_F2 <- as.numeric(cut2(train.source$Age_15_19_yr_F, g=2))
train.source$Age_15_19_yr_M2 <- as.numeric(cut2(train.source$Age_15_19_yr_M, g=2))
train.source$Age_20_24_yr_F2 <- as.numeric(cut2(train.source$Age_20_24_yr_F, g=2))
train.source$Age_20_24_yr_M2 <- as.numeric(cut2(train.source$Age_20_24_yr_M, g=2))
train.source$Age_25_34_yr_F2 <- as.numeric(cut2(train.source$Age_25_34_yr_F, g=2))
train.source$Age_25_34_yr_M2 <- as.numeric(cut2(train.source$Age_25_34_yr_M, g=2))
train.source$Age_35_44_yr_F2 <- as.numeric(cut2(train.source$Age_35_44_yr_F, g=2))
train.source$Age_35_44_yr_M2 <- as.numeric(cut2(train.source$Age_35_44_yr_M, g=2))
train.source$Age_45_54_yr_F2 <- as.numeric(cut2(train.source$Age_45_54_yr_F, g=2))
train.source$Age_45_54_yr_M2 <- as.numeric(cut2(train.source$Age_45_54_yr_M, g=2))
train.source$Age_5_14_yr_F2 <- as.numeric(cut2(train.source$Age_5_14_yr_F, g=2))
train.source$Age_5_14_yr_M2 <- as.numeric(cut2(train.source$Age_5_14_yr_M, g=2))
train.source$Age_55_64_yr_F2 <- as.numeric(cut2(train.source$Age_55_64_yr_F, g=2))
train.source$Age_55_64_yr_M2 <- as.numeric(cut2(train.source$Age_55_64_yr_M, g=2))
train.source$Age_65_74_yr_F2 <- as.numeric(cut2(train.source$Age_65_74_yr_F, g=2))
train.source$Age_65_74_yr_M2 <- as.numeric(cut2(train.source$Age_65_74_yr_M, g=2))
train.source$Age_75_84_yr_F2 <- as.numeric(cut2(train.source$Age_75_84_yr_F, g=2))
train.source$Age_75_84_yr_M2 <- as.numeric(cut2(train.source$Age_75_84_yr_M, g=2))
train.source$Age_85ov_F2 <- as.numeric(cut2(train.source$Age_85ov_F, g=2))
train.source$Age_85ov_M2 <- as.numeric(cut2(train.source$Age_85ov_M, g=2))
train.source$AREA_SQKM2 <- as.numeric(cut2(train.source$AREA_SQKM, g=2))
train.source$Australian_citizen_F2 <- as.numeric(cut2(train.source$Australian_citizen_F, g=2))
train.source$Australian_citizen_M2 <- as.numeric(cut2(train.source$Australian_citizen_M, g=2))
train.source$Average_household_size2 <- as.numeric(cut2(train.source$Average_household_size, g=2))
train.source$Average_num_psns_per_bedroom2 <- as.numeric(cut2(train.source$Average_num_psns_per_bedroom, g=2))
train.source$Birthplace_Australia_F2 <- as.numeric(cut2(train.source$Birthplace_Australia_F, g=2))
train.source$Birthplace_Australia_M2 <- as.numeric(cut2(train.source$Birthplace_Australia_M, g=2))
train.source$Birthplace_Elsewhere_F2 <- as.numeric(cut2(train.source$Birthplace_Elsewhere_F, g=2))
train.source$Birthplace_Elsewhere_M2 <- as.numeric(cut2(train.source$Birthplace_Elsewhere_M, g=2))
train.source$Buddhism_F2 <- as.numeric(cut2(train.source$Buddhism_F, g=2))
train.source$Buddhism_M2 <- as.numeric(cut2(train.source$Buddhism_M, g=2))
train.source$Christianity_Tot_F2 <- as.numeric(cut2(train.source$Christianity_Tot_F, g=2))
train.source$Christianity_Tot_M2 <- as.numeric(cut2(train.source$Christianity_Tot_M, g=2))
train.source$Count_Census_Nt_Ewhere_Aust_F2 <- as.numeric(cut2(train.source$Count_Census_Nt_Ewhere_Aust_F, g=2))
train.source$Count_Census_Nt_Ewhere_Aust_M2 <- as.numeric(cut2(train.source$Count_Census_Nt_Ewhere_Aust_M, g=2))
train.source$Count_Persons_other_dwgs_F2 <- as.numeric(cut2(train.source$Count_Persons_other_dwgs_F, g=2))
train.source$Count_Persons_other_dwgs_M2 <- as.numeric(cut2(train.source$Count_Persons_other_dwgs_M, g=2))
train.source$Count_psns_occ_priv_dwgs_F2 <- as.numeric(cut2(train.source$Count_psns_occ_priv_dwgs_F, g=2))
train.source$Count_psns_occ_priv_dwgs_M2 <- as.numeric(cut2(train.source$Count_psns_occ_priv_dwgs_M, g=2))
train.source$Counted_Census_Night_home_F2 <- as.numeric(cut2(train.source$Counted_Census_Night_home_F, g=2))
train.source$Counted_Census_Night_home_M2 <- as.numeric(cut2(train.source$Counted_Census_Night_home_M, g=2))
train.source$Density2 <- as.numeric(cut2(train.source$Density, g=2))
train.source$Dist_Major_City2 <- as.numeric(cut2(train.source$Dist_Major_City, g=2))
train.source$F_BachDeg_Total2 <- as.numeric(cut2(train.source$F_BachDeg_Total, g=2))
train.source$F_PGrad_Deg_Total2 <- as.numeric(cut2(train.source$F_PGrad_Deg_Total, g=2))
train.source$F_Tot_Divorced2 <- as.numeric(cut2(train.source$F_Tot_Divorced, g=2))
train.source$F_Tot_Married2 <- as.numeric(cut2(train.source$F_Tot_Married, g=2))
train.source$F_Tot_N_a_volunteer2 <- as.numeric(cut2(train.source$F_Tot_N_a_volunteer, g=2))
train.source$F_Tot_Never_Married2 <- as.numeric(cut2(train.source$F_Tot_Never_Married, g=2))
train.source$F_Tot_Separated2 <- as.numeric(cut2(train.source$F_Tot_Separated, g=2))
train.source$F_Tot_Volunteer2 <- as.numeric(cut2(train.source$F_Tot_Volunteer, g=2))
train.source$F_Tot_Widowed2 <- as.numeric(cut2(train.source$F_Tot_Widowed, g=2))
train.source$Foreign2 <- as.numeric(cut2(train.source$Foreign, g=2))
train.source$High_yr_schl_comp_Yr_12_eq_F2 <- as.numeric(cut2(train.source$High_yr_schl_comp_Yr_12_eq_F, g=2))
train.source$High_yr_schl_comp_Yr_12_eq_M2 <- as.numeric(cut2(train.source$High_yr_schl_comp_Yr_12_eq_M, g=2))
train.source$Hinduism_F2 <- as.numeric(cut2(train.source$Hinduism_F, g=2))
train.source$Hinduism_M2 <- as.numeric(cut2(train.source$Hinduism_M, g=2))
train.source$Indig_Bth_Abor_Torres_St_Is_F2 <- as.numeric(cut2(train.source$Indig_Bth_Abor_Torres_St_Is_F, g=2))
train.source$Indig_Bth_Abor_Torres_St_Is_M2 <- as.numeric(cut2(train.source$Indig_Bth_Abor_Torres_St_Is_M, g=2))
train.source$Indig_psns_Torres_Strait_Is_F2 <- as.numeric(cut2(train.source$Indig_psns_Torres_Strait_Is_F, g=2))
train.source$Indig_psns_Torres_Strait_Is_M2 <- as.numeric(cut2(train.source$Indig_psns_Torres_Strait_Is_M, g=2))
train.source$Indigenous_P_Tot_F2 <- as.numeric(cut2(train.source$Indigenous_P_Tot_F, g=2))
train.source$Indigenous_P_Tot_M2 <- as.numeric(cut2(train.source$Indigenous_P_Tot_M, g=2))
train.source$Indigenous_psns_Aboriginal_F2 <- as.numeric(cut2(train.source$Indigenous_psns_Aboriginal_F, g=2))
train.source$Indigenous_psns_Aboriginal_M2 <- as.numeric(cut2(train.source$Indigenous_psns_Aboriginal_M, g=2))
train.source$Infant_Mort2 <- as.numeric(cut2(train.source$Infant_Mort, g=2))
train.source$Islam_F2 <- as.numeric(cut2(train.source$Islam_F, g=2))
train.source$Islam_M2 <- as.numeric(cut2(train.source$Islam_M, g=2))
train.source$Judaism_F2 <- as.numeric(cut2(train.source$Judaism_F, g=2))
train.source$Judaism_M2 <- as.numeric(cut2(train.source$Judaism_M, g=2))
train.source$Lang_spoken_home_Eng_only_F2 <- as.numeric(cut2(train.source$Lang_spoken_home_Eng_only_F, g=2))
train.source$Lang_spoken_home_Eng_only_M2 <- as.numeric(cut2(train.source$Lang_spoken_home_Eng_only_M, g=2))
train.source$Lang_spoken_home_Oth_Lang_F2 <- as.numeric(cut2(train.source$Lang_spoken_home_Oth_Lang_F, g=2))
train.source$Lang_spoken_home_Oth_Lang_M2 <- as.numeric(cut2(train.source$Lang_spoken_home_Oth_Lang_M, g=2))
train.source$Lat2 <- as.numeric(cut2(train.source$Lat, g=2))
train.source$lfs_Tot_LF_F2 <- as.numeric(cut2(train.source$lfs_Tot_LF_F, g=2))
train.source$lfs_Tot_LF_M2 <- as.numeric(cut2(train.source$lfs_Tot_LF_M, g=2))
train.source$lfs_Unmplyed_lookng_for_wrk_F2 <- as.numeric(cut2(train.source$lfs_Unmplyed_lookng_for_wrk_F, g=2))
train.source$lfs_Unmplyed_lookng_for_wrk_M2 <- as.numeric(cut2(train.source$lfs_Unmplyed_lookng_for_wrk_M, g=2))
train.source$Long2 <- as.numeric(cut2(train.source$Long, g=2))
train.source$M_BachDeg_Total2 <- as.numeric(cut2(train.source$M_BachDeg_Total, g=2))
train.source$M_PGrad_Deg_Total2 <- as.numeric(cut2(train.source$M_PGrad_Deg_Total, g=2))
train.source$M_Tot_Divorced2 <- as.numeric(cut2(train.source$M_Tot_Divorced, g=2))
train.source$M_Tot_Married2 <- as.numeric(cut2(train.source$M_Tot_Married, g=2))
train.source$M_Tot_N_a_volunteer2 <- as.numeric(cut2(train.source$M_Tot_N_a_volunteer, g=2))
train.source$M_Tot_Never_Married2 <- as.numeric(cut2(train.source$M_Tot_Never_Married, g=2))
train.source$M_Tot_Separated2 <- as.numeric(cut2(train.source$M_Tot_Separated, g=2))
train.source$M_Tot_Volunteer2 <- as.numeric(cut2(train.source$M_Tot_Volunteer, g=2))
train.source$M_Tot_Widowed2 <- as.numeric(cut2(train.source$M_Tot_Widowed, g=2))
train.source$Male_Ratio2 <- as.numeric(cut2(train.source$Male_Ratio, g=2))
train.source$Marriage_Prob2 <- as.numeric(cut2(train.source$Marriage_Prob, g=2))
train.source$Median_mortgage_repay_monthly2 <- as.numeric(cut2(train.source$Median_mortgage_repay_monthly, g=2))
train.source$Median_Tot_fam_inc_weekly2 <- as.numeric(cut2(train.source$Median_Tot_fam_inc_weekly, g=2))
train.source$Median_Tot_hhd_inc_weekly2 <- as.numeric(cut2(train.source$Median_Tot_hhd_inc_weekly, g=2))
train.source$Median_Tot_prsnl_inc_weekly2 <- as.numeric(cut2(train.source$Median_Tot_prsnl_inc_weekly, g=2))
train.source$No_Religion_F2 <- as.numeric(cut2(train.source$No_Religion_F, g=2))
train.source$No_Religion_M2 <- as.numeric(cut2(train.source$No_Religion_M, g=2))
train.source$O_MTG_Total2 <- as.numeric(cut2(train.source$O_MTG_Total, g=2))
train.source$O_OR_Total2 <- as.numeric(cut2(train.source$O_OR_Total, g=2))
train.source$Old_Age_F2 <- as.numeric(cut2(train.source$Old_Age_F, g=2))
train.source$Old_Age_M2 <- as.numeric(cut2(train.source$Old_Age_M, g=2))
train.source$Other_Religions_Tot_F2 <- as.numeric(cut2(train.source$Other_Religions_Tot_F, g=2))
train.source$Other_Religions_Tot_M2 <- as.numeric(cut2(train.source$Other_Religions_Tot_M, g=2))
train.source$R_Tot_Total2 <- as.numeric(cut2(train.source$R_Tot_Total, g=2))
train.source$SEO_Females2 <- as.numeric(cut2(train.source$SEO_Females, g=2))
train.source$SEO_Males2 <- as.numeric(cut2(train.source$SEO_Males, g=2))
train.source$Stay_at_home2 <- as.numeric(cut2(train.source$Stay_at_home, g=2))
train.source$Tot_Indig_status_ns_F2 <- as.numeric(cut2(train.source$Tot_Indig_status_ns_F, g=2))
train.source$Tot_Indig_status_ns_M2 <- as.numeric(cut2(train.source$Tot_Indig_status_ns_M, g=2))
train.source$Tot_Indigenous_F2 <- as.numeric(cut2(train.source$Tot_Indigenous_F, g=2))
train.source$Tot_Indigenous_M2 <- as.numeric(cut2(train.source$Tot_Indigenous_M, g=2))
train.source$Tot_Non_Indigenous_F2 <- as.numeric(cut2(train.source$Tot_Non_Indigenous_F, g=2))
train.source$Tot_Non_Indigenous_M2 <- as.numeric(cut2(train.source$Tot_Non_Indigenous_M, g=2))
train.source$Total_PDs_Dwellings2 <- as.numeric(cut2(train.source$Total_PDs_Dwellings, g=2))
train.source$Total_PDs_Persons2 <- as.numeric(cut2(train.source$Total_PDs_Persons, g=2))
train.source$Total_Total2 <- as.numeric(cut2(train.source$Total_Total, g=2))
train.source$Unemp2 <- as.numeric(cut2(train.source$Unemp, g=2))

test.source$Aboriginal2 <- as.numeric(cut2(test.source$Aboriginal, g=2))
test.source$Accidental_Mort2 <- as.numeric(cut2(test.source$Accidental_Mort, g=2))
test.source$Age_0_4_yr_F2 <- as.numeric(cut2(test.source$Age_0_4_yr_F, g=2))
test.source$Age_0_4_yr_M2 <- as.numeric(cut2(test.source$Age_0_4_yr_M, g=2))
test.source$Age_15_19_yr_F2 <- as.numeric(cut2(test.source$Age_15_19_yr_F, g=2))
test.source$Age_15_19_yr_M2 <- as.numeric(cut2(test.source$Age_15_19_yr_M, g=2))
test.source$Age_20_24_yr_F2 <- as.numeric(cut2(test.source$Age_20_24_yr_F, g=2))
test.source$Age_20_24_yr_M2 <- as.numeric(cut2(test.source$Age_20_24_yr_M, g=2))
test.source$Age_25_34_yr_F2 <- as.numeric(cut2(test.source$Age_25_34_yr_F, g=2))
test.source$Age_25_34_yr_M2 <- as.numeric(cut2(test.source$Age_25_34_yr_M, g=2))
test.source$Age_35_44_yr_F2 <- as.numeric(cut2(test.source$Age_35_44_yr_F, g=2))
test.source$Age_35_44_yr_M2 <- as.numeric(cut2(test.source$Age_35_44_yr_M, g=2))
test.source$Age_45_54_yr_F2 <- as.numeric(cut2(test.source$Age_45_54_yr_F, g=2))
test.source$Age_45_54_yr_M2 <- as.numeric(cut2(test.source$Age_45_54_yr_M, g=2))
test.source$Age_5_14_yr_F2 <- as.numeric(cut2(test.source$Age_5_14_yr_F, g=2))
test.source$Age_5_14_yr_M2 <- as.numeric(cut2(test.source$Age_5_14_yr_M, g=2))
test.source$Age_55_64_yr_F2 <- as.numeric(cut2(test.source$Age_55_64_yr_F, g=2))
test.source$Age_55_64_yr_M2 <- as.numeric(cut2(test.source$Age_55_64_yr_M, g=2))
test.source$Age_65_74_yr_F2 <- as.numeric(cut2(test.source$Age_65_74_yr_F, g=2))
test.source$Age_65_74_yr_M2 <- as.numeric(cut2(test.source$Age_65_74_yr_M, g=2))
test.source$Age_75_84_yr_F2 <- as.numeric(cut2(test.source$Age_75_84_yr_F, g=2))
test.source$Age_75_84_yr_M2 <- as.numeric(cut2(test.source$Age_75_84_yr_M, g=2))
test.source$Age_85ov_F2 <- as.numeric(cut2(test.source$Age_85ov_F, g=2))
test.source$Age_85ov_M2 <- as.numeric(cut2(test.source$Age_85ov_M, g=2))
test.source$AREA_SQKM2 <- as.numeric(cut2(test.source$AREA_SQKM, g=2))
test.source$Australian_citizen_F2 <- as.numeric(cut2(test.source$Australian_citizen_F, g=2))
test.source$Australian_citizen_M2 <- as.numeric(cut2(test.source$Australian_citizen_M, g=2))
test.source$Average_household_size2 <- as.numeric(cut2(test.source$Average_household_size, g=2))
test.source$Average_num_psns_per_bedroom2 <- as.numeric(cut2(test.source$Average_num_psns_per_bedroom, g=2))
test.source$Birthplace_Australia_F2 <- as.numeric(cut2(test.source$Birthplace_Australia_F, g=2))
test.source$Birthplace_Australia_M2 <- as.numeric(cut2(test.source$Birthplace_Australia_M, g=2))
test.source$Birthplace_Elsewhere_F2 <- as.numeric(cut2(test.source$Birthplace_Elsewhere_F, g=2))
test.source$Birthplace_Elsewhere_M2 <- as.numeric(cut2(test.source$Birthplace_Elsewhere_M, g=2))
test.source$Buddhism_F2 <- as.numeric(cut2(test.source$Buddhism_F, g=2))
test.source$Buddhism_M2 <- as.numeric(cut2(test.source$Buddhism_M, g=2))
test.source$Christianity_Tot_F2 <- as.numeric(cut2(test.source$Christianity_Tot_F, g=2))
test.source$Christianity_Tot_M2 <- as.numeric(cut2(test.source$Christianity_Tot_M, g=2))
test.source$Count_Census_Nt_Ewhere_Aust_F2 <- as.numeric(cut2(test.source$Count_Census_Nt_Ewhere_Aust_F, g=2))
test.source$Count_Census_Nt_Ewhere_Aust_M2 <- as.numeric(cut2(test.source$Count_Census_Nt_Ewhere_Aust_M, g=2))
test.source$Count_Persons_other_dwgs_F2 <- as.numeric(cut2(test.source$Count_Persons_other_dwgs_F, g=2))
test.source$Count_Persons_other_dwgs_M2 <- as.numeric(cut2(test.source$Count_Persons_other_dwgs_M, g=2))
test.source$Count_psns_occ_priv_dwgs_F2 <- as.numeric(cut2(test.source$Count_psns_occ_priv_dwgs_F, g=2))
test.source$Count_psns_occ_priv_dwgs_M2 <- as.numeric(cut2(test.source$Count_psns_occ_priv_dwgs_M, g=2))
test.source$Counted_Census_Night_home_F2 <- as.numeric(cut2(test.source$Counted_Census_Night_home_F, g=2))
test.source$Counted_Census_Night_home_M2 <- as.numeric(cut2(test.source$Counted_Census_Night_home_M, g=2))
test.source$Density2 <- as.numeric(cut2(test.source$Density, g=2))
test.source$Dist_Major_City2 <- as.numeric(cut2(test.source$Dist_Major_City, g=2))
test.source$F_BachDeg_Total2 <- as.numeric(cut2(test.source$F_BachDeg_Total, g=2))
test.source$F_PGrad_Deg_Total2 <- as.numeric(cut2(test.source$F_PGrad_Deg_Total, g=2))
test.source$F_Tot_Divorced2 <- as.numeric(cut2(test.source$F_Tot_Divorced, g=2))
test.source$F_Tot_Married2 <- as.numeric(cut2(test.source$F_Tot_Married, g=2))
test.source$F_Tot_N_a_volunteer2 <- as.numeric(cut2(test.source$F_Tot_N_a_volunteer, g=2))
test.source$F_Tot_Never_Married2 <- as.numeric(cut2(test.source$F_Tot_Never_Married, g=2))
test.source$F_Tot_Separated2 <- as.numeric(cut2(test.source$F_Tot_Separated, g=2))
test.source$F_Tot_Volunteer2 <- as.numeric(cut2(test.source$F_Tot_Volunteer, g=2))
test.source$F_Tot_Widowed2 <- as.numeric(cut2(test.source$F_Tot_Widowed, g=2))
test.source$Foreign2 <- as.numeric(cut2(test.source$Foreign, g=2))
test.source$High_yr_schl_comp_Yr_12_eq_F2 <- as.numeric(cut2(test.source$High_yr_schl_comp_Yr_12_eq_F, g=2))
test.source$High_yr_schl_comp_Yr_12_eq_M2 <- as.numeric(cut2(test.source$High_yr_schl_comp_Yr_12_eq_M, g=2))
test.source$Hinduism_F2 <- as.numeric(cut2(test.source$Hinduism_F, g=2))
test.source$Hinduism_M2 <- as.numeric(cut2(test.source$Hinduism_M, g=2))
test.source$Indig_Bth_Abor_Torres_St_Is_F2 <- as.numeric(cut2(test.source$Indig_Bth_Abor_Torres_St_Is_F, g=2))
test.source$Indig_Bth_Abor_Torres_St_Is_M2 <- as.numeric(cut2(test.source$Indig_Bth_Abor_Torres_St_Is_M, g=2))
test.source$Indig_psns_Torres_Strait_Is_F2 <- as.numeric(cut2(test.source$Indig_psns_Torres_Strait_Is_F, g=2))
test.source$Indig_psns_Torres_Strait_Is_M2 <- as.numeric(cut2(test.source$Indig_psns_Torres_Strait_Is_M, g=2))
test.source$Indigenous_P_Tot_F2 <- as.numeric(cut2(test.source$Indigenous_P_Tot_F, g=2))
test.source$Indigenous_P_Tot_M2 <- as.numeric(cut2(test.source$Indigenous_P_Tot_M, g=2))
test.source$Indigenous_psns_Aboriginal_F2 <- as.numeric(cut2(test.source$Indigenous_psns_Aboriginal_F, g=2))
test.source$Indigenous_psns_Aboriginal_M2 <- as.numeric(cut2(test.source$Indigenous_psns_Aboriginal_M, g=2))
test.source$Infant_Mort2 <- as.numeric(cut2(test.source$Infant_Mort, g=2))
test.source$Islam_F2 <- as.numeric(cut2(test.source$Islam_F, g=2))
test.source$Islam_M2 <- as.numeric(cut2(test.source$Islam_M, g=2))
test.source$Judaism_F2 <- as.numeric(cut2(test.source$Judaism_F, g=2))
test.source$Judaism_M2 <- as.numeric(cut2(test.source$Judaism_M, g=2))
test.source$Lang_spoken_home_Eng_only_F2 <- as.numeric(cut2(test.source$Lang_spoken_home_Eng_only_F, g=2))
test.source$Lang_spoken_home_Eng_only_M2 <- as.numeric(cut2(test.source$Lang_spoken_home_Eng_only_M, g=2))
test.source$Lang_spoken_home_Oth_Lang_F2 <- as.numeric(cut2(test.source$Lang_spoken_home_Oth_Lang_F, g=2))
test.source$Lang_spoken_home_Oth_Lang_M2 <- as.numeric(cut2(test.source$Lang_spoken_home_Oth_Lang_M, g=2))
test.source$Lat2 <- as.numeric(cut2(test.source$Lat, g=2))
test.source$lfs_Tot_LF_F2 <- as.numeric(cut2(test.source$lfs_Tot_LF_F, g=2))
test.source$lfs_Tot_LF_M2 <- as.numeric(cut2(test.source$lfs_Tot_LF_M, g=2))
test.source$lfs_Unmplyed_lookng_for_wrk_F2 <- as.numeric(cut2(test.source$lfs_Unmplyed_lookng_for_wrk_F, g=2))
test.source$lfs_Unmplyed_lookng_for_wrk_M2 <- as.numeric(cut2(test.source$lfs_Unmplyed_lookng_for_wrk_M, g=2))
test.source$Long2 <- as.numeric(cut2(test.source$Long, g=2))
test.source$M_BachDeg_Total2 <- as.numeric(cut2(test.source$M_BachDeg_Total, g=2))
test.source$M_PGrad_Deg_Total2 <- as.numeric(cut2(test.source$M_PGrad_Deg_Total, g=2))
test.source$M_Tot_Divorced2 <- as.numeric(cut2(test.source$M_Tot_Divorced, g=2))
test.source$M_Tot_Married2 <- as.numeric(cut2(test.source$M_Tot_Married, g=2))
test.source$M_Tot_N_a_volunteer2 <- as.numeric(cut2(test.source$M_Tot_N_a_volunteer, g=2))
test.source$M_Tot_Never_Married2 <- as.numeric(cut2(test.source$M_Tot_Never_Married, g=2))
test.source$M_Tot_Separated2 <- as.numeric(cut2(test.source$M_Tot_Separated, g=2))
test.source$M_Tot_Volunteer2 <- as.numeric(cut2(test.source$M_Tot_Volunteer, g=2))
test.source$M_Tot_Widowed2 <- as.numeric(cut2(test.source$M_Tot_Widowed, g=2))
test.source$Male_Ratio2 <- as.numeric(cut2(test.source$Male_Ratio, g=2))
test.source$Marriage_Prob2 <- as.numeric(cut2(test.source$Marriage_Prob, g=2))
test.source$Median_mortgage_repay_monthly2 <- as.numeric(cut2(test.source$Median_mortgage_repay_monthly, g=2))
test.source$Median_Tot_fam_inc_weekly2 <- as.numeric(cut2(test.source$Median_Tot_fam_inc_weekly, g=2))
test.source$Median_Tot_hhd_inc_weekly2 <- as.numeric(cut2(test.source$Median_Tot_hhd_inc_weekly, g=2))
test.source$Median_Tot_prsnl_inc_weekly2 <- as.numeric(cut2(test.source$Median_Tot_prsnl_inc_weekly, g=2))
test.source$No_Religion_F2 <- as.numeric(cut2(test.source$No_Religion_F, g=2))
test.source$No_Religion_M2 <- as.numeric(cut2(test.source$No_Religion_M, g=2))
test.source$O_MTG_Total2 <- as.numeric(cut2(test.source$O_MTG_Total, g=2))
test.source$O_OR_Total2 <- as.numeric(cut2(test.source$O_OR_Total, g=2))
test.source$Old_Age_F2 <- as.numeric(cut2(test.source$Old_Age_F, g=2))
test.source$Old_Age_M2 <- as.numeric(cut2(test.source$Old_Age_M, g=2))
test.source$Other_Religions_Tot_F2 <- as.numeric(cut2(test.source$Other_Religions_Tot_F, g=2))
test.source$Other_Religions_Tot_M2 <- as.numeric(cut2(test.source$Other_Religions_Tot_M, g=2))
test.source$R_Tot_Total2 <- as.numeric(cut2(test.source$R_Tot_Total, g=2))
test.source$SEO_Females2 <- as.numeric(cut2(test.source$SEO_Females, g=2))
test.source$SEO_Males2 <- as.numeric(cut2(test.source$SEO_Males, g=2))
test.source$Stay_at_home2 <- as.numeric(cut2(test.source$Stay_at_home, g=2))
test.source$Tot_Indig_status_ns_F2 <- as.numeric(cut2(test.source$Tot_Indig_status_ns_F, g=2))
test.source$Tot_Indig_status_ns_M2 <- as.numeric(cut2(test.source$Tot_Indig_status_ns_M, g=2))
test.source$Tot_Indigenous_F2 <- as.numeric(cut2(test.source$Tot_Indigenous_F, g=2))
test.source$Tot_Indigenous_M2 <- as.numeric(cut2(test.source$Tot_Indigenous_M, g=2))
test.source$Tot_Non_Indigenous_F2 <- as.numeric(cut2(test.source$Tot_Non_Indigenous_F, g=2))
test.source$Tot_Non_Indigenous_M2 <- as.numeric(cut2(test.source$Tot_Non_Indigenous_M, g=2))
test.source$Total_PDs_Dwellings2 <- as.numeric(cut2(test.source$Total_PDs_Dwellings, g=2))
test.source$Total_PDs_Persons2 <- as.numeric(cut2(test.source$Total_PDs_Persons, g=2))
test.source$Total_Total2 <- as.numeric(cut2(test.source$Total_Total, g=2))
test.source$Unemp2 <- as.numeric(cut2(test.source$Unemp, g=2))

#cut data 3 ways
train.source$Aboriginal3 <- as.numeric(cut2(train.source$Aboriginal, g=3))
train.source$Accidental_Mort3 <- as.numeric(cut2(train.source$Accidental_Mort, g=3))
train.source$Age_0_4_yr_F3 <- as.numeric(cut2(train.source$Age_0_4_yr_F, g=3))
train.source$Age_0_4_yr_M3 <- as.numeric(cut2(train.source$Age_0_4_yr_M, g=3))
train.source$Age_15_19_yr_F3 <- as.numeric(cut2(train.source$Age_15_19_yr_F, g=3))
train.source$Age_15_19_yr_M3 <- as.numeric(cut2(train.source$Age_15_19_yr_M, g=3))
train.source$Age_20_24_yr_F3 <- as.numeric(cut2(train.source$Age_20_24_yr_F, g=3))
train.source$Age_20_24_yr_M3 <- as.numeric(cut2(train.source$Age_20_24_yr_M, g=3))
train.source$Age_25_34_yr_F3 <- as.numeric(cut2(train.source$Age_25_34_yr_F, g=3))
train.source$Age_25_34_yr_M3 <- as.numeric(cut2(train.source$Age_25_34_yr_M, g=3))
train.source$Age_35_44_yr_F3 <- as.numeric(cut2(train.source$Age_35_44_yr_F, g=3))
train.source$Age_35_44_yr_M3 <- as.numeric(cut2(train.source$Age_35_44_yr_M, g=3))
train.source$Age_45_54_yr_F3 <- as.numeric(cut2(train.source$Age_45_54_yr_F, g=3))
train.source$Age_45_54_yr_M3 <- as.numeric(cut2(train.source$Age_45_54_yr_M, g=3))
train.source$Age_5_14_yr_F3 <- as.numeric(cut2(train.source$Age_5_14_yr_F, g=3))
train.source$Age_5_14_yr_M3 <- as.numeric(cut2(train.source$Age_5_14_yr_M, g=3))
train.source$Age_55_64_yr_F3 <- as.numeric(cut2(train.source$Age_55_64_yr_F, g=3))
train.source$Age_55_64_yr_M3 <- as.numeric(cut2(train.source$Age_55_64_yr_M, g=3))
train.source$Age_65_74_yr_F3 <- as.numeric(cut2(train.source$Age_65_74_yr_F, g=3))
train.source$Age_65_74_yr_M3 <- as.numeric(cut2(train.source$Age_65_74_yr_M, g=3))
train.source$Age_75_84_yr_F3 <- as.numeric(cut2(train.source$Age_75_84_yr_F, g=3))
train.source$Age_75_84_yr_M3 <- as.numeric(cut2(train.source$Age_75_84_yr_M, g=3))
train.source$Age_85ov_F3 <- as.numeric(cut2(train.source$Age_85ov_F, g=3))
train.source$Age_85ov_M3 <- as.numeric(cut2(train.source$Age_85ov_M, g=3))
train.source$AREA_SQKM3 <- as.numeric(cut2(train.source$AREA_SQKM, g=3))
train.source$Australian_citizen_F3 <- as.numeric(cut2(train.source$Australian_citizen_F, g=3))
train.source$Australian_citizen_M3 <- as.numeric(cut2(train.source$Australian_citizen_M, g=3))
train.source$Average_household_size3 <- as.numeric(cut2(train.source$Average_household_size, g=3))
train.source$Average_num_psns_per_bedroom3 <- as.numeric(cut2(train.source$Average_num_psns_per_bedroom, g=3))
train.source$Birthplace_Australia_F3 <- as.numeric(cut2(train.source$Birthplace_Australia_F, g=3))
train.source$Birthplace_Australia_M3 <- as.numeric(cut2(train.source$Birthplace_Australia_M, g=3))
train.source$Birthplace_Elsewhere_F3 <- as.numeric(cut2(train.source$Birthplace_Elsewhere_F, g=3))
train.source$Birthplace_Elsewhere_M3 <- as.numeric(cut2(train.source$Birthplace_Elsewhere_M, g=3))
train.source$Buddhism_F3 <- as.numeric(cut2(train.source$Buddhism_F, g=3))
train.source$Buddhism_M3 <- as.numeric(cut2(train.source$Buddhism_M, g=3))
train.source$Christianity_Tot_F3 <- as.numeric(cut2(train.source$Christianity_Tot_F, g=3))
train.source$Christianity_Tot_M3 <- as.numeric(cut2(train.source$Christianity_Tot_M, g=3))
train.source$Count_Census_Nt_Ewhere_Aust_F3 <- as.numeric(cut2(train.source$Count_Census_Nt_Ewhere_Aust_F, g=3))
train.source$Count_Census_Nt_Ewhere_Aust_M3 <- as.numeric(cut2(train.source$Count_Census_Nt_Ewhere_Aust_M, g=3))
train.source$Count_Persons_other_dwgs_F3 <- as.numeric(cut2(train.source$Count_Persons_other_dwgs_F, g=3))
train.source$Count_Persons_other_dwgs_M3 <- as.numeric(cut2(train.source$Count_Persons_other_dwgs_M, g=3))
train.source$Count_psns_occ_priv_dwgs_F3 <- as.numeric(cut2(train.source$Count_psns_occ_priv_dwgs_F, g=3))
train.source$Count_psns_occ_priv_dwgs_M3 <- as.numeric(cut2(train.source$Count_psns_occ_priv_dwgs_M, g=3))
train.source$Counted_Census_Night_home_F3 <- as.numeric(cut2(train.source$Counted_Census_Night_home_F, g=3))
train.source$Counted_Census_Night_home_M3 <- as.numeric(cut2(train.source$Counted_Census_Night_home_M, g=3))
train.source$Density3 <- as.numeric(cut2(train.source$Density, g=3))
train.source$Dist_Major_City3 <- as.numeric(cut2(train.source$Dist_Major_City, g=3))
train.source$F_BachDeg_Total3 <- as.numeric(cut2(train.source$F_BachDeg_Total, g=3))
train.source$F_PGrad_Deg_Total3 <- as.numeric(cut2(train.source$F_PGrad_Deg_Total, g=3))
train.source$F_Tot_Divorced3 <- as.numeric(cut2(train.source$F_Tot_Divorced, g=3))
train.source$F_Tot_Married3 <- as.numeric(cut2(train.source$F_Tot_Married, g=3))
train.source$F_Tot_N_a_volunteer3 <- as.numeric(cut2(train.source$F_Tot_N_a_volunteer, g=3))
train.source$F_Tot_Never_Married3 <- as.numeric(cut2(train.source$F_Tot_Never_Married, g=3))
train.source$F_Tot_Separated3 <- as.numeric(cut2(train.source$F_Tot_Separated, g=3))
train.source$F_Tot_Volunteer3 <- as.numeric(cut2(train.source$F_Tot_Volunteer, g=3))
train.source$F_Tot_Widowed3 <- as.numeric(cut2(train.source$F_Tot_Widowed, g=3))
train.source$Foreign3 <- as.numeric(cut2(train.source$Foreign, g=3))
train.source$High_yr_schl_comp_Yr_12_eq_F3 <- as.numeric(cut2(train.source$High_yr_schl_comp_Yr_12_eq_F, g=3))
train.source$High_yr_schl_comp_Yr_12_eq_M3 <- as.numeric(cut2(train.source$High_yr_schl_comp_Yr_12_eq_M, g=3))
train.source$Hinduism_F3 <- as.numeric(cut2(train.source$Hinduism_F, g=3))
train.source$Hinduism_M3 <- as.numeric(cut2(train.source$Hinduism_M, g=3))
train.source$Indig_Bth_Abor_Torres_St_Is_F3 <- as.numeric(cut2(train.source$Indig_Bth_Abor_Torres_St_Is_F, g=3))
train.source$Indig_Bth_Abor_Torres_St_Is_M3 <- as.numeric(cut2(train.source$Indig_Bth_Abor_Torres_St_Is_M, g=3))
train.source$Indig_psns_Torres_Strait_Is_F3 <- as.numeric(cut2(train.source$Indig_psns_Torres_Strait_Is_F, g=3))
train.source$Indig_psns_Torres_Strait_Is_M3 <- as.numeric(cut2(train.source$Indig_psns_Torres_Strait_Is_M, g=3))
train.source$Indigenous_P_Tot_F3 <- as.numeric(cut2(train.source$Indigenous_P_Tot_F, g=3))
train.source$Indigenous_P_Tot_M3 <- as.numeric(cut2(train.source$Indigenous_P_Tot_M, g=3))
train.source$Indigenous_psns_Aboriginal_F3 <- as.numeric(cut2(train.source$Indigenous_psns_Aboriginal_F, g=3))
train.source$Indigenous_psns_Aboriginal_M3 <- as.numeric(cut2(train.source$Indigenous_psns_Aboriginal_M, g=3))
train.source$Infant_Mort3 <- as.numeric(cut2(train.source$Infant_Mort, g=3))
train.source$Islam_F3 <- as.numeric(cut2(train.source$Islam_F, g=3))
train.source$Islam_M3 <- as.numeric(cut2(train.source$Islam_M, g=3))
train.source$Judaism_F3 <- as.numeric(cut2(train.source$Judaism_F, g=3))
train.source$Judaism_M3 <- as.numeric(cut2(train.source$Judaism_M, g=3))
train.source$Lang_spoken_home_Eng_only_F3 <- as.numeric(cut2(train.source$Lang_spoken_home_Eng_only_F, g=3))
train.source$Lang_spoken_home_Eng_only_M3 <- as.numeric(cut2(train.source$Lang_spoken_home_Eng_only_M, g=3))
train.source$Lang_spoken_home_Oth_Lang_F3 <- as.numeric(cut2(train.source$Lang_spoken_home_Oth_Lang_F, g=3))
train.source$Lang_spoken_home_Oth_Lang_M3 <- as.numeric(cut2(train.source$Lang_spoken_home_Oth_Lang_M, g=3))
train.source$Lat3 <- as.numeric(cut2(train.source$Lat, g=3))
train.source$lfs_Tot_LF_F3 <- as.numeric(cut2(train.source$lfs_Tot_LF_F, g=3))
train.source$lfs_Tot_LF_M3 <- as.numeric(cut2(train.source$lfs_Tot_LF_M, g=3))
train.source$lfs_Unmplyed_lookng_for_wrk_F3 <- as.numeric(cut2(train.source$lfs_Unmplyed_lookng_for_wrk_F, g=3))
train.source$lfs_Unmplyed_lookng_for_wrk_M3 <- as.numeric(cut2(train.source$lfs_Unmplyed_lookng_for_wrk_M, g=3))
train.source$Long3 <- as.numeric(cut2(train.source$Long, g=3))
train.source$M_BachDeg_Total3 <- as.numeric(cut2(train.source$M_BachDeg_Total, g=3))
train.source$M_PGrad_Deg_Total3 <- as.numeric(cut2(train.source$M_PGrad_Deg_Total, g=3))
train.source$M_Tot_Divorced3 <- as.numeric(cut2(train.source$M_Tot_Divorced, g=3))
train.source$M_Tot_Married3 <- as.numeric(cut2(train.source$M_Tot_Married, g=3))
train.source$M_Tot_N_a_volunteer3 <- as.numeric(cut2(train.source$M_Tot_N_a_volunteer, g=3))
train.source$M_Tot_Never_Married3 <- as.numeric(cut2(train.source$M_Tot_Never_Married, g=3))
train.source$M_Tot_Separated3 <- as.numeric(cut2(train.source$M_Tot_Separated, g=3))
train.source$M_Tot_Volunteer3 <- as.numeric(cut2(train.source$M_Tot_Volunteer, g=3))
train.source$M_Tot_Widowed3 <- as.numeric(cut2(train.source$M_Tot_Widowed, g=3))
train.source$Male_Ratio3 <- as.numeric(cut2(train.source$Male_Ratio, g=3))
train.source$Marriage_Prob3 <- as.numeric(cut2(train.source$Marriage_Prob, g=3))
train.source$Median_mortgage_repay_monthly3 <- as.numeric(cut2(train.source$Median_mortgage_repay_monthly, g=3))
train.source$Median_Tot_fam_inc_weekly3 <- as.numeric(cut2(train.source$Median_Tot_fam_inc_weekly, g=3))
train.source$Median_Tot_hhd_inc_weekly3 <- as.numeric(cut2(train.source$Median_Tot_hhd_inc_weekly, g=3))
train.source$Median_Tot_prsnl_inc_weekly3 <- as.numeric(cut2(train.source$Median_Tot_prsnl_inc_weekly, g=3))
train.source$No_Religion_F3 <- as.numeric(cut2(train.source$No_Religion_F, g=3))
train.source$No_Religion_M3 <- as.numeric(cut2(train.source$No_Religion_M, g=3))
train.source$O_MTG_Total3 <- as.numeric(cut2(train.source$O_MTG_Total, g=3))
train.source$O_OR_Total3 <- as.numeric(cut2(train.source$O_OR_Total, g=3))
train.source$Old_Age_F3 <- as.numeric(cut2(train.source$Old_Age_F, g=3))
train.source$Old_Age_M3 <- as.numeric(cut2(train.source$Old_Age_M, g=3))
train.source$Other_Religions_Tot_F3 <- as.numeric(cut2(train.source$Other_Religions_Tot_F, g=3))
train.source$Other_Religions_Tot_M3 <- as.numeric(cut2(train.source$Other_Religions_Tot_M, g=3))
train.source$R_Tot_Total3 <- as.numeric(cut2(train.source$R_Tot_Total, g=3))
train.source$SEO_Females3 <- as.numeric(cut2(train.source$SEO_Females, g=3))
train.source$SEO_Males3 <- as.numeric(cut2(train.source$SEO_Males, g=3))
train.source$Stay_at_home3 <- as.numeric(cut2(train.source$Stay_at_home, g=3))
train.source$Tot_Indig_status_ns_F3 <- as.numeric(cut2(train.source$Tot_Indig_status_ns_F, g=3))
train.source$Tot_Indig_status_ns_M3 <- as.numeric(cut2(train.source$Tot_Indig_status_ns_M, g=3))
train.source$Tot_Indigenous_F3 <- as.numeric(cut2(train.source$Tot_Indigenous_F, g=3))
train.source$Tot_Indigenous_M3 <- as.numeric(cut2(train.source$Tot_Indigenous_M, g=3))
train.source$Tot_Non_Indigenous_F3 <- as.numeric(cut2(train.source$Tot_Non_Indigenous_F, g=3))
train.source$Tot_Non_Indigenous_M3 <- as.numeric(cut2(train.source$Tot_Non_Indigenous_M, g=3))
train.source$Total_PDs_Dwellings3 <- as.numeric(cut2(train.source$Total_PDs_Dwellings, g=3))
train.source$Total_PDs_Persons3 <- as.numeric(cut2(train.source$Total_PDs_Persons, g=3))
train.source$Total_Total3 <- as.numeric(cut2(train.source$Total_Total, g=3))
train.source$Unemp3 <- as.numeric(cut2(train.source$Unemp, g=3))

test.source$Aboriginal3 <- as.numeric(cut2(test.source$Aboriginal, g=3))
test.source$Accidental_Mort3 <- as.numeric(cut2(test.source$Accidental_Mort, g=3))
test.source$Age_0_4_yr_F3 <- as.numeric(cut2(test.source$Age_0_4_yr_F, g=3))
test.source$Age_0_4_yr_M3 <- as.numeric(cut2(test.source$Age_0_4_yr_M, g=3))
test.source$Age_15_19_yr_F3 <- as.numeric(cut2(test.source$Age_15_19_yr_F, g=3))
test.source$Age_15_19_yr_M3 <- as.numeric(cut2(test.source$Age_15_19_yr_M, g=3))
test.source$Age_20_24_yr_F3 <- as.numeric(cut2(test.source$Age_20_24_yr_F, g=3))
test.source$Age_20_24_yr_M3 <- as.numeric(cut2(test.source$Age_20_24_yr_M, g=3))
test.source$Age_25_34_yr_F3 <- as.numeric(cut2(test.source$Age_25_34_yr_F, g=3))
test.source$Age_25_34_yr_M3 <- as.numeric(cut2(test.source$Age_25_34_yr_M, g=3))
test.source$Age_35_44_yr_F3 <- as.numeric(cut2(test.source$Age_35_44_yr_F, g=3))
test.source$Age_35_44_yr_M3 <- as.numeric(cut2(test.source$Age_35_44_yr_M, g=3))
test.source$Age_45_54_yr_F3 <- as.numeric(cut2(test.source$Age_45_54_yr_F, g=3))
test.source$Age_45_54_yr_M3 <- as.numeric(cut2(test.source$Age_45_54_yr_M, g=3))
test.source$Age_5_14_yr_F3 <- as.numeric(cut2(test.source$Age_5_14_yr_F, g=3))
test.source$Age_5_14_yr_M3 <- as.numeric(cut2(test.source$Age_5_14_yr_M, g=3))
test.source$Age_55_64_yr_F3 <- as.numeric(cut2(test.source$Age_55_64_yr_F, g=3))
test.source$Age_55_64_yr_M3 <- as.numeric(cut2(test.source$Age_55_64_yr_M, g=3))
test.source$Age_65_74_yr_F3 <- as.numeric(cut2(test.source$Age_65_74_yr_F, g=3))
test.source$Age_65_74_yr_M3 <- as.numeric(cut2(test.source$Age_65_74_yr_M, g=3))
test.source$Age_75_84_yr_F3 <- as.numeric(cut2(test.source$Age_75_84_yr_F, g=3))
test.source$Age_75_84_yr_M3 <- as.numeric(cut2(test.source$Age_75_84_yr_M, g=3))
test.source$Age_85ov_F3 <- as.numeric(cut2(test.source$Age_85ov_F, g=3))
test.source$Age_85ov_M3 <- as.numeric(cut2(test.source$Age_85ov_M, g=3))
test.source$AREA_SQKM3 <- as.numeric(cut2(test.source$AREA_SQKM, g=3))
test.source$Australian_citizen_F3 <- as.numeric(cut2(test.source$Australian_citizen_F, g=3))
test.source$Australian_citizen_M3 <- as.numeric(cut2(test.source$Australian_citizen_M, g=3))
test.source$Average_household_size3 <- as.numeric(cut2(test.source$Average_household_size, g=3))
test.source$Average_num_psns_per_bedroom3 <- as.numeric(cut2(test.source$Average_num_psns_per_bedroom, g=3))
test.source$Birthplace_Australia_F3 <- as.numeric(cut2(test.source$Birthplace_Australia_F, g=3))
test.source$Birthplace_Australia_M3 <- as.numeric(cut2(test.source$Birthplace_Australia_M, g=3))
test.source$Birthplace_Elsewhere_F3 <- as.numeric(cut2(test.source$Birthplace_Elsewhere_F, g=3))
test.source$Birthplace_Elsewhere_M3 <- as.numeric(cut2(test.source$Birthplace_Elsewhere_M, g=3))
test.source$Buddhism_F3 <- as.numeric(cut2(test.source$Buddhism_F, g=3))
test.source$Buddhism_M3 <- as.numeric(cut2(test.source$Buddhism_M, g=3))
test.source$Christianity_Tot_F3 <- as.numeric(cut2(test.source$Christianity_Tot_F, g=3))
test.source$Christianity_Tot_M3 <- as.numeric(cut2(test.source$Christianity_Tot_M, g=3))
test.source$Count_Census_Nt_Ewhere_Aust_F3 <- as.numeric(cut2(test.source$Count_Census_Nt_Ewhere_Aust_F, g=3))
test.source$Count_Census_Nt_Ewhere_Aust_M3 <- as.numeric(cut2(test.source$Count_Census_Nt_Ewhere_Aust_M, g=3))
test.source$Count_Persons_other_dwgs_F3 <- as.numeric(cut2(test.source$Count_Persons_other_dwgs_F, g=3))
test.source$Count_Persons_other_dwgs_M3 <- as.numeric(cut2(test.source$Count_Persons_other_dwgs_M, g=3))
test.source$Count_psns_occ_priv_dwgs_F3 <- as.numeric(cut2(test.source$Count_psns_occ_priv_dwgs_F, g=3))
test.source$Count_psns_occ_priv_dwgs_M3 <- as.numeric(cut2(test.source$Count_psns_occ_priv_dwgs_M, g=3))
test.source$Counted_Census_Night_home_F3 <- as.numeric(cut2(test.source$Counted_Census_Night_home_F, g=3))
test.source$Counted_Census_Night_home_M3 <- as.numeric(cut2(test.source$Counted_Census_Night_home_M, g=3))
test.source$Density3 <- as.numeric(cut2(test.source$Density, g=3))
test.source$Dist_Major_City3 <- as.numeric(cut2(test.source$Dist_Major_City, g=3))
test.source$F_BachDeg_Total3 <- as.numeric(cut2(test.source$F_BachDeg_Total, g=3))
test.source$F_PGrad_Deg_Total3 <- as.numeric(cut2(test.source$F_PGrad_Deg_Total, g=3))
test.source$F_Tot_Divorced3 <- as.numeric(cut2(test.source$F_Tot_Divorced, g=3))
test.source$F_Tot_Married3 <- as.numeric(cut2(test.source$F_Tot_Married, g=3))
test.source$F_Tot_N_a_volunteer3 <- as.numeric(cut2(test.source$F_Tot_N_a_volunteer, g=3))
test.source$F_Tot_Never_Married3 <- as.numeric(cut2(test.source$F_Tot_Never_Married, g=3))
test.source$F_Tot_Separated3 <- as.numeric(cut2(test.source$F_Tot_Separated, g=3))
test.source$F_Tot_Volunteer3 <- as.numeric(cut2(test.source$F_Tot_Volunteer, g=3))
test.source$F_Tot_Widowed3 <- as.numeric(cut2(test.source$F_Tot_Widowed, g=3))
test.source$Foreign3 <- as.numeric(cut2(test.source$Foreign, g=3))
test.source$High_yr_schl_comp_Yr_12_eq_F3 <- as.numeric(cut2(test.source$High_yr_schl_comp_Yr_12_eq_F, g=3))
test.source$High_yr_schl_comp_Yr_12_eq_M3 <- as.numeric(cut2(test.source$High_yr_schl_comp_Yr_12_eq_M, g=3))
test.source$Hinduism_F3 <- as.numeric(cut2(test.source$Hinduism_F, g=3))
test.source$Hinduism_M3 <- as.numeric(cut2(test.source$Hinduism_M, g=3))
test.source$Indig_Bth_Abor_Torres_St_Is_F3 <- as.numeric(cut2(test.source$Indig_Bth_Abor_Torres_St_Is_F, g=3))
test.source$Indig_Bth_Abor_Torres_St_Is_M3 <- as.numeric(cut2(test.source$Indig_Bth_Abor_Torres_St_Is_M, g=3))
test.source$Indig_psns_Torres_Strait_Is_F3 <- as.numeric(cut2(test.source$Indig_psns_Torres_Strait_Is_F, g=3))
test.source$Indig_psns_Torres_Strait_Is_M3 <- as.numeric(cut2(test.source$Indig_psns_Torres_Strait_Is_M, g=3))
test.source$Indigenous_P_Tot_F3 <- as.numeric(cut2(test.source$Indigenous_P_Tot_F, g=3))
test.source$Indigenous_P_Tot_M3 <- as.numeric(cut2(test.source$Indigenous_P_Tot_M, g=3))
test.source$Indigenous_psns_Aboriginal_F3 <- as.numeric(cut2(test.source$Indigenous_psns_Aboriginal_F, g=3))
test.source$Indigenous_psns_Aboriginal_M3 <- as.numeric(cut2(test.source$Indigenous_psns_Aboriginal_M, g=3))
test.source$Infant_Mort3 <- as.numeric(cut2(test.source$Infant_Mort, g=3))
test.source$Islam_F3 <- as.numeric(cut2(test.source$Islam_F, g=3))
test.source$Islam_M3 <- as.numeric(cut2(test.source$Islam_M, g=3))
test.source$Judaism_F3 <- as.numeric(cut2(test.source$Judaism_F, g=3))
test.source$Judaism_M3 <- as.numeric(cut2(test.source$Judaism_M, g=3))
test.source$Lang_spoken_home_Eng_only_F3 <- as.numeric(cut2(test.source$Lang_spoken_home_Eng_only_F, g=3))
test.source$Lang_spoken_home_Eng_only_M3 <- as.numeric(cut2(test.source$Lang_spoken_home_Eng_only_M, g=3))
test.source$Lang_spoken_home_Oth_Lang_F3 <- as.numeric(cut2(test.source$Lang_spoken_home_Oth_Lang_F, g=3))
test.source$Lang_spoken_home_Oth_Lang_M3 <- as.numeric(cut2(test.source$Lang_spoken_home_Oth_Lang_M, g=3))
test.source$Lat3 <- as.numeric(cut2(test.source$Lat, g=3))
test.source$lfs_Tot_LF_F3 <- as.numeric(cut2(test.source$lfs_Tot_LF_F, g=3))
test.source$lfs_Tot_LF_M3 <- as.numeric(cut2(test.source$lfs_Tot_LF_M, g=3))
test.source$lfs_Unmplyed_lookng_for_wrk_F3 <- as.numeric(cut2(test.source$lfs_Unmplyed_lookng_for_wrk_F, g=3))
test.source$lfs_Unmplyed_lookng_for_wrk_M3 <- as.numeric(cut2(test.source$lfs_Unmplyed_lookng_for_wrk_M, g=3))
test.source$Long3 <- as.numeric(cut2(test.source$Long, g=3))
test.source$M_BachDeg_Total3 <- as.numeric(cut2(test.source$M_BachDeg_Total, g=3))
test.source$M_PGrad_Deg_Total3 <- as.numeric(cut2(test.source$M_PGrad_Deg_Total, g=3))
test.source$M_Tot_Divorced3 <- as.numeric(cut2(test.source$M_Tot_Divorced, g=3))
test.source$M_Tot_Married3 <- as.numeric(cut2(test.source$M_Tot_Married, g=3))
test.source$M_Tot_N_a_volunteer3 <- as.numeric(cut2(test.source$M_Tot_N_a_volunteer, g=3))
test.source$M_Tot_Never_Married3 <- as.numeric(cut2(test.source$M_Tot_Never_Married, g=3))
test.source$M_Tot_Separated3 <- as.numeric(cut2(test.source$M_Tot_Separated, g=3))
test.source$M_Tot_Volunteer3 <- as.numeric(cut2(test.source$M_Tot_Volunteer, g=3))
test.source$M_Tot_Widowed3 <- as.numeric(cut2(test.source$M_Tot_Widowed, g=3))
test.source$Male_Ratio3 <- as.numeric(cut2(test.source$Male_Ratio, g=3))
test.source$Marriage_Prob3 <- as.numeric(cut2(test.source$Marriage_Prob, g=3))
test.source$Median_mortgage_repay_monthly3 <- as.numeric(cut2(test.source$Median_mortgage_repay_monthly, g=3))
test.source$Median_Tot_fam_inc_weekly3 <- as.numeric(cut2(test.source$Median_Tot_fam_inc_weekly, g=3))
test.source$Median_Tot_hhd_inc_weekly3 <- as.numeric(cut2(test.source$Median_Tot_hhd_inc_weekly, g=3))
test.source$Median_Tot_prsnl_inc_weekly3 <- as.numeric(cut2(test.source$Median_Tot_prsnl_inc_weekly, g=3))
test.source$No_Religion_F3 <- as.numeric(cut2(test.source$No_Religion_F, g=3))
test.source$No_Religion_M3 <- as.numeric(cut2(test.source$No_Religion_M, g=3))
test.source$O_MTG_Total3 <- as.numeric(cut2(test.source$O_MTG_Total, g=3))
test.source$O_OR_Total3 <- as.numeric(cut2(test.source$O_OR_Total, g=3))
test.source$Old_Age_F3 <- as.numeric(cut2(test.source$Old_Age_F, g=3))
test.source$Old_Age_M3 <- as.numeric(cut2(test.source$Old_Age_M, g=3))
test.source$Other_Religions_Tot_F3 <- as.numeric(cut2(test.source$Other_Religions_Tot_F, g=3))
test.source$Other_Religions_Tot_M3 <- as.numeric(cut2(test.source$Other_Religions_Tot_M, g=3))
test.source$R_Tot_Total3 <- as.numeric(cut2(test.source$R_Tot_Total, g=3))
test.source$SEO_Females3 <- as.numeric(cut2(test.source$SEO_Females, g=3))
test.source$SEO_Males3 <- as.numeric(cut2(test.source$SEO_Males, g=3))
test.source$Stay_at_home3 <- as.numeric(cut2(test.source$Stay_at_home, g=3))
test.source$Tot_Indig_status_ns_F3 <- as.numeric(cut2(test.source$Tot_Indig_status_ns_F, g=3))
test.source$Tot_Indig_status_ns_M3 <- as.numeric(cut2(test.source$Tot_Indig_status_ns_M, g=3))
test.source$Tot_Indigenous_F3 <- as.numeric(cut2(test.source$Tot_Indigenous_F, g=3))
test.source$Tot_Indigenous_M3 <- as.numeric(cut2(test.source$Tot_Indigenous_M, g=3))
test.source$Tot_Non_Indigenous_F3 <- as.numeric(cut2(test.source$Tot_Non_Indigenous_F, g=3))
test.source$Tot_Non_Indigenous_M3 <- as.numeric(cut2(test.source$Tot_Non_Indigenous_M, g=3))
test.source$Total_PDs_Dwellings3 <- as.numeric(cut2(test.source$Total_PDs_Dwellings, g=3))
test.source$Total_PDs_Persons3 <- as.numeric(cut2(test.source$Total_PDs_Persons, g=3))
test.source$Total_Total3 <- as.numeric(cut2(test.source$Total_Total, g=3))
test.source$Unemp3 <- as.numeric(cut2(test.source$Unemp, g=3))

#cut data 4 ways
train.source$Aboriginal4 <- as.numeric(cut2(train.source$Aboriginal, g=4))
train.source$Accidental_Mort4 <- as.numeric(cut2(train.source$Accidental_Mort, g=4))
train.source$Age_0_4_yr_F4 <- as.numeric(cut2(train.source$Age_0_4_yr_F, g=4))
train.source$Age_0_4_yr_M4 <- as.numeric(cut2(train.source$Age_0_4_yr_M, g=4))
train.source$Age_15_19_yr_F4 <- as.numeric(cut2(train.source$Age_15_19_yr_F, g=4))
train.source$Age_15_19_yr_M4 <- as.numeric(cut2(train.source$Age_15_19_yr_M, g=4))
train.source$Age_20_24_yr_F4 <- as.numeric(cut2(train.source$Age_20_24_yr_F, g=4))
train.source$Age_20_24_yr_M4 <- as.numeric(cut2(train.source$Age_20_24_yr_M, g=4))
train.source$Age_25_34_yr_F4 <- as.numeric(cut2(train.source$Age_25_34_yr_F, g=4))
train.source$Age_25_34_yr_M4 <- as.numeric(cut2(train.source$Age_25_34_yr_M, g=4))
train.source$Age_35_44_yr_F4 <- as.numeric(cut2(train.source$Age_35_44_yr_F, g=4))
train.source$Age_35_44_yr_M4 <- as.numeric(cut2(train.source$Age_35_44_yr_M, g=4))
train.source$Age_45_54_yr_F4 <- as.numeric(cut2(train.source$Age_45_54_yr_F, g=4))
train.source$Age_45_54_yr_M4 <- as.numeric(cut2(train.source$Age_45_54_yr_M, g=4))
train.source$Age_5_14_yr_F4 <- as.numeric(cut2(train.source$Age_5_14_yr_F, g=4))
train.source$Age_5_14_yr_M4 <- as.numeric(cut2(train.source$Age_5_14_yr_M, g=4))
train.source$Age_55_64_yr_F4 <- as.numeric(cut2(train.source$Age_55_64_yr_F, g=4))
train.source$Age_55_64_yr_M4 <- as.numeric(cut2(train.source$Age_55_64_yr_M, g=4))
train.source$Age_65_74_yr_F4 <- as.numeric(cut2(train.source$Age_65_74_yr_F, g=4))
train.source$Age_65_74_yr_M4 <- as.numeric(cut2(train.source$Age_65_74_yr_M, g=4))
train.source$Age_75_84_yr_F4 <- as.numeric(cut2(train.source$Age_75_84_yr_F, g=4))
train.source$Age_75_84_yr_M4 <- as.numeric(cut2(train.source$Age_75_84_yr_M, g=4))
train.source$Age_85ov_F4 <- as.numeric(cut2(train.source$Age_85ov_F, g=4))
train.source$Age_85ov_M4 <- as.numeric(cut2(train.source$Age_85ov_M, g=4))
train.source$AREA_SQKM4 <- as.numeric(cut2(train.source$AREA_SQKM, g=4))
train.source$Australian_citizen_F4 <- as.numeric(cut2(train.source$Australian_citizen_F, g=4))
train.source$Australian_citizen_M4 <- as.numeric(cut2(train.source$Australian_citizen_M, g=4))
train.source$Average_household_size4 <- as.numeric(cut2(train.source$Average_household_size, g=4))
train.source$Average_num_psns_per_bedroom4 <- as.numeric(cut2(train.source$Average_num_psns_per_bedroom, g=4))
train.source$Birthplace_Australia_F4 <- as.numeric(cut2(train.source$Birthplace_Australia_F, g=4))
train.source$Birthplace_Australia_M4 <- as.numeric(cut2(train.source$Birthplace_Australia_M, g=4))
train.source$Birthplace_Elsewhere_F4 <- as.numeric(cut2(train.source$Birthplace_Elsewhere_F, g=4))
train.source$Birthplace_Elsewhere_M4 <- as.numeric(cut2(train.source$Birthplace_Elsewhere_M, g=4))
train.source$Buddhism_F4 <- as.numeric(cut2(train.source$Buddhism_F, g=4))
train.source$Buddhism_M4 <- as.numeric(cut2(train.source$Buddhism_M, g=4))
train.source$Christianity_Tot_F4 <- as.numeric(cut2(train.source$Christianity_Tot_F, g=4))
train.source$Christianity_Tot_M4 <- as.numeric(cut2(train.source$Christianity_Tot_M, g=4))
train.source$Count_Census_Nt_Ewhere_Aust_F4 <- as.numeric(cut2(train.source$Count_Census_Nt_Ewhere_Aust_F, g=4))
train.source$Count_Census_Nt_Ewhere_Aust_M4 <- as.numeric(cut2(train.source$Count_Census_Nt_Ewhere_Aust_M, g=4))
train.source$Count_Persons_other_dwgs_F4 <- as.numeric(cut2(train.source$Count_Persons_other_dwgs_F, g=4))
train.source$Count_Persons_other_dwgs_M4 <- as.numeric(cut2(train.source$Count_Persons_other_dwgs_M, g=4))
train.source$Count_psns_occ_priv_dwgs_F4 <- as.numeric(cut2(train.source$Count_psns_occ_priv_dwgs_F, g=4))
train.source$Count_psns_occ_priv_dwgs_M4 <- as.numeric(cut2(train.source$Count_psns_occ_priv_dwgs_M, g=4))
train.source$Counted_Census_Night_home_F4 <- as.numeric(cut2(train.source$Counted_Census_Night_home_F, g=4))
train.source$Counted_Census_Night_home_M4 <- as.numeric(cut2(train.source$Counted_Census_Night_home_M, g=4))
train.source$Density4 <- as.numeric(cut2(train.source$Density, g=4))
train.source$Dist_Major_City4 <- as.numeric(cut2(train.source$Dist_Major_City, g=4))
train.source$F_BachDeg_Total4 <- as.numeric(cut2(train.source$F_BachDeg_Total, g=4))
train.source$F_PGrad_Deg_Total4 <- as.numeric(cut2(train.source$F_PGrad_Deg_Total, g=4))
train.source$F_Tot_Divorced4 <- as.numeric(cut2(train.source$F_Tot_Divorced, g=4))
train.source$F_Tot_Married4 <- as.numeric(cut2(train.source$F_Tot_Married, g=4))
train.source$F_Tot_N_a_volunteer4 <- as.numeric(cut2(train.source$F_Tot_N_a_volunteer, g=4))
train.source$F_Tot_Never_Married4 <- as.numeric(cut2(train.source$F_Tot_Never_Married, g=4))
train.source$F_Tot_Separated4 <- as.numeric(cut2(train.source$F_Tot_Separated, g=4))
train.source$F_Tot_Volunteer4 <- as.numeric(cut2(train.source$F_Tot_Volunteer, g=4))
train.source$F_Tot_Widowed4 <- as.numeric(cut2(train.source$F_Tot_Widowed, g=4))
train.source$Foreign4 <- as.numeric(cut2(train.source$Foreign, g=4))
train.source$High_yr_schl_comp_Yr_12_eq_F4 <- as.numeric(cut2(train.source$High_yr_schl_comp_Yr_12_eq_F, g=4))
train.source$High_yr_schl_comp_Yr_12_eq_M4 <- as.numeric(cut2(train.source$High_yr_schl_comp_Yr_12_eq_M, g=4))
train.source$Hinduism_F4 <- as.numeric(cut2(train.source$Hinduism_F, g=4))
train.source$Hinduism_M4 <- as.numeric(cut2(train.source$Hinduism_M, g=4))
train.source$Indig_Bth_Abor_Torres_St_Is_F4 <- as.numeric(cut2(train.source$Indig_Bth_Abor_Torres_St_Is_F, g=4))
train.source$Indig_Bth_Abor_Torres_St_Is_M4 <- as.numeric(cut2(train.source$Indig_Bth_Abor_Torres_St_Is_M, g=4))
train.source$Indig_psns_Torres_Strait_Is_F4 <- as.numeric(cut2(train.source$Indig_psns_Torres_Strait_Is_F, g=4))
train.source$Indig_psns_Torres_Strait_Is_M4 <- as.numeric(cut2(train.source$Indig_psns_Torres_Strait_Is_M, g=4))
train.source$Indigenous_P_Tot_F4 <- as.numeric(cut2(train.source$Indigenous_P_Tot_F, g=4))
train.source$Indigenous_P_Tot_M4 <- as.numeric(cut2(train.source$Indigenous_P_Tot_M, g=4))
train.source$Indigenous_psns_Aboriginal_F4 <- as.numeric(cut2(train.source$Indigenous_psns_Aboriginal_F, g=4))
train.source$Indigenous_psns_Aboriginal_M4 <- as.numeric(cut2(train.source$Indigenous_psns_Aboriginal_M, g=4))
train.source$Infant_Mort4 <- as.numeric(cut2(train.source$Infant_Mort, g=4))
train.source$Islam_F4 <- as.numeric(cut2(train.source$Islam_F, g=4))
train.source$Islam_M4 <- as.numeric(cut2(train.source$Islam_M, g=4))
train.source$Judaism_F4 <- as.numeric(cut2(train.source$Judaism_F, g=4))
train.source$Judaism_M4 <- as.numeric(cut2(train.source$Judaism_M, g=4))
train.source$Lang_spoken_home_Eng_only_F4 <- as.numeric(cut2(train.source$Lang_spoken_home_Eng_only_F, g=4))
train.source$Lang_spoken_home_Eng_only_M4 <- as.numeric(cut2(train.source$Lang_spoken_home_Eng_only_M, g=4))
train.source$Lang_spoken_home_Oth_Lang_F4 <- as.numeric(cut2(train.source$Lang_spoken_home_Oth_Lang_F, g=4))
train.source$Lang_spoken_home_Oth_Lang_M4 <- as.numeric(cut2(train.source$Lang_spoken_home_Oth_Lang_M, g=4))
train.source$Lat4 <- as.numeric(cut2(train.source$Lat, g=4))
train.source$lfs_Tot_LF_F4 <- as.numeric(cut2(train.source$lfs_Tot_LF_F, g=4))
train.source$lfs_Tot_LF_M4 <- as.numeric(cut2(train.source$lfs_Tot_LF_M, g=4))
train.source$lfs_Unmplyed_lookng_for_wrk_F4 <- as.numeric(cut2(train.source$lfs_Unmplyed_lookng_for_wrk_F, g=4))
train.source$lfs_Unmplyed_lookng_for_wrk_M4 <- as.numeric(cut2(train.source$lfs_Unmplyed_lookng_for_wrk_M, g=4))
train.source$Long4 <- as.numeric(cut2(train.source$Long, g=4))
train.source$M_BachDeg_Total4 <- as.numeric(cut2(train.source$M_BachDeg_Total, g=4))
train.source$M_PGrad_Deg_Total4 <- as.numeric(cut2(train.source$M_PGrad_Deg_Total, g=4))
train.source$M_Tot_Divorced4 <- as.numeric(cut2(train.source$M_Tot_Divorced, g=4))
train.source$M_Tot_Married4 <- as.numeric(cut2(train.source$M_Tot_Married, g=4))
train.source$M_Tot_N_a_volunteer4 <- as.numeric(cut2(train.source$M_Tot_N_a_volunteer, g=4))
train.source$M_Tot_Never_Married4 <- as.numeric(cut2(train.source$M_Tot_Never_Married, g=4))
train.source$M_Tot_Separated4 <- as.numeric(cut2(train.source$M_Tot_Separated, g=4))
train.source$M_Tot_Volunteer4 <- as.numeric(cut2(train.source$M_Tot_Volunteer, g=4))
train.source$M_Tot_Widowed4 <- as.numeric(cut2(train.source$M_Tot_Widowed, g=4))
train.source$Male_Ratio4 <- as.numeric(cut2(train.source$Male_Ratio, g=4))
train.source$Marriage_Prob4 <- as.numeric(cut2(train.source$Marriage_Prob, g=4))
train.source$Median_mortgage_repay_monthly4 <- as.numeric(cut2(train.source$Median_mortgage_repay_monthly, g=4))
train.source$Median_Tot_fam_inc_weekly4 <- as.numeric(cut2(train.source$Median_Tot_fam_inc_weekly, g=4))
train.source$Median_Tot_hhd_inc_weekly4 <- as.numeric(cut2(train.source$Median_Tot_hhd_inc_weekly, g=4))
train.source$Median_Tot_prsnl_inc_weekly4 <- as.numeric(cut2(train.source$Median_Tot_prsnl_inc_weekly, g=4))
train.source$No_Religion_F4 <- as.numeric(cut2(train.source$No_Religion_F, g=4))
train.source$No_Religion_M4 <- as.numeric(cut2(train.source$No_Religion_M, g=4))
train.source$O_MTG_Total4 <- as.numeric(cut2(train.source$O_MTG_Total, g=4))
train.source$O_OR_Total4 <- as.numeric(cut2(train.source$O_OR_Total, g=4))
train.source$Old_Age_F4 <- as.numeric(cut2(train.source$Old_Age_F, g=4))
train.source$Old_Age_M4 <- as.numeric(cut2(train.source$Old_Age_M, g=4))
train.source$Other_Religions_Tot_F4 <- as.numeric(cut2(train.source$Other_Religions_Tot_F, g=4))
train.source$Other_Religions_Tot_M4 <- as.numeric(cut2(train.source$Other_Religions_Tot_M, g=4))
train.source$R_Tot_Total4 <- as.numeric(cut2(train.source$R_Tot_Total, g=4))
train.source$SEO_Females4 <- as.numeric(cut2(train.source$SEO_Females, g=4))
train.source$SEO_Males4 <- as.numeric(cut2(train.source$SEO_Males, g=4))
train.source$Stay_at_home4 <- as.numeric(cut2(train.source$Stay_at_home, g=4))
train.source$Tot_Indig_status_ns_F4 <- as.numeric(cut2(train.source$Tot_Indig_status_ns_F, g=4))
train.source$Tot_Indig_status_ns_M4 <- as.numeric(cut2(train.source$Tot_Indig_status_ns_M, g=4))
train.source$Tot_Indigenous_F4 <- as.numeric(cut2(train.source$Tot_Indigenous_F, g=4))
train.source$Tot_Indigenous_M4 <- as.numeric(cut2(train.source$Tot_Indigenous_M, g=4))
train.source$Tot_Non_Indigenous_F4 <- as.numeric(cut2(train.source$Tot_Non_Indigenous_F, g=4))
train.source$Tot_Non_Indigenous_M4 <- as.numeric(cut2(train.source$Tot_Non_Indigenous_M, g=4))
train.source$Total_PDs_Dwellings4 <- as.numeric(cut2(train.source$Total_PDs_Dwellings, g=4))
train.source$Total_PDs_Persons4 <- as.numeric(cut2(train.source$Total_PDs_Persons, g=4))
train.source$Total_Total4 <- as.numeric(cut2(train.source$Total_Total, g=4))
train.source$Unemp4 <- as.numeric(cut2(train.source$Unemp, g=4))

test.source$Aboriginal4 <- as.numeric(cut2(test.source$Aboriginal, g=4))
test.source$Accidental_Mort4 <- as.numeric(cut2(test.source$Accidental_Mort, g=4))
test.source$Age_0_4_yr_F4 <- as.numeric(cut2(test.source$Age_0_4_yr_F, g=4))
test.source$Age_0_4_yr_M4 <- as.numeric(cut2(test.source$Age_0_4_yr_M, g=4))
test.source$Age_15_19_yr_F4 <- as.numeric(cut2(test.source$Age_15_19_yr_F, g=4))
test.source$Age_15_19_yr_M4 <- as.numeric(cut2(test.source$Age_15_19_yr_M, g=4))
test.source$Age_20_24_yr_F4 <- as.numeric(cut2(test.source$Age_20_24_yr_F, g=4))
test.source$Age_20_24_yr_M4 <- as.numeric(cut2(test.source$Age_20_24_yr_M, g=4))
test.source$Age_25_34_yr_F4 <- as.numeric(cut2(test.source$Age_25_34_yr_F, g=4))
test.source$Age_25_34_yr_M4 <- as.numeric(cut2(test.source$Age_25_34_yr_M, g=4))
test.source$Age_35_44_yr_F4 <- as.numeric(cut2(test.source$Age_35_44_yr_F, g=4))
test.source$Age_35_44_yr_M4 <- as.numeric(cut2(test.source$Age_35_44_yr_M, g=4))
test.source$Age_45_54_yr_F4 <- as.numeric(cut2(test.source$Age_45_54_yr_F, g=4))
test.source$Age_45_54_yr_M4 <- as.numeric(cut2(test.source$Age_45_54_yr_M, g=4))
test.source$Age_5_14_yr_F4 <- as.numeric(cut2(test.source$Age_5_14_yr_F, g=4))
test.source$Age_5_14_yr_M4 <- as.numeric(cut2(test.source$Age_5_14_yr_M, g=4))
test.source$Age_55_64_yr_F4 <- as.numeric(cut2(test.source$Age_55_64_yr_F, g=4))
test.source$Age_55_64_yr_M4 <- as.numeric(cut2(test.source$Age_55_64_yr_M, g=4))
test.source$Age_65_74_yr_F4 <- as.numeric(cut2(test.source$Age_65_74_yr_F, g=4))
test.source$Age_65_74_yr_M4 <- as.numeric(cut2(test.source$Age_65_74_yr_M, g=4))
test.source$Age_75_84_yr_F4 <- as.numeric(cut2(test.source$Age_75_84_yr_F, g=4))
test.source$Age_75_84_yr_M4 <- as.numeric(cut2(test.source$Age_75_84_yr_M, g=4))
test.source$Age_85ov_F4 <- as.numeric(cut2(test.source$Age_85ov_F, g=4))
test.source$Age_85ov_M4 <- as.numeric(cut2(test.source$Age_85ov_M, g=4))
test.source$AREA_SQKM4 <- as.numeric(cut2(test.source$AREA_SQKM, g=4))
test.source$Australian_citizen_F4 <- as.numeric(cut2(test.source$Australian_citizen_F, g=4))
test.source$Australian_citizen_M4 <- as.numeric(cut2(test.source$Australian_citizen_M, g=4))
test.source$Average_household_size4 <- as.numeric(cut2(test.source$Average_household_size, g=4))
test.source$Average_num_psns_per_bedroom4 <- as.numeric(cut2(test.source$Average_num_psns_per_bedroom, g=4))
test.source$Birthplace_Australia_F4 <- as.numeric(cut2(test.source$Birthplace_Australia_F, g=4))
test.source$Birthplace_Australia_M4 <- as.numeric(cut2(test.source$Birthplace_Australia_M, g=4))
test.source$Birthplace_Elsewhere_F4 <- as.numeric(cut2(test.source$Birthplace_Elsewhere_F, g=4))
test.source$Birthplace_Elsewhere_M4 <- as.numeric(cut2(test.source$Birthplace_Elsewhere_M, g=4))
test.source$Buddhism_F4 <- as.numeric(cut2(test.source$Buddhism_F, g=4))
test.source$Buddhism_M4 <- as.numeric(cut2(test.source$Buddhism_M, g=4))
test.source$Christianity_Tot_F4 <- as.numeric(cut2(test.source$Christianity_Tot_F, g=4))
test.source$Christianity_Tot_M4 <- as.numeric(cut2(test.source$Christianity_Tot_M, g=4))
test.source$Count_Census_Nt_Ewhere_Aust_F4 <- as.numeric(cut2(test.source$Count_Census_Nt_Ewhere_Aust_F, g=4))
test.source$Count_Census_Nt_Ewhere_Aust_M4 <- as.numeric(cut2(test.source$Count_Census_Nt_Ewhere_Aust_M, g=4))
test.source$Count_Persons_other_dwgs_F4 <- as.numeric(cut2(test.source$Count_Persons_other_dwgs_F, g=4))
test.source$Count_Persons_other_dwgs_M4 <- as.numeric(cut2(test.source$Count_Persons_other_dwgs_M, g=4))
test.source$Count_psns_occ_priv_dwgs_F4 <- as.numeric(cut2(test.source$Count_psns_occ_priv_dwgs_F, g=4))
test.source$Count_psns_occ_priv_dwgs_M4 <- as.numeric(cut2(test.source$Count_psns_occ_priv_dwgs_M, g=4))
test.source$Counted_Census_Night_home_F4 <- as.numeric(cut2(test.source$Counted_Census_Night_home_F, g=4))
test.source$Counted_Census_Night_home_M4 <- as.numeric(cut2(test.source$Counted_Census_Night_home_M, g=4))
test.source$Density4 <- as.numeric(cut2(test.source$Density, g=4))
test.source$Dist_Major_City4 <- as.numeric(cut2(test.source$Dist_Major_City, g=4))
test.source$F_BachDeg_Total4 <- as.numeric(cut2(test.source$F_BachDeg_Total, g=4))
test.source$F_PGrad_Deg_Total4 <- as.numeric(cut2(test.source$F_PGrad_Deg_Total, g=4))
test.source$F_Tot_Divorced4 <- as.numeric(cut2(test.source$F_Tot_Divorced, g=4))
test.source$F_Tot_Married4 <- as.numeric(cut2(test.source$F_Tot_Married, g=4))
test.source$F_Tot_N_a_volunteer4 <- as.numeric(cut2(test.source$F_Tot_N_a_volunteer, g=4))
test.source$F_Tot_Never_Married4 <- as.numeric(cut2(test.source$F_Tot_Never_Married, g=4))
test.source$F_Tot_Separated4 <- as.numeric(cut2(test.source$F_Tot_Separated, g=4))
test.source$F_Tot_Volunteer4 <- as.numeric(cut2(test.source$F_Tot_Volunteer, g=4))
test.source$F_Tot_Widowed4 <- as.numeric(cut2(test.source$F_Tot_Widowed, g=4))
test.source$Foreign4 <- as.numeric(cut2(test.source$Foreign, g=4))
test.source$High_yr_schl_comp_Yr_12_eq_F4 <- as.numeric(cut2(test.source$High_yr_schl_comp_Yr_12_eq_F, g=4))
test.source$High_yr_schl_comp_Yr_12_eq_M4 <- as.numeric(cut2(test.source$High_yr_schl_comp_Yr_12_eq_M, g=4))
test.source$Hinduism_F4 <- as.numeric(cut2(test.source$Hinduism_F, g=4))
test.source$Hinduism_M4 <- as.numeric(cut2(test.source$Hinduism_M, g=4))
test.source$Indig_Bth_Abor_Torres_St_Is_F4 <- as.numeric(cut2(test.source$Indig_Bth_Abor_Torres_St_Is_F, g=4))
test.source$Indig_Bth_Abor_Torres_St_Is_M4 <- as.numeric(cut2(test.source$Indig_Bth_Abor_Torres_St_Is_M, g=4))
test.source$Indig_psns_Torres_Strait_Is_F4 <- as.numeric(cut2(test.source$Indig_psns_Torres_Strait_Is_F, g=4))
test.source$Indig_psns_Torres_Strait_Is_M4 <- as.numeric(cut2(test.source$Indig_psns_Torres_Strait_Is_M, g=4))
test.source$Indigenous_P_Tot_F4 <- as.numeric(cut2(test.source$Indigenous_P_Tot_F, g=4))
test.source$Indigenous_P_Tot_M4 <- as.numeric(cut2(test.source$Indigenous_P_Tot_M, g=4))
test.source$Indigenous_psns_Aboriginal_F4 <- as.numeric(cut2(test.source$Indigenous_psns_Aboriginal_F, g=4))
test.source$Indigenous_psns_Aboriginal_M4 <- as.numeric(cut2(test.source$Indigenous_psns_Aboriginal_M, g=4))
test.source$Infant_Mort4 <- as.numeric(cut2(test.source$Infant_Mort, g=4))
test.source$Islam_F4 <- as.numeric(cut2(test.source$Islam_F, g=4))
test.source$Islam_M4 <- as.numeric(cut2(test.source$Islam_M, g=4))
test.source$Judaism_F4 <- as.numeric(cut2(test.source$Judaism_F, g=4))
test.source$Judaism_M4 <- as.numeric(cut2(test.source$Judaism_M, g=4))
test.source$Lang_spoken_home_Eng_only_F4 <- as.numeric(cut2(test.source$Lang_spoken_home_Eng_only_F, g=4))
test.source$Lang_spoken_home_Eng_only_M4 <- as.numeric(cut2(test.source$Lang_spoken_home_Eng_only_M, g=4))
test.source$Lang_spoken_home_Oth_Lang_F4 <- as.numeric(cut2(test.source$Lang_spoken_home_Oth_Lang_F, g=4))
test.source$Lang_spoken_home_Oth_Lang_M4 <- as.numeric(cut2(test.source$Lang_spoken_home_Oth_Lang_M, g=4))
test.source$Lat4 <- as.numeric(cut2(test.source$Lat, g=4))
test.source$lfs_Tot_LF_F4 <- as.numeric(cut2(test.source$lfs_Tot_LF_F, g=4))
test.source$lfs_Tot_LF_M4 <- as.numeric(cut2(test.source$lfs_Tot_LF_M, g=4))
test.source$lfs_Unmplyed_lookng_for_wrk_F4 <- as.numeric(cut2(test.source$lfs_Unmplyed_lookng_for_wrk_F, g=4))
test.source$lfs_Unmplyed_lookng_for_wrk_M4 <- as.numeric(cut2(test.source$lfs_Unmplyed_lookng_for_wrk_M, g=4))
test.source$Long4 <- as.numeric(cut2(test.source$Long, g=4))
test.source$M_BachDeg_Total4 <- as.numeric(cut2(test.source$M_BachDeg_Total, g=4))
test.source$M_PGrad_Deg_Total4 <- as.numeric(cut2(test.source$M_PGrad_Deg_Total, g=4))
test.source$M_Tot_Divorced4 <- as.numeric(cut2(test.source$M_Tot_Divorced, g=4))
test.source$M_Tot_Married4 <- as.numeric(cut2(test.source$M_Tot_Married, g=4))
test.source$M_Tot_N_a_volunteer4 <- as.numeric(cut2(test.source$M_Tot_N_a_volunteer, g=4))
test.source$M_Tot_Never_Married4 <- as.numeric(cut2(test.source$M_Tot_Never_Married, g=4))
test.source$M_Tot_Separated4 <- as.numeric(cut2(test.source$M_Tot_Separated, g=4))
test.source$M_Tot_Volunteer4 <- as.numeric(cut2(test.source$M_Tot_Volunteer, g=4))
test.source$M_Tot_Widowed4 <- as.numeric(cut2(test.source$M_Tot_Widowed, g=4))
test.source$Male_Ratio4 <- as.numeric(cut2(test.source$Male_Ratio, g=4))
test.source$Marriage_Prob4 <- as.numeric(cut2(test.source$Marriage_Prob, g=4))
test.source$Median_mortgage_repay_monthly4 <- as.numeric(cut2(test.source$Median_mortgage_repay_monthly, g=4))
test.source$Median_Tot_fam_inc_weekly4 <- as.numeric(cut2(test.source$Median_Tot_fam_inc_weekly, g=4))
test.source$Median_Tot_hhd_inc_weekly4 <- as.numeric(cut2(test.source$Median_Tot_hhd_inc_weekly, g=4))
test.source$Median_Tot_prsnl_inc_weekly4 <- as.numeric(cut2(test.source$Median_Tot_prsnl_inc_weekly, g=4))
test.source$No_Religion_F4 <- as.numeric(cut2(test.source$No_Religion_F, g=4))
test.source$No_Religion_M4 <- as.numeric(cut2(test.source$No_Religion_M, g=4))
test.source$O_MTG_Total4 <- as.numeric(cut2(test.source$O_MTG_Total, g=4))
test.source$O_OR_Total4 <- as.numeric(cut2(test.source$O_OR_Total, g=4))
test.source$Old_Age_F4 <- as.numeric(cut2(test.source$Old_Age_F, g=4))
test.source$Old_Age_M4 <- as.numeric(cut2(test.source$Old_Age_M, g=4))
test.source$Other_Religions_Tot_F4 <- as.numeric(cut2(test.source$Other_Religions_Tot_F, g=4))
test.source$Other_Religions_Tot_M4 <- as.numeric(cut2(test.source$Other_Religions_Tot_M, g=4))
test.source$R_Tot_Total4 <- as.numeric(cut2(test.source$R_Tot_Total, g=4))
test.source$SEO_Females4 <- as.numeric(cut2(test.source$SEO_Females, g=4))
test.source$SEO_Males4 <- as.numeric(cut2(test.source$SEO_Males, g=4))
test.source$Stay_at_home4 <- as.numeric(cut2(test.source$Stay_at_home, g=4))
test.source$Tot_Indig_status_ns_F4 <- as.numeric(cut2(test.source$Tot_Indig_status_ns_F, g=4))
test.source$Tot_Indig_status_ns_M4 <- as.numeric(cut2(test.source$Tot_Indig_status_ns_M, g=4))
test.source$Tot_Indigenous_F4 <- as.numeric(cut2(test.source$Tot_Indigenous_F, g=4))
test.source$Tot_Indigenous_M4 <- as.numeric(cut2(test.source$Tot_Indigenous_M, g=4))
test.source$Tot_Non_Indigenous_F4 <- as.numeric(cut2(test.source$Tot_Non_Indigenous_F, g=4))
test.source$Tot_Non_Indigenous_M4 <- as.numeric(cut2(test.source$Tot_Non_Indigenous_M, g=4))
test.source$Total_PDs_Dwellings4 <- as.numeric(cut2(test.source$Total_PDs_Dwellings, g=4))
test.source$Total_PDs_Persons4 <- as.numeric(cut2(test.source$Total_PDs_Persons, g=4))
test.source$Total_Total4 <- as.numeric(cut2(test.source$Total_Total, g=4))
test.source$Unemp4 <- as.numeric(cut2(test.source$Unemp, g=4))

#cut data 5 ways
train.source$Aboriginal5 <- as.numeric(cut2(train.source$Aboriginal, g=5))
train.source$Accidental_Mort5 <- as.numeric(cut2(train.source$Accidental_Mort, g=5))
train.source$Age_0_4_yr_F5 <- as.numeric(cut2(train.source$Age_0_4_yr_F, g=5))
train.source$Age_0_4_yr_M5 <- as.numeric(cut2(train.source$Age_0_4_yr_M, g=5))
train.source$Age_15_19_yr_F5 <- as.numeric(cut2(train.source$Age_15_19_yr_F, g=5))
train.source$Age_15_19_yr_M5 <- as.numeric(cut2(train.source$Age_15_19_yr_M, g=5))
train.source$Age_20_24_yr_F5 <- as.numeric(cut2(train.source$Age_20_24_yr_F, g=5))
train.source$Age_20_24_yr_M5 <- as.numeric(cut2(train.source$Age_20_24_yr_M, g=5))
train.source$Age_25_34_yr_F5 <- as.numeric(cut2(train.source$Age_25_34_yr_F, g=5))
train.source$Age_25_34_yr_M5 <- as.numeric(cut2(train.source$Age_25_34_yr_M, g=5))
train.source$Age_35_44_yr_F5 <- as.numeric(cut2(train.source$Age_35_44_yr_F, g=5))
train.source$Age_35_44_yr_M5 <- as.numeric(cut2(train.source$Age_35_44_yr_M, g=5))
train.source$Age_45_54_yr_F5 <- as.numeric(cut2(train.source$Age_45_54_yr_F, g=5))
train.source$Age_45_54_yr_M5 <- as.numeric(cut2(train.source$Age_45_54_yr_M, g=5))
train.source$Age_5_14_yr_F5 <- as.numeric(cut2(train.source$Age_5_14_yr_F, g=5))
train.source$Age_5_14_yr_M5 <- as.numeric(cut2(train.source$Age_5_14_yr_M, g=5))
train.source$Age_55_64_yr_F5 <- as.numeric(cut2(train.source$Age_55_64_yr_F, g=5))
train.source$Age_55_64_yr_M5 <- as.numeric(cut2(train.source$Age_55_64_yr_M, g=5))
train.source$Age_65_74_yr_F5 <- as.numeric(cut2(train.source$Age_65_74_yr_F, g=5))
train.source$Age_65_74_yr_M5 <- as.numeric(cut2(train.source$Age_65_74_yr_M, g=5))
train.source$Age_75_84_yr_F5 <- as.numeric(cut2(train.source$Age_75_84_yr_F, g=5))
train.source$Age_75_84_yr_M5 <- as.numeric(cut2(train.source$Age_75_84_yr_M, g=5))
train.source$Age_85ov_F5 <- as.numeric(cut2(train.source$Age_85ov_F, g=5))
train.source$Age_85ov_M5 <- as.numeric(cut2(train.source$Age_85ov_M, g=5))
train.source$AREA_SQKM5 <- as.numeric(cut2(train.source$AREA_SQKM, g=5))
train.source$Australian_citizen_F5 <- as.numeric(cut2(train.source$Australian_citizen_F, g=5))
train.source$Australian_citizen_M5 <- as.numeric(cut2(train.source$Australian_citizen_M, g=5))
train.source$Average_household_size5 <- as.numeric(cut2(train.source$Average_household_size, g=5))
train.source$Average_num_psns_per_bedroom5 <- as.numeric(cut2(train.source$Average_num_psns_per_bedroom, g=5))
train.source$Birthplace_Australia_F5 <- as.numeric(cut2(train.source$Birthplace_Australia_F, g=5))
train.source$Birthplace_Australia_M5 <- as.numeric(cut2(train.source$Birthplace_Australia_M, g=5))
train.source$Birthplace_Elsewhere_F5 <- as.numeric(cut2(train.source$Birthplace_Elsewhere_F, g=5))
train.source$Birthplace_Elsewhere_M5 <- as.numeric(cut2(train.source$Birthplace_Elsewhere_M, g=5))
train.source$Buddhism_F5 <- as.numeric(cut2(train.source$Buddhism_F, g=5))
train.source$Buddhism_M5 <- as.numeric(cut2(train.source$Buddhism_M, g=5))
train.source$Christianity_Tot_F5 <- as.numeric(cut2(train.source$Christianity_Tot_F, g=5))
train.source$Christianity_Tot_M5 <- as.numeric(cut2(train.source$Christianity_Tot_M, g=5))
train.source$Count_Census_Nt_Ewhere_Aust_F5 <- as.numeric(cut2(train.source$Count_Census_Nt_Ewhere_Aust_F, g=5))
train.source$Count_Census_Nt_Ewhere_Aust_M5 <- as.numeric(cut2(train.source$Count_Census_Nt_Ewhere_Aust_M, g=5))
train.source$Count_Persons_other_dwgs_F5 <- as.numeric(cut2(train.source$Count_Persons_other_dwgs_F, g=5))
train.source$Count_Persons_other_dwgs_M5 <- as.numeric(cut2(train.source$Count_Persons_other_dwgs_M, g=5))
train.source$Count_psns_occ_priv_dwgs_F5 <- as.numeric(cut2(train.source$Count_psns_occ_priv_dwgs_F, g=5))
train.source$Count_psns_occ_priv_dwgs_M5 <- as.numeric(cut2(train.source$Count_psns_occ_priv_dwgs_M, g=5))
train.source$Counted_Census_Night_home_F5 <- as.numeric(cut2(train.source$Counted_Census_Night_home_F, g=5))
train.source$Counted_Census_Night_home_M5 <- as.numeric(cut2(train.source$Counted_Census_Night_home_M, g=5))
train.source$Density5 <- as.numeric(cut2(train.source$Density, g=5))
train.source$Dist_Major_City5 <- as.numeric(cut2(train.source$Dist_Major_City, g=5))
train.source$F_BachDeg_Total5 <- as.numeric(cut2(train.source$F_BachDeg_Total, g=5))
train.source$F_PGrad_Deg_Total5 <- as.numeric(cut2(train.source$F_PGrad_Deg_Total, g=5))
train.source$F_Tot_Divorced5 <- as.numeric(cut2(train.source$F_Tot_Divorced, g=5))
train.source$F_Tot_Married5 <- as.numeric(cut2(train.source$F_Tot_Married, g=5))
train.source$F_Tot_N_a_volunteer5 <- as.numeric(cut2(train.source$F_Tot_N_a_volunteer, g=5))
train.source$F_Tot_Never_Married5 <- as.numeric(cut2(train.source$F_Tot_Never_Married, g=5))
train.source$F_Tot_Separated5 <- as.numeric(cut2(train.source$F_Tot_Separated, g=5))
train.source$F_Tot_Volunteer5 <- as.numeric(cut2(train.source$F_Tot_Volunteer, g=5))
train.source$F_Tot_Widowed5 <- as.numeric(cut2(train.source$F_Tot_Widowed, g=5))
train.source$Foreign5 <- as.numeric(cut2(train.source$Foreign, g=5))
train.source$High_yr_schl_comp_Yr_12_eq_F5 <- as.numeric(cut2(train.source$High_yr_schl_comp_Yr_12_eq_F, g=5))
train.source$High_yr_schl_comp_Yr_12_eq_M5 <- as.numeric(cut2(train.source$High_yr_schl_comp_Yr_12_eq_M, g=5))
train.source$Hinduism_F5 <- as.numeric(cut2(train.source$Hinduism_F, g=5))
train.source$Hinduism_M5 <- as.numeric(cut2(train.source$Hinduism_M, g=5))
train.source$Indig_Bth_Abor_Torres_St_Is_F5 <- as.numeric(cut2(train.source$Indig_Bth_Abor_Torres_St_Is_F, g=5))
train.source$Indig_Bth_Abor_Torres_St_Is_M5 <- as.numeric(cut2(train.source$Indig_Bth_Abor_Torres_St_Is_M, g=5))
train.source$Indig_psns_Torres_Strait_Is_F5 <- as.numeric(cut2(train.source$Indig_psns_Torres_Strait_Is_F, g=5))
train.source$Indig_psns_Torres_Strait_Is_M5 <- as.numeric(cut2(train.source$Indig_psns_Torres_Strait_Is_M, g=5))
train.source$Indigenous_P_Tot_F5 <- as.numeric(cut2(train.source$Indigenous_P_Tot_F, g=5))
train.source$Indigenous_P_Tot_M5 <- as.numeric(cut2(train.source$Indigenous_P_Tot_M, g=5))
train.source$Indigenous_psns_Aboriginal_F5 <- as.numeric(cut2(train.source$Indigenous_psns_Aboriginal_F, g=5))
train.source$Indigenous_psns_Aboriginal_M5 <- as.numeric(cut2(train.source$Indigenous_psns_Aboriginal_M, g=5))
train.source$Infant_Mort5 <- as.numeric(cut2(train.source$Infant_Mort, g=5))
train.source$Islam_F5 <- as.numeric(cut2(train.source$Islam_F, g=5))
train.source$Islam_M5 <- as.numeric(cut2(train.source$Islam_M, g=5))
train.source$Judaism_F5 <- as.numeric(cut2(train.source$Judaism_F, g=5))
train.source$Judaism_M5 <- as.numeric(cut2(train.source$Judaism_M, g=5))
train.source$Lang_spoken_home_Eng_only_F5 <- as.numeric(cut2(train.source$Lang_spoken_home_Eng_only_F, g=5))
train.source$Lang_spoken_home_Eng_only_M5 <- as.numeric(cut2(train.source$Lang_spoken_home_Eng_only_M, g=5))
train.source$Lang_spoken_home_Oth_Lang_F5 <- as.numeric(cut2(train.source$Lang_spoken_home_Oth_Lang_F, g=5))
train.source$Lang_spoken_home_Oth_Lang_M5 <- as.numeric(cut2(train.source$Lang_spoken_home_Oth_Lang_M, g=5))
train.source$Lat5 <- as.numeric(cut2(train.source$Lat, g=5))
train.source$lfs_Tot_LF_F5 <- as.numeric(cut2(train.source$lfs_Tot_LF_F, g=5))
train.source$lfs_Tot_LF_M5 <- as.numeric(cut2(train.source$lfs_Tot_LF_M, g=5))
train.source$lfs_Unmplyed_lookng_for_wrk_F5 <- as.numeric(cut2(train.source$lfs_Unmplyed_lookng_for_wrk_F, g=5))
train.source$lfs_Unmplyed_lookng_for_wrk_M5 <- as.numeric(cut2(train.source$lfs_Unmplyed_lookng_for_wrk_M, g=5))
train.source$Long5 <- as.numeric(cut2(train.source$Long, g=5))
train.source$M_BachDeg_Total5 <- as.numeric(cut2(train.source$M_BachDeg_Total, g=5))
train.source$M_PGrad_Deg_Total5 <- as.numeric(cut2(train.source$M_PGrad_Deg_Total, g=5))
train.source$M_Tot_Divorced5 <- as.numeric(cut2(train.source$M_Tot_Divorced, g=5))
train.source$M_Tot_Married5 <- as.numeric(cut2(train.source$M_Tot_Married, g=5))
train.source$M_Tot_N_a_volunteer5 <- as.numeric(cut2(train.source$M_Tot_N_a_volunteer, g=5))
train.source$M_Tot_Never_Married5 <- as.numeric(cut2(train.source$M_Tot_Never_Married, g=5))
train.source$M_Tot_Separated5 <- as.numeric(cut2(train.source$M_Tot_Separated, g=5))
train.source$M_Tot_Volunteer5 <- as.numeric(cut2(train.source$M_Tot_Volunteer, g=5))
train.source$M_Tot_Widowed5 <- as.numeric(cut2(train.source$M_Tot_Widowed, g=5))
train.source$Male_Ratio5 <- as.numeric(cut2(train.source$Male_Ratio, g=5))
train.source$Marriage_Prob5 <- as.numeric(cut2(train.source$Marriage_Prob, g=5))
train.source$Median_mortgage_repay_monthly5 <- as.numeric(cut2(train.source$Median_mortgage_repay_monthly, g=5))
train.source$Median_Tot_fam_inc_weekly5 <- as.numeric(cut2(train.source$Median_Tot_fam_inc_weekly, g=5))
train.source$Median_Tot_hhd_inc_weekly5 <- as.numeric(cut2(train.source$Median_Tot_hhd_inc_weekly, g=5))
train.source$Median_Tot_prsnl_inc_weekly5 <- as.numeric(cut2(train.source$Median_Tot_prsnl_inc_weekly, g=5))
train.source$No_Religion_F5 <- as.numeric(cut2(train.source$No_Religion_F, g=5))
train.source$No_Religion_M5 <- as.numeric(cut2(train.source$No_Religion_M, g=5))
train.source$O_MTG_Total5 <- as.numeric(cut2(train.source$O_MTG_Total, g=5))
train.source$O_OR_Total5 <- as.numeric(cut2(train.source$O_OR_Total, g=5))
train.source$Old_Age_F5 <- as.numeric(cut2(train.source$Old_Age_F, g=5))
train.source$Old_Age_M5 <- as.numeric(cut2(train.source$Old_Age_M, g=5))
train.source$Other_Religions_Tot_F5 <- as.numeric(cut2(train.source$Other_Religions_Tot_F, g=5))
train.source$Other_Religions_Tot_M5 <- as.numeric(cut2(train.source$Other_Religions_Tot_M, g=5))
train.source$R_Tot_Total5 <- as.numeric(cut2(train.source$R_Tot_Total, g=5))
train.source$SEO_Females5 <- as.numeric(cut2(train.source$SEO_Females, g=5))
train.source$SEO_Males5 <- as.numeric(cut2(train.source$SEO_Males, g=5))
train.source$Stay_at_home5 <- as.numeric(cut2(train.source$Stay_at_home, g=5))
train.source$Tot_Indig_status_ns_F5 <- as.numeric(cut2(train.source$Tot_Indig_status_ns_F, g=5))
train.source$Tot_Indig_status_ns_M5 <- as.numeric(cut2(train.source$Tot_Indig_status_ns_M, g=5))
train.source$Tot_Indigenous_F5 <- as.numeric(cut2(train.source$Tot_Indigenous_F, g=5))
train.source$Tot_Indigenous_M5 <- as.numeric(cut2(train.source$Tot_Indigenous_M, g=5))
train.source$Tot_Non_Indigenous_F5 <- as.numeric(cut2(train.source$Tot_Non_Indigenous_F, g=5))
train.source$Tot_Non_Indigenous_M5 <- as.numeric(cut2(train.source$Tot_Non_Indigenous_M, g=5))
train.source$Total_PDs_Dwellings5 <- as.numeric(cut2(train.source$Total_PDs_Dwellings, g=5))
train.source$Total_PDs_Persons5 <- as.numeric(cut2(train.source$Total_PDs_Persons, g=5))
train.source$Total_Total5 <- as.numeric(cut2(train.source$Total_Total, g=5))
train.source$Unemp5 <- as.numeric(cut2(train.source$Unemp, g=5))

test.source$Aboriginal5 <- as.numeric(cut2(test.source$Aboriginal, g=5))
test.source$Accidental_Mort5 <- as.numeric(cut2(test.source$Accidental_Mort, g=5))
test.source$Age_0_4_yr_F5 <- as.numeric(cut2(test.source$Age_0_4_yr_F, g=5))
test.source$Age_0_4_yr_M5 <- as.numeric(cut2(test.source$Age_0_4_yr_M, g=5))
test.source$Age_15_19_yr_F5 <- as.numeric(cut2(test.source$Age_15_19_yr_F, g=5))
test.source$Age_15_19_yr_M5 <- as.numeric(cut2(test.source$Age_15_19_yr_M, g=5))
test.source$Age_20_24_yr_F5 <- as.numeric(cut2(test.source$Age_20_24_yr_F, g=5))
test.source$Age_20_24_yr_M5 <- as.numeric(cut2(test.source$Age_20_24_yr_M, g=5))
test.source$Age_25_34_yr_F5 <- as.numeric(cut2(test.source$Age_25_34_yr_F, g=5))
test.source$Age_25_34_yr_M5 <- as.numeric(cut2(test.source$Age_25_34_yr_M, g=5))
test.source$Age_35_44_yr_F5 <- as.numeric(cut2(test.source$Age_35_44_yr_F, g=5))
test.source$Age_35_44_yr_M5 <- as.numeric(cut2(test.source$Age_35_44_yr_M, g=5))
test.source$Age_45_54_yr_F5 <- as.numeric(cut2(test.source$Age_45_54_yr_F, g=5))
test.source$Age_45_54_yr_M5 <- as.numeric(cut2(test.source$Age_45_54_yr_M, g=5))
test.source$Age_5_14_yr_F5 <- as.numeric(cut2(test.source$Age_5_14_yr_F, g=5))
test.source$Age_5_14_yr_M5 <- as.numeric(cut2(test.source$Age_5_14_yr_M, g=5))
test.source$Age_55_64_yr_F5 <- as.numeric(cut2(test.source$Age_55_64_yr_F, g=5))
test.source$Age_55_64_yr_M5 <- as.numeric(cut2(test.source$Age_55_64_yr_M, g=5))
test.source$Age_65_74_yr_F5 <- as.numeric(cut2(test.source$Age_65_74_yr_F, g=5))
test.source$Age_65_74_yr_M5 <- as.numeric(cut2(test.source$Age_65_74_yr_M, g=5))
test.source$Age_75_84_yr_F5 <- as.numeric(cut2(test.source$Age_75_84_yr_F, g=5))
test.source$Age_75_84_yr_M5 <- as.numeric(cut2(test.source$Age_75_84_yr_M, g=5))
test.source$Age_85ov_F5 <- as.numeric(cut2(test.source$Age_85ov_F, g=5))
test.source$Age_85ov_M5 <- as.numeric(cut2(test.source$Age_85ov_M, g=5))
test.source$AREA_SQKM5 <- as.numeric(cut2(test.source$AREA_SQKM, g=5))
test.source$Australian_citizen_F5 <- as.numeric(cut2(test.source$Australian_citizen_F, g=5))
test.source$Australian_citizen_M5 <- as.numeric(cut2(test.source$Australian_citizen_M, g=5))
test.source$Average_household_size5 <- as.numeric(cut2(test.source$Average_household_size, g=5))
test.source$Average_num_psns_per_bedroom5 <- as.numeric(cut2(test.source$Average_num_psns_per_bedroom, g=5))
test.source$Birthplace_Australia_F5 <- as.numeric(cut2(test.source$Birthplace_Australia_F, g=5))
test.source$Birthplace_Australia_M5 <- as.numeric(cut2(test.source$Birthplace_Australia_M, g=5))
test.source$Birthplace_Elsewhere_F5 <- as.numeric(cut2(test.source$Birthplace_Elsewhere_F, g=5))
test.source$Birthplace_Elsewhere_M5 <- as.numeric(cut2(test.source$Birthplace_Elsewhere_M, g=5))
test.source$Buddhism_F5 <- as.numeric(cut2(test.source$Buddhism_F, g=5))
test.source$Buddhism_M5 <- as.numeric(cut2(test.source$Buddhism_M, g=5))
test.source$Christianity_Tot_F5 <- as.numeric(cut2(test.source$Christianity_Tot_F, g=5))
test.source$Christianity_Tot_M5 <- as.numeric(cut2(test.source$Christianity_Tot_M, g=5))
test.source$Count_Census_Nt_Ewhere_Aust_F5 <- as.numeric(cut2(test.source$Count_Census_Nt_Ewhere_Aust_F, g=5))
test.source$Count_Census_Nt_Ewhere_Aust_M5 <- as.numeric(cut2(test.source$Count_Census_Nt_Ewhere_Aust_M, g=5))
test.source$Count_Persons_other_dwgs_F5 <- as.numeric(cut2(test.source$Count_Persons_other_dwgs_F, g=5))
test.source$Count_Persons_other_dwgs_M5 <- as.numeric(cut2(test.source$Count_Persons_other_dwgs_M, g=5))
test.source$Count_psns_occ_priv_dwgs_F5 <- as.numeric(cut2(test.source$Count_psns_occ_priv_dwgs_F, g=5))
test.source$Count_psns_occ_priv_dwgs_M5 <- as.numeric(cut2(test.source$Count_psns_occ_priv_dwgs_M, g=5))
test.source$Counted_Census_Night_home_F5 <- as.numeric(cut2(test.source$Counted_Census_Night_home_F, g=5))
test.source$Counted_Census_Night_home_M5 <- as.numeric(cut2(test.source$Counted_Census_Night_home_M, g=5))
test.source$Density5 <- as.numeric(cut2(test.source$Density, g=5))
test.source$Dist_Major_City5 <- as.numeric(cut2(test.source$Dist_Major_City, g=5))
test.source$F_BachDeg_Total5 <- as.numeric(cut2(test.source$F_BachDeg_Total, g=5))
test.source$F_PGrad_Deg_Total5 <- as.numeric(cut2(test.source$F_PGrad_Deg_Total, g=5))
test.source$F_Tot_Divorced5 <- as.numeric(cut2(test.source$F_Tot_Divorced, g=5))
test.source$F_Tot_Married5 <- as.numeric(cut2(test.source$F_Tot_Married, g=5))
test.source$F_Tot_N_a_volunteer5 <- as.numeric(cut2(test.source$F_Tot_N_a_volunteer, g=5))
test.source$F_Tot_Never_Married5 <- as.numeric(cut2(test.source$F_Tot_Never_Married, g=5))
test.source$F_Tot_Separated5 <- as.numeric(cut2(test.source$F_Tot_Separated, g=5))
test.source$F_Tot_Volunteer5 <- as.numeric(cut2(test.source$F_Tot_Volunteer, g=5))
test.source$F_Tot_Widowed5 <- as.numeric(cut2(test.source$F_Tot_Widowed, g=5))
test.source$Foreign5 <- as.numeric(cut2(test.source$Foreign, g=5))
test.source$High_yr_schl_comp_Yr_12_eq_F5 <- as.numeric(cut2(test.source$High_yr_schl_comp_Yr_12_eq_F, g=5))
test.source$High_yr_schl_comp_Yr_12_eq_M5 <- as.numeric(cut2(test.source$High_yr_schl_comp_Yr_12_eq_M, g=5))
test.source$Hinduism_F5 <- as.numeric(cut2(test.source$Hinduism_F, g=5))
test.source$Hinduism_M5 <- as.numeric(cut2(test.source$Hinduism_M, g=5))
test.source$Indig_Bth_Abor_Torres_St_Is_F5 <- as.numeric(cut2(test.source$Indig_Bth_Abor_Torres_St_Is_F, g=5))
test.source$Indig_Bth_Abor_Torres_St_Is_M5 <- as.numeric(cut2(test.source$Indig_Bth_Abor_Torres_St_Is_M, g=5))
test.source$Indig_psns_Torres_Strait_Is_F5 <- as.numeric(cut2(test.source$Indig_psns_Torres_Strait_Is_F, g=5))
test.source$Indig_psns_Torres_Strait_Is_M5 <- as.numeric(cut2(test.source$Indig_psns_Torres_Strait_Is_M, g=5))
test.source$Indigenous_P_Tot_F5 <- as.numeric(cut2(test.source$Indigenous_P_Tot_F, g=5))
test.source$Indigenous_P_Tot_M5 <- as.numeric(cut2(test.source$Indigenous_P_Tot_M, g=5))
test.source$Indigenous_psns_Aboriginal_F5 <- as.numeric(cut2(test.source$Indigenous_psns_Aboriginal_F, g=5))
test.source$Indigenous_psns_Aboriginal_M5 <- as.numeric(cut2(test.source$Indigenous_psns_Aboriginal_M, g=5))
test.source$Infant_Mort5 <- as.numeric(cut2(test.source$Infant_Mort, g=5))
test.source$Islam_F5 <- as.numeric(cut2(test.source$Islam_F, g=5))
test.source$Islam_M5 <- as.numeric(cut2(test.source$Islam_M, g=5))
test.source$Judaism_F5 <- as.numeric(cut2(test.source$Judaism_F, g=5))
test.source$Judaism_M5 <- as.numeric(cut2(test.source$Judaism_M, g=5))
test.source$Lang_spoken_home_Eng_only_F5 <- as.numeric(cut2(test.source$Lang_spoken_home_Eng_only_F, g=5))
test.source$Lang_spoken_home_Eng_only_M5 <- as.numeric(cut2(test.source$Lang_spoken_home_Eng_only_M, g=5))
test.source$Lang_spoken_home_Oth_Lang_F5 <- as.numeric(cut2(test.source$Lang_spoken_home_Oth_Lang_F, g=5))
test.source$Lang_spoken_home_Oth_Lang_M5 <- as.numeric(cut2(test.source$Lang_spoken_home_Oth_Lang_M, g=5))
test.source$Lat5 <- as.numeric(cut2(test.source$Lat, g=5))
test.source$lfs_Tot_LF_F5 <- as.numeric(cut2(test.source$lfs_Tot_LF_F, g=5))
test.source$lfs_Tot_LF_M5 <- as.numeric(cut2(test.source$lfs_Tot_LF_M, g=5))
test.source$lfs_Unmplyed_lookng_for_wrk_F5 <- as.numeric(cut2(test.source$lfs_Unmplyed_lookng_for_wrk_F, g=5))
test.source$lfs_Unmplyed_lookng_for_wrk_M5 <- as.numeric(cut2(test.source$lfs_Unmplyed_lookng_for_wrk_M, g=5))
test.source$Long5 <- as.numeric(cut2(test.source$Long, g=5))
test.source$M_BachDeg_Total5 <- as.numeric(cut2(test.source$M_BachDeg_Total, g=5))
test.source$M_PGrad_Deg_Total5 <- as.numeric(cut2(test.source$M_PGrad_Deg_Total, g=5))
test.source$M_Tot_Divorced5 <- as.numeric(cut2(test.source$M_Tot_Divorced, g=5))
test.source$M_Tot_Married5 <- as.numeric(cut2(test.source$M_Tot_Married, g=5))
test.source$M_Tot_N_a_volunteer5 <- as.numeric(cut2(test.source$M_Tot_N_a_volunteer, g=5))
test.source$M_Tot_Never_Married5 <- as.numeric(cut2(test.source$M_Tot_Never_Married, g=5))
test.source$M_Tot_Separated5 <- as.numeric(cut2(test.source$M_Tot_Separated, g=5))
test.source$M_Tot_Volunteer5 <- as.numeric(cut2(test.source$M_Tot_Volunteer, g=5))
test.source$M_Tot_Widowed5 <- as.numeric(cut2(test.source$M_Tot_Widowed, g=5))
test.source$Male_Ratio5 <- as.numeric(cut2(test.source$Male_Ratio, g=5))
test.source$Marriage_Prob5 <- as.numeric(cut2(test.source$Marriage_Prob, g=5))
test.source$Median_mortgage_repay_monthly5 <- as.numeric(cut2(test.source$Median_mortgage_repay_monthly, g=5))
test.source$Median_Tot_fam_inc_weekly5 <- as.numeric(cut2(test.source$Median_Tot_fam_inc_weekly, g=5))
test.source$Median_Tot_hhd_inc_weekly5 <- as.numeric(cut2(test.source$Median_Tot_hhd_inc_weekly, g=5))
test.source$Median_Tot_prsnl_inc_weekly5 <- as.numeric(cut2(test.source$Median_Tot_prsnl_inc_weekly, g=5))
test.source$No_Religion_F5 <- as.numeric(cut2(test.source$No_Religion_F, g=5))
test.source$No_Religion_M5 <- as.numeric(cut2(test.source$No_Religion_M, g=5))
test.source$O_MTG_Total5 <- as.numeric(cut2(test.source$O_MTG_Total, g=5))
test.source$O_OR_Total5 <- as.numeric(cut2(test.source$O_OR_Total, g=5))
test.source$Old_Age_F5 <- as.numeric(cut2(test.source$Old_Age_F, g=5))
test.source$Old_Age_M5 <- as.numeric(cut2(test.source$Old_Age_M, g=5))
test.source$Other_Religions_Tot_F5 <- as.numeric(cut2(test.source$Other_Religions_Tot_F, g=5))
test.source$Other_Religions_Tot_M5 <- as.numeric(cut2(test.source$Other_Religions_Tot_M, g=5))
test.source$R_Tot_Total5 <- as.numeric(cut2(test.source$R_Tot_Total, g=5))
test.source$SEO_Females5 <- as.numeric(cut2(test.source$SEO_Females, g=5))
test.source$SEO_Males5 <- as.numeric(cut2(test.source$SEO_Males, g=5))
test.source$Stay_at_home5 <- as.numeric(cut2(test.source$Stay_at_home, g=5))
test.source$Tot_Indig_status_ns_F5 <- as.numeric(cut2(test.source$Tot_Indig_status_ns_F, g=5))
test.source$Tot_Indig_status_ns_M5 <- as.numeric(cut2(test.source$Tot_Indig_status_ns_M, g=5))
test.source$Tot_Indigenous_F5 <- as.numeric(cut2(test.source$Tot_Indigenous_F, g=5))
test.source$Tot_Indigenous_M5 <- as.numeric(cut2(test.source$Tot_Indigenous_M, g=5))
test.source$Tot_Non_Indigenous_F5 <- as.numeric(cut2(test.source$Tot_Non_Indigenous_F, g=5))
test.source$Tot_Non_Indigenous_M5 <- as.numeric(cut2(test.source$Tot_Non_Indigenous_M, g=5))
test.source$Total_PDs_Dwellings5 <- as.numeric(cut2(test.source$Total_PDs_Dwellings, g=5))
test.source$Total_PDs_Persons5 <- as.numeric(cut2(test.source$Total_PDs_Persons, g=5))
test.source$Total_Total5 <- as.numeric(cut2(test.source$Total_Total, g=5))
test.source$Unemp5 <- as.numeric(cut2(test.source$Unemp, g=5))

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

# calculate correlation matrix
#correlationMatrix <- cor(train.source[,1:122])
# summarize the correlation matrix
#print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
#print(highlyCorrelated)

#use multithreading
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)


#subsets <- c(5,12,15,18,20,22,25,28,30,35,40)

# define the control using a random forest selection function
#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(train.source[,2:107], train.source[,107], metric='RMSE', sizes=subsets, rfeControl=control)
# summarize the results
#print(results)
# list the chosen features
#predictors(results)
# plot the results
#plot(results, type=c("g", "o"))

# define training control
train_control <- trainControl(method="cv", number=10)

# fit rlm model on training data
fit.rlmbest <- train(DEATHS ~ #AREA_SQKM
                   #+ Long
                   #+ Lat
                   #Age_0_4_yr_M
                   #+ Age_0_4_yr_F
                   #+ Age_5_14_yr_M
                   #+ Age_5_14_yr_F
                   #+ Age_15_19_yr_M
                   + Age_15_19_yr_F
                 + Age_20_24_yr_M
                 + Age_20_24_yr_F
                 #+ Age_25_34_yr_M
                 #+ Age_25_34_yr_F
                 #+ Age_35_44_yr_M
                 #+ Age_35_44_yr_F
                 #+ Age_45_54_yr_M
                 #+ Age_45_54_yr_F
                 #+ Age_55_64_yr_M
                 #+ Age_55_64_yr_F
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
                 + Indigenous_psns_Aboriginal_F
                 #+ Indig_psns_Torres_Strait_Is_M
                 #+ Indig_psns_Torres_Strait_Is_F
                 + Indig_Bth_Abor_Torres_St_Is_M
                 #+ Indig_Bth_Abor_Torres_St_Is_F
                 #+ Indigenous_P_Tot_M
                 #+ Indigenous_P_Tot_F
                 #+ Birthplace_Australia_M
                 + Birthplace_Australia_F
                 #+ Birthplace_Elsewhere_M
                 + Birthplace_Elsewhere_F
                 #+ Lang_spoken_home_Eng_only_M
                 + Lang_spoken_home_Eng_only_F
                 + Lang_spoken_home_Oth_Lang_M
                 + Lang_spoken_home_Oth_Lang_F
                 #+ Australian_citizen_M
                 #+ Australian_citizen_F
                 #+ High_yr_schl_comp_Yr_12_eq_M
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
                 #+ M_Tot_Separated
                 #+ M_Tot_Divorced
                 + M_Tot_Widowed
                 + M_Tot_Never_Married
                 + F_Tot_Married
                 + F_Tot_Separated
                 #+ F_Tot_Divorced
                 + F_Tot_Widowed
                 #+ F_Tot_Never_Married
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
                 #+ F_Tot_Volunteer
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
                 #+ birth_elsewhere
                 #+ unemployment
                 #+ density
                 #+ christian
                 #+ buddhism
                 #+ hinduism
                 #+ judaism
                 #+ islam 
                 #+ noreligion
                 +Age_85ov_F3
                 +Male_Ratio3
                 +Age_85ov_F4
                 +Count_Persons_other_dwgs_F4
                 +Infant_Mort4
                 +Age_85ov_F5
                 +O_MTG_Total5
                 +Count_Persons_other_dwgs_F5
                 +Male_Ratio5
                 +M_Tot_Widowed5
                 
                 , method = 'rlm', data = traindata, trControl = train_control)

pred.train.rlm <- predict(fit.rlmbest, newdata = traindata)
pred.valid.rlm <- predict(fit.rlmbest, newdata = validdata)
pred.test.rlm <- predict(fit.rlmbest, newdata = testdata)

traindata$rlm <- pred.train.rlm
validdata$rlm <- pred.valid.rlm
testdata$rlm <- pred.test.rlm

# fit model on training data
fit.all <-  train(DEATHS ~ +Age_5_14_yr_F
                  +Age_55_64_yr_M
                  +Age_55_64_yr_F
                  +Age_65_74_yr_F
                  +Age_65_74_yr_M
                  +Age_75_84_yr_F
                  +Age_75_84_yr_M
                  +Age_85ov_F
                  +Age_85ov_M
                  +AUS_MORT_DTH
                  +Australian_citizen_F
                  +Birthplace_Australia_F
                  +Christianity_Tot_F
                  +Count_Census_Nt_Ewhere_Aust_F
                  +Count_Persons_other_dwgs_F
                  +Count_Persons_other_dwgs_M
                  +Counted_Census_Night_home_F
                  +F_Tot_Divorced
                  +F_Tot_Never_Married
                  +F_Tot_Separated
                  +F_Tot_Widowed
                  +M_Tot_Divorced
                  +M_Tot_Widowed
                  +O_OR_Total
                  +Tot_Indig_status_ns_M
                  +Tot_Non_Indigenous_F
                  +Total_PDs_Dwellings
                  +Total_Total
                  +Age_85ov_F3
                  +Male_Ratio3
                  +Age_85ov_F4
                  +Count_Persons_other_dwgs_F4
                  +Infant_Mort4
                  +Age_85ov_F5
                  +O_MTG_Total5
                  +Count_Persons_other_dwgs_F5
                  +Male_Ratio5
                  +M_Tot_Widowed5
                  +rlm
                  
                  , data = traindata, method = 'glm', trControl = train_control)

#stop multithreading
stopCluster(cl)

# run model on training data
pred.train.glm <- predict(fit.all, newdata = traindata)

# run model on validation data and test data
pred.valid.glm <- predict(fit.all, newdata = validdata)
pred.test.glm <- predict(fit.all, newdata = testdata)

# calculate the root mean squared number (i.e. submission score)
sqrt(mean((pred.train.glm-traindata$DEATHS)^2))
sqrt(mean((pred.valid.glm-validdata$DEATHS)^2))

fit.all

#convert result to data frame and then create submission csv file
submit <- as.data.frame(pred.test.glm)
submit$rownumber = 1:nrow(submit)
colnames(submit) <- c("DEATHS","id")
write.table (submit[c("id","DEATHS")], sep=",", file = "c:/kaggle/data/submit12.csv", col.names=TRUE, row.names = FALSE)

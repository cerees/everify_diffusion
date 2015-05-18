#--------------#
################
###CLEAN DATA###
################
#--------------#

############################################
###SOURCE PACKAGES AND LOAD ORIGINAL DATA###
############################################
source("scripts/load_packages.R")
load("data/loaded_dfs.Rdata")

##############
###RESHAPES###
##############

###EVERIFY ADOPTION###
everify<-clear.labels(everify)
everify$adopt_year<-NA
everify$adopt_year[everify$ev1997==1]<-1997
everify$adopt_year[everify$ev1998==1]<-1998
everify$adopt_year[everify$ev1999==1]<-1999
everify$adopt_year[everify$ev2000==1]<-2000
everify$adopt_year[everify$ev2001==1]<-2001
everify$adopt_year[everify$ev2002==1]<-2002
everify$adopt_year[everify$ev2003==1]<-2003
everify$adopt_year[everify$ev2004==1]<-2004
everify$adopt_year[everify$ev2005==1]<-2005
everify$adopt_year[everify$ev2006==1]<-2006
everify$adopt_year[everify$ev2007==1]<-2007
everify$adopt_year[everify$ev2008==1]<-2008
everify$adopt_year[everify$ev2009==1]<-2009
everify$adopt_year[everify$ev2010==1]<-2010
everify$adopt_year[everify$ev2011==1]<-2011
everify$adopt_year[everify$ev2012==1]<-2012
everify$adopt_year[everify$ev2013==1]<-2013
everify$adopt_year[everify$ev2014==1]<-2014
everify_wide<-everify[c(1:2, 22, 3, 4:21)]
everify_long<-reshape(everify_wide,
        dir = "long",
        varying=list(names(everify_wide)[5:22]),
        v.names="adoption",
        idvar = c("state", "fips", "adopt_year", "censor"),
        timevar= "year",
        times=c(1997:2014),
        new.row.names = (1:900))
names(everify_long)[names(everify_long) == 'state'] <- 'state_abb'
names(everify_long)[names(everify_long) == 'fips'] <- 'state_fips'
rm(everify)

###ACS NATIVITY###
acs_nativity_file_paths<-list.files(path="data/originals/acs_data/nativity_and_race", pattern='*.csv', full=T)
acs_nativity_file_names<-lapply(acs_nativity_file_paths, function(x){
  library(tools)
  basename(file_path_sans_ext(x))})
acs_nativity_file_names<-do.call(rbind, acs_nativity_file_names)
for (i in seq(acs_nativity_files))
  names(acs_nativity_files)[i]<-paste(acs_nativity_file_names[i])
for (i in seq(acs_nativity_files))
  assign(tolower(names(acs_nativity_files[i])), acs_nativity_files[[i]])

#Assign year variables
years<-(2006:2013)

#Create Key to pull variables names
acs_nativity_06_key<-data.frame(cbind(c(names(acs_06_est_s0501)), c(acs_06_est_s0501[1,])))
acs_nativity_07_key<-data.frame(cbind(c(names(acs_07_1yr_s0501)), c(acs_07_1yr_s0501[1,])))
acs_nativity_08_key<-data.frame(cbind(c(names(acs_08_1yr_s0501)), c(acs_08_1yr_s0501[1,])))
acs_nativity_09_key<-data.frame(cbind(c(names(acs_09_1yr_s0501)), c(acs_09_1yr_s0501[1,])))
# For 06, 07 08, & 09
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; HC01_EST_VC18==blk_pop;
#HC01_EST_VC24==hisp_pop; HC03_EST_VC24==hisp_for_pop;
#HC05_EST_VC24==hisp_non_citizen_pop
acs_nativity_10_key<-data.frame(cbind(c(names(acs_10_1yr_s0501)), c(acs_10_1yr_s0501[1,])))
acs_nativity_11_key<-data.frame(cbind(c(names(acs_11_1yr_s0501)), c(acs_11_1yr_s0501[1,])))
acs_nativity_12_key<-data.frame(cbind(c(names(acs_12_1yr_s0501)), c(acs_12_1yr_s0501[1,])))
acs_nativity_13_key<-data.frame(cbind(c(names(acs_13_1yr_s0501)), c(acs_13_1yr_s0501[1,])))
# For 10, 11 12, & 13
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; "HC01_EST_VC18==blk_pop;
#HC01_EST_VC28==hisp_pop; HC03_EST_VC28==hisp_for_pop;
#HC05_EST_VC28==hisp_non_citizen_pop

#Subset Data Frames
acs_nativity_06_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                  "HC05_EST_VC01","HC01_EST_VC18",
                  "HC01_EST_VC24", "HC03_EST_VC24",
                  "HC05_EST_VC24")
acs_nativity_07_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC18",
                   "HC01_EST_VC24", "HC03_EST_VC24",
                   "HC05_EST_VC24")
acs_nativity_07_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC18",
                   "HC01_EST_VC24", "HC03_EST_VC24",
                   "HC05_EST_VC24")
acs_nativity_08_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC18",
                   "HC01_EST_VC24", "HC03_EST_VC24",
                   "HC05_EST_VC24")
acs_nativity_09_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC18",
                   "HC01_EST_VC24", "HC03_EST_VC24",
                   "HC05_EST_VC24")
acs_nativity_10_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC21",
                   "HC01_EST_VC28", "HC03_EST_VC28",
                   "HC05_EST_VC28")
acs_nativity_11_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC21",
                   "HC01_EST_VC28", "HC03_EST_VC28",
                   "HC05_EST_VC28")
acs_nativity_12_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC21",
                   "HC01_EST_VC28", "HC03_EST_VC28",
                   "HC05_EST_VC28")
acs_nativity_13_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC21",
                   "HC01_EST_VC28", "HC03_EST_VC28",
                   "HC05_EST_VC28")
acs_nativity_06<- acs_06_est_s0501[, c(acs_nativity_06_varnames)]
acs_nativity_07<- acs_07_1yr_s0501[, c(acs_nativity_07_varnames)]
acs_nativity_08<- acs_08_1yr_s0501[, c(acs_nativity_08_varnames)]
acs_nativity_09<- acs_09_1yr_s0501[, c(acs_nativity_09_varnames)]
acs_nativity_10<- acs_10_1yr_s0501[, c(acs_nativity_10_varnames)]
acs_nativity_11<- acs_11_1yr_s0501[, c(acs_nativity_11_varnames)]
acs_nativity_12<- acs_12_1yr_s0501[, c(acs_nativity_12_varnames)]
acs_nativity_13<- acs_13_1yr_s0501[, c(acs_nativity_13_varnames)]
acs_nativity_07_3yr<- acs_07_3yr_s0501[, c(acs_nativity_07_varnames)]
acs_nativity_08_3yr<- acs_08_3yr_s0501[, c(acs_nativity_08_varnames)]
acs_nativity_09_3yr<- acs_09_3yr_s0501[, c(acs_nativity_09_varnames)]
acs_nativity_10_3yr<- acs_10_3yr_s0501[, c(acs_nativity_10_varnames)]
acs_nativity_11_3yr<- acs_11_3yr_s0501[, c(acs_nativity_11_varnames)]
acs_nativity_12_3yr<- acs_12_3yr_s0501[, c(acs_nativity_12_varnames)]
acs_nativity_13_3yr<- acs_13_3yr_s0501[, c(acs_nativity_13_varnames)]
acs_nativity_09_5yr<- acs_09_5yr_s0501[, c(acs_nativity_09_varnames)]
acs_nativity_10_5yr<- acs_10_5yr_s0501[, c(acs_nativity_10_varnames)]
acs_nativity_11_5yr<- acs_11_5yr_s0501[, c(acs_nativity_11_varnames)]
acs_nativity_12_5yr<- acs_12_5yr_s0501[, c(acs_nativity_12_varnames)]
acs_nativity_13_5yr<- acs_13_5yr_s0501[, c(acs_nativity_13_varnames)]

not_in_2010_1yr_nativity<-acs_nativity_09$GEO.display.label[acs_nativity_09$GEO.display.label %in% acs_nativity_10$GEO.display.label==F]
not_in_2011_1yr_nativity<-acs_nativity_09$GEO.display.label[acs_nativity_09$GEO.display.label %in% acs_nativity_11$GEO.display.label==F]
not_in_2012_1yr_nativity<-acs_nativity_09$GEO.display.label[acs_nativity_09$GEO.display.label %in% acs_nativity_12$GEO.display.label==F]
not_in_2013_1yr_nativity<-acs_nativity_09$GEO.display.label[acs_nativity_09$GEO.display.label %in% acs_nativity_13$GEO.display.label==F]

acs_nativity_10<-rbind(acs_nativity_10, acs_nativity_10_3yr[acs_nativity_10_3yr$GEO.display.label %in% not_in_2010_1yr_nativity==T,])
acs_nativity_11<-rbind(acs_nativity_11, acs_nativity_11_3yr[acs_nativity_11_3yr$GEO.display.label %in% not_in_2011_1yr_nativity==T,])
acs_nativity_12<-rbind(acs_nativity_12, acs_nativity_12_3yr[acs_nativity_12_3yr$GEO.display.label %in% not_in_2012_1yr_nativity==T,])
acs_nativity_13<-rbind(acs_nativity_13, acs_nativity_13_3yr[acs_nativity_13_3yr$GEO.display.label %in% not_in_2013_1yr_nativity==T,])

not_in_2010_3yr_nativity<-acs_nativity_09$GEO.display.label[acs_nativity_09$GEO.display.label %in% acs_nativity_10$GEO.display.label==F]
not_in_2011_3yr_nativity<-acs_nativity_09$GEO.display.label[acs_nativity_09$GEO.display.label %in% acs_nativity_11$GEO.display.label==F]
not_in_2012_3yr_nativity<-acs_nativity_09$GEO.display.label[acs_nativity_09$GEO.display.label %in% acs_nativity_12$GEO.display.label==F]
not_in_2013_3yr_nativity<-acs_nativity_09$GEO.display.label[acs_nativity_09$GEO.display.label %in% acs_nativity_13$GEO.display.label==F]

acs_nativity_10<-rbind(acs_nativity_10, acs_nativity_10_5yr[acs_nativity_10_5yr$GEO.display.label %in% not_in_2010_3yr_nativity==T,])
acs_nativity_11<-rbind(acs_nativity_11, acs_nativity_11_5yr[acs_nativity_11_5yr$GEO.display.label %in% not_in_2011_3yr_nativity==T,])
acs_nativity_12<-rbind(acs_nativity_12, acs_nativity_12_5yr[acs_nativity_12_5yr$GEO.display.label %in% not_in_2012_3yr_nativity==T,])
acs_nativity_13<-rbind(acs_nativity_13, acs_nativity_13_5yr[acs_nativity_13_5yr$GEO.display.label %in% not_in_2013_3yr_nativity==T,])

acs_nativity_name_change<- function(x) {
  names(x) <- c("geoid", "state_fips", "state_name",
                "tot_pop", "foreign_pop", 
                "non_citizen_pop", "pct_blk",
                "pct_hisp", "pct_foreign_hisp",
                "pct_noncitizen_hisp")
  return(x)
}

acs_nativity_long<-lapply(list(acs_nativity_06,acs_nativity_07,acs_nativity_08,acs_nativity_09,
         acs_nativity_10, acs_nativity_11, acs_nativity_12, acs_nativity_13), acs_nativity_name_change)

for (i in seq(years)){
    acs_nativity_long[[i]]$year<-years[i]
  }
acs_nativity_long<-do.call(smartbind, acs_nativity_long)
acs_nativity_long<-acs_nativity_long[acs_nativity_long$geoid!="Id",]
acs_nativity_long<-acs_nativity_long[,c("geoid", "state_fips", "state_name", "year",
                      "tot_pop", "foreign_pop", 
                      "non_citizen_pop", "pct_blk",
                      "pct_hisp", "pct_foreign_hisp",
                      "pct_noncitizen_hisp")]

acs_nativity_long$pct_foreign<- (as.numeric(acs_nativity_long$foreign_pop)/as.numeric(acs_nativity_long$tot_pop))*100
acs_nativity_long$pct_non_citizen<- (as.numeric(acs_nativity_long$non_citizen_pop)/as.numeric(acs_nativity_long$tot_pop))*100

not_states_name<-c("Puerto Rico", "District of Columbia")
acs_nativity_long<- acs_nativity_long[(acs_nativity_long$state_name %in% not_states_name==F),]

###ACS EDUCATION###
acs_education_file_paths<-c(list.files(path="data/originals/acs_data/education_1year", pattern='*ann.csv', full=T),
                       list.files(path="data/originals/acs_data/education_3year", pattern='*ann.csv', full=T),
                       list.files(path="data/originals/acs_data/education_5year", pattern='*ann.csv', full=T))
acs_education_file_names<-lapply(acs_education_file_paths, function(x){
  library(tools)
  basename(file_path_sans_ext(x))})
acs_education_file_names<-do.call(rbind, acs_education_file_names)
for (i in seq(acs_education_files))
  names(acs_education_files)[i]<-paste(acs_education_file_names[i])
for (i in seq(acs_education_files))
  assign(tolower(names(acs_education_files[i])), acs_education_files[[i]])


#Assign year variables
years<-(2006:2013)

#Create Key to pull variables names
acs_education_06_key<-data.frame(cbind(c(names(acs_06_est_s1501_with_ann)), c(acs_06_est_s1501_with_ann[1,])))
acs_education_07_key<-data.frame(cbind(c(names(acs_07_1yr_s1501_with_ann)), c(acs_07_1yr_s1501_with_ann[1,])))
acs_education_08_key<-data.frame(cbind(c(names(acs_08_1yr_s1501_with_ann)), c(acs_08_1yr_s1501_with_ann[1,])))
acs_education_09_key<-data.frame(cbind(c(names(acs_09_1yr_s1501_with_ann)), c(acs_09_1yr_s1501_with_ann[1,])))
# For 06, 07 08, & 09
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; HC01_EST_VC18==blk_pop;
#HC01_EST_VC24==hisp_pop; HC03_EST_VC24==hisp_for_pop;
#HC05_EST_VC24==hisp_non_citizen_pop
acs_education_10_key<-data.frame(cbind(c(names(acs_10_1yr_s1501_with_ann)), c(acs_10_1yr_s1501_with_ann[1,])))
acs_education_11_key<-data.frame(cbind(c(names(acs_11_1yr_s1501_with_ann)), c(acs_11_1yr_s1501_with_ann[1,])))
acs_education_12_key<-data.frame(cbind(c(names(acs_12_1yr_s1501_with_ann)), c(acs_12_1yr_s1501_with_ann[1,])))
acs_education_13_key<-data.frame(cbind(c(names(acs_13_1yr_s1501_with_ann)), c(acs_13_1yr_s1501_with_ann[1,])))
# For 10, 11 12, & 13
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; "HC01_EST_VC18==blk_pop;
#HC01_EST_VC28==hisp_pop; HC03_EST_VC28==hisp_for_pop;
#HC05_EST_VC28==hisp_non_citizen_pop

#Subset Data Frames
acs_education_06_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                            "HC01_EST_VC15")
acs_education_07_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HC01_EST_VC15")
acs_education_08_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HC01_EST_VC15")
acs_education_09_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HC01_EST_VC15")
acs_education_10_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HC01_EST_VC17")
acs_education_11_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HC01_EST_VC17")
acs_education_12_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HC01_EST_VC17")
acs_education_13_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HC01_EST_VC17")
acs_education_06<- acs_06_est_s1501_with_ann[, c(acs_education_06_varnames)]
acs_education_07<- acs_07_1yr_s1501_with_ann[, c(acs_education_07_varnames)]
acs_education_08<- acs_08_1yr_s1501_with_ann[, c(acs_education_08_varnames)]
acs_education_09<- acs_09_1yr_s1501_with_ann[, c(acs_education_09_varnames)]
acs_education_10<- acs_10_1yr_s1501_with_ann[, c(acs_education_10_varnames)]
acs_education_11<- acs_11_1yr_s1501_with_ann[, c(acs_education_11_varnames)]
acs_education_12<- acs_12_1yr_s1501_with_ann[, c(acs_education_12_varnames)]
acs_education_13<- acs_13_1yr_s1501_with_ann[, c(acs_education_13_varnames)]
acs_education_07_3yr<- acs_07_3yr_s1501_with_ann[, c(acs_education_07_varnames)]
acs_education_08_3yr<- acs_08_3yr_s1501_with_ann[, c(acs_education_08_varnames)]
acs_education_09_3yr<- acs_09_3yr_s1501_with_ann[, c(acs_education_09_varnames)]
acs_education_10_3yr<- acs_10_3yr_s1501_with_ann[, c(acs_education_10_varnames)]
acs_education_11_3yr<- acs_11_3yr_s1501_with_ann[, c(acs_education_11_varnames)]
acs_education_12_3yr<- acs_12_3yr_s1501_with_ann[, c(acs_education_12_varnames)]
acs_education_13_3yr<- acs_13_3yr_s1501_with_ann[, c(acs_education_13_varnames)]
acs_education_09_5yr<- acs_09_5yr_s1501_with_ann[, c(acs_education_09_varnames)]
acs_education_10_5yr<- acs_10_5yr_s1501_with_ann[, c(acs_education_10_varnames)]
acs_education_11_5yr<- acs_11_5yr_s1501_with_ann[, c(acs_education_11_varnames)]
acs_education_12_5yr<- acs_12_5yr_s1501_with_ann[, c(acs_education_12_varnames)]
acs_education_13_5yr<- acs_13_5yr_s1501_with_ann[, c(acs_education_13_varnames)]

not_in_2010_1yr_education<-acs_education_09$GEO.display.label[acs_education_09$GEO.display.label %in% acs_education_10$GEO.display.label==F]
not_in_2011_1yr_education<-acs_education_09$GEO.display.label[acs_education_09$GEO.display.label %in% acs_education_11$GEO.display.label==F]
not_in_2012_1yr_education<-acs_education_09$GEO.display.label[acs_education_09$GEO.display.label %in% acs_education_12$GEO.display.label==F]
not_in_2013_1yr_education<-acs_education_09$GEO.display.label[acs_education_09$GEO.display.label %in% acs_education_13$GEO.display.label==F]

acs_education_10<-rbind(acs_education_10, acs_education_10_3yr[acs_education_10_3yr$GEO.display.label %in% not_in_2010_1yr_education==T,])
acs_education_11<-rbind(acs_education_11, acs_education_11_3yr[acs_education_11_3yr$GEO.display.label %in% not_in_2011_1yr_education==T,])
acs_education_12<-rbind(acs_education_12, acs_education_12_3yr[acs_education_12_3yr$GEO.display.label %in% not_in_2012_1yr_education==T,])
acs_education_13<-rbind(acs_education_13, acs_education_13_3yr[acs_education_13_3yr$GEO.display.label %in% not_in_2013_1yr_education==T,])

not_in_2010_3yr_education<-acs_education_09$GEO.display.label[acs_education_09$GEO.display.label %in% acs_education_10$GEO.display.label==F]
not_in_2011_3yr_education<-acs_education_09$GEO.display.label[acs_education_09$GEO.display.label %in% acs_education_11$GEO.display.label==F]
not_in_2012_3yr_education<-acs_education_09$GEO.display.label[acs_education_09$GEO.display.label %in% acs_education_12$GEO.display.label==F]
not_in_2013_3yr_education<-acs_education_09$GEO.display.label[acs_education_09$GEO.display.label %in% acs_education_13$GEO.display.label==F]

acs_education_10<-rbind(acs_education_10, acs_education_10_5yr[acs_education_10_5yr$GEO.display.label %in% not_in_2010_3yr_education==T,])
acs_education_11<-rbind(acs_education_11, acs_education_11_5yr[acs_education_11_5yr$GEO.display.label %in% not_in_2011_3yr_education==T,])
acs_education_12<-rbind(acs_education_12, acs_education_12_5yr[acs_education_12_5yr$GEO.display.label %in% not_in_2012_3yr_education==T,])
acs_education_13<-rbind(acs_education_13, acs_education_13_5yr[acs_education_13_5yr$GEO.display.label %in% not_in_2013_3yr_education==T,])

acs_education_name_change<- function(x) {
  names(x) <- c("geoid", "state_fips", "state_name",
                "pct_ba_degree")
  return(x)
}

acs_education_long<-lapply(list(acs_education_06,acs_education_07,acs_education_08,acs_education_09,
                               acs_education_10, acs_education_11, acs_education_12, acs_education_13), acs_education_name_change)

for (i in seq(years)){
  acs_education_long[[i]]$year<-years[i]
}
acs_education_long<-do.call(smartbind, acs_education_long)
acs_education_long<-acs_education_long[acs_education_long$geoid!="Id",]
acs_education_long<-acs_education_long[,c("geoid", "state_fips", "state_name", "year",
                                          "pct_ba_degree")]

not_states_name<-c("Puerto Rico", "District of Columbia")
acs_education_long<- acs_education_long[(acs_education_long$state_name %in% not_states_name==F),]

###ACS INCOME###
acs_income_file_paths<-c(list.files(path="data/originals/acs_data/income_1year", pattern='*ann.csv', full=T),
                            list.files(path="data/originals/acs_data/income_3year", pattern='*ann.csv', full=T),
                            list.files(path="data/originals/acs_data/income_5year", pattern='*ann.csv', full=T))
acs_income_file_names<-lapply(acs_income_file_paths, function(x){
  library(tools)
  basename(file_path_sans_ext(x))})
acs_income_file_names<-do.call(rbind, acs_income_file_names)
for (i in seq(acs_income_files))
  names(acs_income_files)[i]<-paste(acs_income_file_names[i])
for (i in seq(acs_income_files))
  assign(tolower(names(acs_income_files[i])), acs_income_files[[i]])


#Assign year variables
years<-(2006:2013)

#Create Key to pull variables names
acs_income_06_key<-data.frame(cbind(c(names(acs_06_est_b19013_with_ann)), c(acs_06_est_b19013_with_ann[1,])))
acs_income_07_key<-data.frame(cbind(c(names(acs_07_1yr_b19013_with_ann)), c(acs_07_1yr_b19013_with_ann[1,])))
acs_income_08_key<-data.frame(cbind(c(names(acs_08_1yr_b19013_with_ann)), c(acs_08_1yr_b19013_with_ann[1,])))
acs_income_09_key<-data.frame(cbind(c(names(acs_09_1yr_b19013_with_ann)), c(acs_09_1yr_b19013_with_ann[1,])))
# For 06, 07 08, & 09
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; HC01_EST_VC18==blk_pop;
#HC01_EST_VC24==hisp_pop; HC03_EST_VC24==hisp_for_pop;
#HC05_EST_VC24==hisp_non_citizen_pop
acs_income_10_key<-data.frame(cbind(c(names(acs_10_1yr_b19013_with_ann)), c(acs_10_1yr_b19013_with_ann[1,])))
acs_income_11_key<-data.frame(cbind(c(names(acs_11_1yr_b19013_with_ann)), c(acs_11_1yr_b19013_with_ann[1,])))
acs_income_12_key<-data.frame(cbind(c(names(acs_12_1yr_b19013_with_ann)), c(acs_12_1yr_b19013_with_ann[1,])))
acs_income_13_key<-data.frame(cbind(c(names(acs_13_1yr_b19013_with_ann)), c(acs_13_1yr_b19013_with_ann[1,])))
# For 10, 11 12, & 13
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; "HC01_EST_VC18==blk_pop;
#HC01_EST_VC28==hisp_pop; HC03_EST_VC28==hisp_for_pop;
#HC05_EST_VC28==hisp_non_citizen_pop

#Subset Data Frames
acs_income_06_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HD01_VD01")
acs_income_07_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HD01_VD01")
acs_income_08_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HD01_VD01")
acs_income_09_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HD01_VD01")
acs_income_10_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HD01_VD01")
acs_income_11_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HD01_VD01")
acs_income_12_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HD01_VD01")
acs_income_13_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                             "HD01_VD01")
acs_income_06<- acs_06_est_b19013_with_ann[, c(acs_income_06_varnames)]
acs_income_07<- acs_07_1yr_b19013_with_ann[, c(acs_income_07_varnames)]
acs_income_08<- acs_08_1yr_b19013_with_ann[, c(acs_income_08_varnames)]
acs_income_09<- acs_09_1yr_b19013_with_ann[, c(acs_income_09_varnames)]
acs_income_10<- acs_10_1yr_b19013_with_ann[, c(acs_income_10_varnames)]
acs_income_11<- acs_11_1yr_b19013_with_ann[, c(acs_income_11_varnames)]
acs_income_12<- acs_12_1yr_b19013_with_ann[, c(acs_income_12_varnames)]
acs_income_13<- acs_13_1yr_b19013_with_ann[, c(acs_income_13_varnames)]
acs_income_07_3yr<- acs_07_3yr_b19013_with_ann[, c(acs_income_07_varnames)]
acs_income_08_3yr<- acs_08_3yr_b19013_with_ann[, c(acs_income_08_varnames)]
acs_income_09_3yr<- acs_09_3yr_b19013_with_ann[, c(acs_income_09_varnames)]
acs_income_10_3yr<- acs_10_3yr_b19013_with_ann[, c(acs_income_10_varnames)]
acs_income_11_3yr<- acs_11_3yr_b19013_with_ann[, c(acs_income_11_varnames)]
acs_income_12_3yr<- acs_12_3yr_b19013_with_ann[, c(acs_income_12_varnames)]
acs_income_13_3yr<- acs_13_3yr_b19013_with_ann[, c(acs_income_13_varnames)]
acs_income_09_5yr<- acs_09_5yr_b19013_with_ann[, c(acs_income_09_varnames)]
acs_income_10_5yr<- acs_10_5yr_b19013_with_ann[, c(acs_income_10_varnames)]
acs_income_11_5yr<- acs_11_5yr_b19013_with_ann[, c(acs_income_11_varnames)]
acs_income_12_5yr<- acs_12_5yr_b19013_with_ann[, c(acs_income_12_varnames)]
acs_income_13_5yr<- acs_13_5yr_b19013_with_ann[, c(acs_income_13_varnames)]

not_in_2010_1yr_income<-acs_income_09$GEO.display.label[acs_income_09$GEO.display.label %in% acs_income_10$GEO.display.label==F]
not_in_2011_1yr_income<-acs_income_09$GEO.display.label[acs_income_09$GEO.display.label %in% acs_income_11$GEO.display.label==F]
not_in_2012_1yr_income<-acs_income_09$GEO.display.label[acs_income_09$GEO.display.label %in% acs_income_12$GEO.display.label==F]
not_in_2013_1yr_income<-acs_income_09$GEO.display.label[acs_income_09$GEO.display.label %in% acs_income_13$GEO.display.label==F]

acs_income_10<-rbind(acs_income_10, acs_income_10_3yr[acs_income_10_3yr$GEO.display.label %in% not_in_2010_1yr_income==T,])
acs_income_11<-rbind(acs_income_11, acs_income_11_3yr[acs_income_11_3yr$GEO.display.label %in% not_in_2011_1yr_income==T,])
acs_income_12<-rbind(acs_income_12, acs_income_12_3yr[acs_income_12_3yr$GEO.display.label %in% not_in_2012_1yr_income==T,])
acs_income_13<-rbind(acs_income_13, acs_income_13_3yr[acs_income_13_3yr$GEO.display.label %in% not_in_2013_1yr_income==T,])

not_in_2010_3yr_income<-acs_income_09$GEO.display.label[acs_income_09$GEO.display.label %in% acs_income_10$GEO.display.label==F]
not_in_2011_3yr_income<-acs_income_09$GEO.display.label[acs_income_09$GEO.display.label %in% acs_income_11$GEO.display.label==F]
not_in_2012_3yr_income<-acs_income_09$GEO.display.label[acs_income_09$GEO.display.label %in% acs_income_12$GEO.display.label==F]
not_in_2013_3yr_income<-acs_income_09$GEO.display.label[acs_income_09$GEO.display.label %in% acs_income_13$GEO.display.label==F]

acs_income_10<-rbind(acs_income_10, acs_income_10_5yr[acs_income_10_5yr$GEO.display.label %in% not_in_2010_3yr_income==T,])
acs_income_11<-rbind(acs_income_11, acs_income_11_5yr[acs_income_11_5yr$GEO.display.label %in% not_in_2011_3yr_income==T,])
acs_income_12<-rbind(acs_income_12, acs_income_12_5yr[acs_income_12_5yr$GEO.display.label %in% not_in_2012_3yr_income==T,])
acs_income_13<-rbind(acs_income_13, acs_income_13_5yr[acs_income_13_5yr$GEO.display.label %in% not_in_2013_3yr_income==T,])

acs_income_name_change<- function(x) {
  names(x) <- c("geoid", "state_fips", "state_name",
                "median_income")
  return(x)
}

acs_income_long<-lapply(list(acs_income_06,acs_income_07,acs_income_08,acs_income_09,
                                acs_income_10, acs_income_11, acs_income_12, acs_income_13), acs_income_name_change)

for (i in seq(years)){
  acs_income_long[[i]]$year<-years[i]
}
acs_income_long<-do.call(smartbind, acs_income_long)
acs_income_long<-acs_income_long[acs_income_long$geoid!="Id",]
acs_income_long<-acs_income_long[,c("geoid", "state_fips", "state_name", "year",
                                          "median_income")]

not_states_name<-c("Puerto Rico", "District of Columbia")
acs_income_long<- acs_income_long[(acs_income_long$state_name %in% not_states_name==F),]

###ACS URBANICITY###
acs_urbanicity_file_paths<-c(list.files(path="data/originals/acs_data/urbanicity", pattern='*ann.csv', full=T))
acs_urbanicity_file_names<-lapply(acs_urbanicity_file_paths, function(x){
  library(tools)
  basename(file_path_sans_ext(x))})
acs_urbanicity_file_names<-do.call(rbind, acs_urbanicity_file_names)
for (i in seq(acs_urbanicity_files))
  names(acs_urbanicity_files)[i]<-paste(acs_urbanicity_file_names[i])
for (i in seq(acs_urbanicity_files))
  assign(tolower(names(acs_urbanicity_files[i])), acs_urbanicity_files[[i]])


#Assign year variables
years<-(2006:2013)

#Create Key to pull variables names
acs_urbanicity_06_key<-data.frame(cbind(c(names(acs_06_est_b01003_with_ann)), c(acs_06_est_b01003_with_ann[1,])))
acs_urbanicity_07_key<-data.frame(cbind(c(names(acs_07_1yr_b01003_with_ann)), c(acs_07_1yr_b01003_with_ann[1,])))
acs_urbanicity_08_key<-data.frame(cbind(c(names(acs_08_1yr_b01003_with_ann)), c(acs_08_1yr_b01003_with_ann[1,])))
acs_urbanicity_09_key<-data.frame(cbind(c(names(acs_09_1yr_b01003_with_ann)), c(acs_09_1yr_b01003_with_ann[1,])))
# For 06, 07 08, & 09
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; HC01_EST_VC18==blk_pop;
#HC01_EST_VC24==hisp_pop; HC03_EST_VC24==hisp_for_pop;
#HC05_EST_VC24==hisp_non_citizen_pop
acs_urbanicity_10_key<-data.frame(cbind(c(names(acs_10_1yr_b01003_with_ann)), c(acs_10_1yr_b01003_with_ann[1,])))
acs_urbanicity_11_key<-data.frame(cbind(c(names(acs_11_1yr_b01003_with_ann)), c(acs_11_1yr_b01003_with_ann[1,])))
acs_urbanicity_12_key<-data.frame(cbind(c(names(acs_12_1yr_b01003_with_ann)), c(acs_12_1yr_b01003_with_ann[1,])))
acs_urbanicity_13_key<-data.frame(cbind(c(names(acs_13_1yr_b01003_with_ann)), c(acs_13_1yr_b01003_with_ann[1,])))
# For 10, 11 12, & 13
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; "HC01_EST_VC18==blk_pop;
#HC01_EST_VC28==hisp_pop; HC03_EST_VC28==hisp_for_pop;
#HC05_EST_VC28==hisp_non_citizen_pop

#Subset Data Frames
acs_urbanicity_06_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                          "HD01_VD01")
acs_urbanicity_07_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                          "HD01_VD01")
acs_urbanicity_08_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                          "HD01_VD01")
acs_urbanicity_09_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                          "HD01_VD01")
acs_urbanicity_10_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                          "HD01_VD01")
acs_urbanicity_11_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                          "HD01_VD01")
acs_urbanicity_12_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                          "HD01_VD01")
acs_urbanicity_13_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                          "HD01_VD01")
acs_urbanicity_06<- acs_06_est_b01003_with_ann[, c(acs_urbanicity_06_varnames)]
acs_urbanicity_07<- acs_07_1yr_b01003_with_ann[, c(acs_urbanicity_07_varnames)]
acs_urbanicity_08<- acs_08_1yr_b01003_with_ann[, c(acs_urbanicity_08_varnames)]
acs_urbanicity_09<- acs_09_1yr_b01003_with_ann[, c(acs_urbanicity_09_varnames)]
acs_urbanicity_10<- acs_10_1yr_b01003_with_ann[, c(acs_urbanicity_10_varnames)]
acs_urbanicity_11<- acs_11_1yr_b01003_with_ann[, c(acs_urbanicity_11_varnames)]
acs_urbanicity_12<- acs_12_1yr_b01003_with_ann[, c(acs_urbanicity_12_varnames)]
acs_urbanicity_13<- acs_13_1yr_b01003_with_ann[, c(acs_urbanicity_13_varnames)]
acs_urbanicity_07_3yr<- acs_07_3yr_b01003_with_ann[, c(acs_urbanicity_07_varnames)]
acs_urbanicity_08_3yr<- acs_08_3yr_b01003_with_ann[, c(acs_urbanicity_08_varnames)]
acs_urbanicity_09_3yr<- acs_09_3yr_b01003_with_ann[, c(acs_urbanicity_09_varnames)]
acs_urbanicity_10_3yr<- acs_10_3yr_b01003_with_ann[, c(acs_urbanicity_10_varnames)]
acs_urbanicity_11_3yr<- acs_11_3yr_b01003_with_ann[, c(acs_urbanicity_11_varnames)]
acs_urbanicity_12_3yr<- acs_12_3yr_b01003_with_ann[, c(acs_urbanicity_12_varnames)]
acs_urbanicity_13_3yr<- acs_13_3yr_b01003_with_ann[, c(acs_urbanicity_13_varnames)]
acs_urbanicity_09_5yr<- acs_09_5yr_b01003_with_ann[, c(acs_urbanicity_09_varnames)]
acs_urbanicity_10_5yr<- acs_10_5yr_b01003_with_ann[, c(acs_urbanicity_10_varnames)]
acs_urbanicity_11_5yr<- acs_11_5yr_b01003_with_ann[, c(acs_urbanicity_11_varnames)]
acs_urbanicity_12_5yr<- acs_12_5yr_b01003_with_ann[, c(acs_urbanicity_12_varnames)]
acs_urbanicity_13_5yr<- acs_13_5yr_b01003_with_ann[, c(acs_urbanicity_13_varnames)]

not_in_2010_1yr_urbanicity<-acs_urbanicity_09$GEO.display.label[acs_urbanicity_09$GEO.display.label %in% acs_urbanicity_10$GEO.display.label==F]
not_in_2011_1yr_urbanicity<-acs_urbanicity_09$GEO.display.label[acs_urbanicity_09$GEO.display.label %in% acs_urbanicity_11$GEO.display.label==F]
not_in_2012_1yr_urbanicity<-acs_urbanicity_09$GEO.display.label[acs_urbanicity_09$GEO.display.label %in% acs_urbanicity_12$GEO.display.label==F]
not_in_2013_1yr_urbanicity<-acs_urbanicity_09$GEO.display.label[acs_urbanicity_09$GEO.display.label %in% acs_urbanicity_13$GEO.display.label==F]

acs_urbanicity_10<-rbind(acs_urbanicity_10, acs_urbanicity_10_3yr[acs_urbanicity_10_3yr$GEO.display.label %in% not_in_2010_1yr_urbanicity==T,])
acs_urbanicity_11<-rbind(acs_urbanicity_11, acs_urbanicity_11_3yr[acs_urbanicity_11_3yr$GEO.display.label %in% not_in_2011_1yr_urbanicity==T,])
acs_urbanicity_12<-rbind(acs_urbanicity_12, acs_urbanicity_12_3yr[acs_urbanicity_12_3yr$GEO.display.label %in% not_in_2012_1yr_urbanicity==T,])
acs_urbanicity_13<-rbind(acs_urbanicity_13, acs_urbanicity_13_3yr[acs_urbanicity_13_3yr$GEO.display.label %in% not_in_2013_1yr_urbanicity==T,])

not_in_2010_3yr_urbanicity<-acs_urbanicity_09$GEO.display.label[acs_urbanicity_09$GEO.display.label %in% acs_urbanicity_10$GEO.display.label==F]
not_in_2011_3yr_urbanicity<-acs_urbanicity_09$GEO.display.label[acs_urbanicity_09$GEO.display.label %in% acs_urbanicity_11$GEO.display.label==F]
not_in_2012_3yr_urbanicity<-acs_urbanicity_09$GEO.display.label[acs_urbanicity_09$GEO.display.label %in% acs_urbanicity_12$GEO.display.label==F]
not_in_2013_3yr_urbanicity<-acs_urbanicity_09$GEO.display.label[acs_urbanicity_09$GEO.display.label %in% acs_urbanicity_13$GEO.display.label==F]

acs_urbanicity_10<-rbind(acs_urbanicity_10, acs_urbanicity_10_5yr[acs_urbanicity_10_5yr$GEO.display.label %in% not_in_2010_3yr_urbanicity==T,])
acs_urbanicity_11<-rbind(acs_urbanicity_11, acs_urbanicity_11_5yr[acs_urbanicity_11_5yr$GEO.display.label %in% not_in_2011_3yr_urbanicity==T,])
acs_urbanicity_12<-rbind(acs_urbanicity_12, acs_urbanicity_12_5yr[acs_urbanicity_12_5yr$GEO.display.label %in% not_in_2012_3yr_urbanicity==T,])
acs_urbanicity_13<-rbind(acs_urbanicity_13, acs_urbanicity_13_5yr[acs_urbanicity_13_5yr$GEO.display.label %in% not_in_2013_3yr_urbanicity==T,])

acs_urbanicity_name_change<- function(x) {
  names(x) <- c("geoid", "state_fips", "state_name",
                "urban_pop")
  return(x)
}

acs_urbanicity_long<-lapply(list(acs_urbanicity_06,acs_urbanicity_07,acs_urbanicity_08,acs_urbanicity_09,
                             acs_urbanicity_10, acs_urbanicity_11, acs_urbanicity_12, acs_urbanicity_13), acs_urbanicity_name_change)

for (i in seq(years)){
  acs_urbanicity_long[[i]]$year<-years[i]
}
acs_urbanicity_long<-do.call(smartbind, acs_urbanicity_long)
acs_urbanicity_long<-acs_urbanicity_long[acs_urbanicity_long$geoid!="Id",]
acs_urbanicity_long<-acs_urbanicity_long[,c("geoid", "state_fips", "state_name", "year",
                                    "urban_pop")]

not_states_name<-c("Puerto Rico", "District of Columbia", "District of Columbia -- Urban")
acs_urbanicity_long<- acs_urbanicity_long[(acs_urbanicity_long$state_name %in% not_states_name==F),]
state_names_labels<- data.frame(do.call('rbind', (strsplit(acs_urbanicity_long$state_name, ' --', fixed = T))))
acs_urbanicity_long$state_name<- state_names_labels$X1

###ACS MERGE###
acs_long<-merge(acs_income_long, acs_education_long, by = c("state_fips", "year"))
acs_long<-merge(acs_long, acs_nativity_long, by = c("state_fips", "year"))
acs_long<-merge(acs_long, acs_urbanicity_long, by = c("state_fips", "year"))
acs_long<-acs_long[,c(-6,-7,-9,-10,-20,-21)]
acs_long<-acs_long%>%
  rename(geoid=geoid.x, state_name=state_name.x)
acs_long[,c(5:16)]<- sapply(acs_long[,c(5:16)], as.numeric)
acs_long$pct_urban<- (acs_long$urban_pop/acs_long$tot_pop)*100
rm(list=ls(pattern="acs_[^l]"))
rm(list=ls(pattern="not*"))

###BLS###
apiDF <- function(data){
  df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   stringsAsFactors=FALSE)
  
  i <- 0
  for(d in data){
    i <- i + 1
    df[i,] <- unlist(d)
  }
  return(df)
}

for(c in 1:length(bls_codes$code)){
  df<-apiDF(bls_files$Results$series[[c]]$data)
  df$state<-paste0(as.character(bls_codes$state[c]))
  assign(paste0(bls_codes$state[c], "_unemp"), df)
}
bls_names<-ls(pattern="*_unemp")
bls_long<-ldply(bls_names, get)
bls_long<-bls_long[bls_long$periodName=="Annual",]
bls_long<-bls_long[,c("state", "year", "value")]
names(bls_long)<-c("state_name", "year", "unemp_rate")
rm(list=ls(pattern="_unemp"))
bls_long<-arrange(bls_long, state_name, year)
bls_long$unemp_rate<- as.numeric(bls_long$unemp_rate)
rm(list=ls(pattern="bls_[^l]"))
rm(df)

###PRESIDENTIAL ELECTIONS###
trim_parantheses<- function(x) gsub('\\(.\\)', '', x)
trim_trailing <- function (x) gsub("\\s+$", "", x)
trim_comma<- function (x) gsub(",", "", x)
pres_elections_long<-do.call(rbind, pres_files)
pres_elections_long$rep_ecvs[is.na(pres_elections_long$rep_ecvs)==T]<-0
pres_elections_long$dem_ecvs[is.na(pres_elections_long$dem_ecvs)==T]<-0
pres_elections_long[]<- data.frame(lapply(pres_elections_long, trim_comma))
pres_elections_long[,c("rep_ecvs", "dem_ecvs", "rep_presvotes", 
                       "dem_presvotes", "oth_presvotes", "tot_presvotes", 
                       "year")]<-data.frame(lapply(pres_elections_long[,c("rep_ecvs", "dem_ecvs", "rep_presvotes", 
                                                                          "dem_presvotes", "oth_presvotes", "tot_presvotes", 
                                                                          "year")], as.numeric))
pres_elections_long$pct_presvotes_rep<- (pres_elections_long$rep_presvotes/pres_elections_long$tot_presvotes)*100
pres_elections_long$pct_presvotes_dem<- (pres_elections_long$dem_presvotes/pres_elections_long$tot_presvotes)*100
pres_elections_long$pct_presvotes_oth<- (pres_elections_long$oth_presvotes/pres_elections_long$tot_presvotes)*100
pres_elections_long$pres_party_state[pres_elections_long$pct_presvotes_dem>pres_elections_long$pct_presvotes_rep]<-"D"
pres_elections_long$pres_party_state[pres_elections_long$pct_presvotes_dem<pres_elections_long$pct_presvotes_rep]<-"R"
pres_elections_long$rep_pres_state<-0
pres_elections_long$rep_pres_state[pres_elections_long$pres_party_state=="R"]<-1
pres_elections_long<-select(pres_elections_long, state_abb=state, year, tot_presvotes, pct_presvotes_dem, 
                            pct_presvotes_rep, pct_presvotes_oth, pres_party_state, rep_pres_state)
rm(pres_files)

###FEDERAL ELECTIONS###
fed_elections_long<-do.call(smartbind, fed_files)
fed_elections_long[, c("dem_fedvotes", "rep_fedvotes", "oth_fedvotes")]<-sapply(fed_elections_long[, c("dem_fedvotes", "rep_fedvotes", "oth_fedvotes")], function(x){
  as.numeric(gsub(",","", x))
})

not_states_abb<-c("AS", "GU", "PR", "MP", "VI", "DC")
fed_elections_long<-data.frame(fed_elections_long)%>%
  mutate(pct_fedvotes_dem=((dem_fedvotes/(dem_fedvotes+rep_fedvotes+oth_fedvotes))*100),
         pct_fedvotes_rep=((rep_fedvotes/(dem_fedvotes+rep_fedvotes+oth_fedvotes))*100))%>%
  select(state_abb=state, year, pct_fedvotes_dem, pct_fedvotes_rep, dem_fedvotes, rep_fedvotes)
fed_elections_long<- fed_elections_long[(fed_elections_long$state_abb %in% not_states_abb==F),]
fed_elections_long<- fed_elections_long[,c("state_abb", "year", "pct_fedvotes_dem", "pct_fedvotes_rep")]
rm(fed_files)

###GUBERNATORIAL ELECTIONS###
gov_elections_long<-do.call(smartbind, gov_elections_files)
gov_elections_long[]<-as.data.frame(sapply(gov_elections_long, trim_parantheses))
gov_elections_long[]<-as.data.frame(sapply(gov_elections_long, trim_trailing))
gov_elections_long[]<-as.data.frame(sapply(gov_elections_long, trim_comma))
gov_elections_long<-unique(gov_elections_long)
gov_elections_long<- gov_elections_long[,c("states", "year", "election_year",
                                           "rep_pct", "dem_pct", "ind_pct", 
                                           "oth_pct", "tot_votes")]
gov_elections_long[,c("year", "election_year",
                      "rep_pct", "dem_pct", "ind_pct", 
                      "oth_pct", "tot_votes")]<-data.frame(lapply(gov_elections_long[,c("year", "election_year",
                                                                  "rep_pct", "dem_pct", "ind_pct", 
                                                                  "oth_pct", "tot_votes")], as.numeric))
gov_elections_long<- rename(gov_elections_long, state_name=states, pct_govvotes_dem=dem_pct, pct_govvotes_rep=rep_pct,
                            pct_govvotes_ind=ind_pct, pct_govvotes_oth=oth_pct, tot_govvotes=tot_votes, 
                            gov_election_year=election_year)
rm(gov_elections_files)

###GUBERNATORIAL PARTY AFFILIATION###
for (i in seq(years)){
  gov_party_files[[i]]$year<-years[i]
}
gov_party_long<-do.call(smartbind, gov_party_files)
gov_party_long$rep_gov[gov_party_long$party=="R"]<-1
gov_party_long$rep_gov[gov_party_long$party!="R"]<-0
gov_party_long<- gov_party_long[,c("state", "year", "party", "rep_gov")]
gov_party_long<- rename(gov_party_long, state_name=state, gov_party=party)
rm(gov_party_files)

###EXECUTIVE ELECTION FREQUENCY###
state_exec_elections_long<-do.call(smartbind, state_exec_elections_files)
state_exec_elections_long$gov_election<-0
state_exec_elections_long$gov_election[grepl('^G,', state_exec_elections_long$elections)==T]<-1
state_exec_elections_long<-state_exec_elections_long[,c("state_name", "year", "gov_election")]
state_exec_elections_long$state_name<- trim_parantheses(state_exec_elections_long$state_name)
state_exec_elections_long$state_name<- trim_trailing(state_exec_elections_long$state_name)
rm(state_exec_elections_files)

###STATE LEGISLATURE ELECTIONS###
state_legs[27,2:153]<-NA
names(state_legs)<- gsub("oth", "other", names(state_legs))
names(state_legs)<- gsub("h_", "hz", names(state_legs))
state_legs_wide<-state_legs
state_legs_long<-state_legs_wide%>%
  gather(party_house, members, dems_lhz90:vac_uhz14) %>%
  separate(party_house, c("party_house", "year"), sep="z", extra = "merge")%>%
  spread(party_house, members) %>%
  arrange(state, year)
state_legs_long$year[as.numeric(state_legs_long$year)>89]<-paste0("19",as.character(state_legs_long$year[as.numeric(state_legs_long$year)>89]), sep="")
state_legs_long$year[as.numeric(state_legs_long$year)<90]<-paste0("20",as.character(state_legs_long$year[as.numeric(state_legs_long$year)<90]), sep="")
state_legs_long[2:10]<-sapply(state_legs_long[2:10], as.numeric)
state_legs_long$lower_house[state_legs_long$dems_lh>state_legs_long$rep_lh]<-"D"
state_legs_long$lower_house[state_legs_long$dems_lh<state_legs_long$rep_lh]<-"R"
state_legs_long$lower_house[state_legs_long$dems_lh==state_legs_long$rep_lh]<-"Split"
state_legs_long$upper_house[state_legs_long$dems_uh>state_legs_long$rep_uh]<-"D"
state_legs_long$upper_house[state_legs_long$dems_uh<state_legs_long$rep_uh]<-"R"
state_legs_long$upper_house[state_legs_long$dems_uh==state_legs_long$rep_uh]<-"Split"

state_legs_long$legs_control[(state_legs_long$lower_house=="D" 
                              & state_legs_long$upper_house=="D")]<-"D"
state_legs_long$legs_control[(state_legs_long$lower_house=="R" 
                              & state_legs_long$upper_house=="R")]<-"R"
state_legs_long$legs_control[(state_legs_long$lower_house!=state_legs_long$upper_house)]<-"Split"
state_legs_long$legs_control[(state_legs_long$lower_house=="Split" | state_legs_long$upper_house=="Split")]<-"Split"
state_legs_long$rep_legs[state_legs_long$legs_control=="R"]<-1
state_legs_long$rep_legs[state_legs_long$legs_control!="R"]<-0
state_legs_long$tot_seats<- rowSums(state_legs_long[,3:10])
state_legs_long$dem_seats<- state_legs_long$dems_lh+state_legs_long$dems_uh
state_legs_long$rep_seats<- state_legs_long$rep_lh+state_legs_long$rep_uh
state_legs_long$pct_dem_state_legs<- (state_legs_long$dem_seats/state_legs_long$tot_seats)*100
state_legs_long$pct_rep_state_legs<- (state_legs_long$rep_seats/state_legs_long$tot_seats)*100
state_legs_long<- select(state_legs_long, state_name=state, year, rep_state_legs=rep_legs, pct_rep_state_legs, pct_dem_state_legs)
state_legs_long$state_name<- trim_trailing(state_legs_long$state_name)
rm(state_legs)

###STATE LEGISLATURE ELECTION FREQUENCY###
state_legs_elections_long<-do.call(smartbind, state_legs_elections_files)
state_legs_elections_long[,c("tot_senate","tot_house","senate_elections",
                             "house_elections","year")]<-lapply(state_legs_elections_long[,c("tot_senate","tot_house",
                                                                                             "senate_elections",
                                                                                             "house_elections","year")], 
                                                                as.numeric)
state_legs_elections_long$tot_state_elections<- state_legs_elections_long$senate_elections+
  state_legs_elections_long$house_elections
state_legs_elections_long<- select(state_legs_elections_long, state_name, year, tot_state_elections)
#state_legs_elections_long$tot_state_elections[is.na(state_legs_elections_long$tot_state_elections)==T]<-
rm(state_legs_elections_files)
###CAMPAIGN CONTRIBUTIONS###
campaign_contributions_long<- campaign_contributions%>%
  select(state_abb=Election_State, year=Election_Year, party=General_Party, office=General_Office, sector=Broad_Sector, donations=X._of_Records, donation_ammount=Total_.)
campaign_contributions_long$state_legs_election<-0
campaign_contributions_long$state_legs_election[(campaign_contributions_long$office=="State Senate"|
                                                  campaign_contributions_long$office=="State House/Assembly")]<-1
campaign_contributions_long<- campaign_contributions_long%>%
  group_by(state_abb, year, sector)%>%
  summarise(donation_ammount=sum(donation_ammount))%>%
  spread(sector, donation_ammount)%>%
  rename(agriculture_donations=Agriculture, construction_donations=Construction)
campaign_contributions_long$agriculture_donations[campaign_contributions_long$agriculture_donations<0]<-0
campaign_contributions_long$construction_donations[campaign_contributions_long$construction_donations<0]<-0
campaign_contributions_long<-filter(campaign_contributions_long, year>2004)
rm(campaign_contributions)

###CITIZEN IDEAOLOGY###
citizen_ideaology_long<- citizen_ideaology%>%
  filter(year>2005)%>%
  rename(state_name=statename, state_fips=state, citizen_idea=citi6013, 
         state_idea_adacope=inst6013_adacope, 
         state_idea_nominal=inst6014_nom)
citizen_ideaology_long<- select(citizen_ideaology_long, state_name, year, state_fips, citizen_idea)
citizen_ideaology_long<- filter(citizen_ideaology_long, year>=2006)
rm(citizen_ideaology)

###STATE NAMES###
state_names_long<- state_names%>%
  select(state_name=name, state_abb=abbreviation, state_fips=fips_state)
rm(state_names, state_names_labels)
###STATES SHAPEFILE###
us_states <- us_states[!us_states$STATE_NAME %in% c("District of Columbia", "Alaska", "Hawaii", "Nebraska"),]
#us_states<-us_states[us_states$STATE_NAME!="District of Columbia",]
#us_states<-us_states[us_states$STATE_NAME!="Alaska",]
#us_states<-us_states[us_states$STATE_NAME!="Hawaii",]

#####################
###MERGE DATA SETS###
#####################
full_data_long<- merge(state_names_long[,c("state_abb", "state_name")], everify_long, by="state_abb", all.y=T)
full_data_long<- merge(full_data_long, acs_long, by=c("state_name", "year"), all.x=T)
full_data_long<- merge(full_data_long, bls_long, by=c("state_name", "year"), all.x=T)
full_data_long<- merge(full_data_long, campaign_contributions_long, by=c("state_abb", "year"), all.x=T)
full_data_long<- merge(full_data_long, citizen_ideaology_long, by=c("state_name", "year"), all.x=T)
full_data_long<- merge(full_data_long, fed_elections_long, by=c("state_abb", "year"), all.x=T)
full_data_long<- merge(full_data_long, gov_elections_long, by=c("state_name", "year"), all.x=T)
full_data_long<- merge(full_data_long, gov_party_long, by=c("state_name", "year"), all.x=T)
full_data_long<- merge(full_data_long, pres_elections_long, by=c("state_abb", "year"), all.x=T)
full_data_long<- merge(full_data_long, state_exec_elections_long, by=c("state_name", "year"), all.x=T)
full_data_long<- merge(full_data_long, state_legs_elections_long, by=c("state_name", "year"), all.x=T)
full_data_long<- merge(full_data_long, state_legs_long, by=c("state_name", "year"), all.x=T)
full_data_long<- merge(full_data_long, us_states@data[,c("STATE_NAME", "DRAWSEQ")], by.x="state_name", by.y="STATE_NAME", all.x=T)
full_data_long<- arrange(full_data_long, state_name, year) #sort to match spatial weights matrix creation
full_data_long<- na.locf(full_data_long) #Fill in missing data
full_data_long<- filter(full_data_long, year>2005, year<2014, state_name!="Hawaii", state_name!="Alaska", state_name!="Nebraska") #Remove Hawaii and Alaska
full_data_long<- rename(full_data_long, draw_seq=DRAWSEQ) #rename drawsequence
full_data_long<- arrange(full_data_long, year, draw_seq) #sort to match spatial weights matrix creation

##########################
###CREATE NEIGHBORHOODS###
##########################
us_states_coords<- coordinates(us_states) #get coordinates for plotting
us_states_nb<-poly2nb(us_states, queen = T, row.names=us_states@data$STATE_NAME) #queen based weights matrix
plot(us_states)
plot(us_states_nb, us_states_coords, add=T) #plot neighbors
us_states_lw<-nb2listw(us_states_nb, style="W") #create weights list
us_states_sn<-listw2sn(us_states_lw) 
us_states_spmatrix<-listw2mat(us_states_lw) #create matrix to expand weights to include years
colnames(us_states_spmatrix)<-row.names(us_states_spmatrix) #rename matrix with state names
us_states_nb_names<-as.data.frame(cbind(attr(us_states_sn, "region.id")[us_states_sn$from], attr(us_states_sn,
                                                               "region.id")[us_states_sn$to])) #get neighbor names
us_states_nb_names<-aggregate(V2~ as.factor(V1), us_states_nb_names, paste, collapse = ", ")  #collapse list of neighbor names
us_states_nb_names<- rename(us_states_nb_names, state_name=`as.factor(V1)`, neighbors=V2) #rename
full_data_long<- merge(full_data_long, us_states_nb_names, by="state_name", all.x=T) #merge neighbor names into full data set
full_data_long$state_name<- as.character(full_data_long$state_name)
names(full_data_long)
full_data_long<- arrange(full_data_long, year, draw_seq) #arrange data to match weights matrix
full_data_long[,c(2,4:8,10:35,37:41,43:48)]<-lapply(full_data_long[,c(2,4:8,10:35,37:41,43:48)], as.numeric)

################################################
###CREATE LONGITUDINAL SPATIAL WEIGHTS MATRIX###
################################################
us_states_wel<- as.data.frame(c(from=NULL, to=NULL, weight=NULL))#create empty df

for (y in unique(full_data_long$year)){ #loop through, copy matrix, append year to column/row names, bind yearly matrices
 print(y)
 matrix <- us_states_spmatrix
 colnames(matrix) <- paste(y, colnames(matrix), sep = "_")
 row.names(matrix) <- paste(y, row.names(matrix), sep = "_")
 matrix  <- graph.adjacency(matrix ,weighted=TRUE)
 wel <- get.data.frame(matrix)
 us_states_wel<-rbind(us_states_wel, wel)
}

us_states_wel_years=graph.data.frame( us_states_wel)
us_states_spmatrix_years<-get.adjacency(us_states_wel_years, attr="weight", sparse=FALSE)
us_states_lw_years <- mat2listw(us_states_spmatrix_years)
us_states_sn_years <- listw2sn(us_states_lw_years)

#############################
###CREATE LAGGED VARIABLES###
#############################

###SUBSET NUMERIC VARIABLES###
full_data_long_numeric<-sapply(full_data_long, is.numeric)
full_data_long_spatlag <- data.frame(matrix(ncol=length(full_data_long_numeric), nrow = 0))
colnames(full_data_long_spatlag) <- names(full_data_long_numeric)

###CREATE SPATIAL LAGGED VARIABLES USING LOOP###
for (y in sort(unique(full_data_long$year))){ #loop through, subset by year, create neighborlist, then create lagged variables
  print(y)
  us_states_ss<-filter(full_data_long, year==y)
  nb<-poly2nb(us_states) # Create neighbour list
  weights<-nb2listw(nb,style="W") # Create weights (row based)
  us_states_ss <- us_states_ss[sapply(us_states_ss,is.numeric)]
  spatial_lag<-lapply(us_states_ss , function(x){
    lag.listw(weights, x)})
  full_data_long_spatlag<-rbind(full_data_long_spatlag, spatial_lag)
}

colnames(full_data_long_spatlag) <- paste("lagged", colnames(full_data_long_spatlag), sep = "_") # add  lagged to column names
full_data_long<-cbind(full_data_long, full_data_long_spatlag) #merge with full_data_file

###############################
###SAVE OUT CLEANED DATA SET###
###############################
save(full_data_long, us_states, us_states_coords, us_states_lw, 
     us_states_nb, us_states_sn, us_states_spmatrix, us_states_sn_years, 
     us_states_lw_years, us_states_wel_years, us_states_spmatrix_years, 
     file="data/cleaned_dfs.Rdata")
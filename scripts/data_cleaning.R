########################
###LOAD ORIGINAL DATA###
########################
###SOURCE AND LOAD###
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


###ACS
acs_file_paths<-list.files(path="data/originals/acs_data", pattern='*.csv', full=T)
acs_file_names<-lapply(acs_file_paths, function(x){
  library(tools)
  basename(file_path_sans_ext(x))})
acs_file_names<-do.call(rbind, acs_file_names)
for (i in seq(acs_files))
  names(acs_files)[i]<-paste(acs_file_names[i])
for (i in seq(acs_files))
  assign(tolower(names(acs_files[i])), acs_files[[i]])


#Assign year variables
years<-(2006:2013)

#Create Key to pull variables names
acs_06_key<-data.frame(cbind(c(names(acs_06_est_s0501)), c(acs_06_est_s0501[1,])))
acs_07_key<-data.frame(cbind(c(names(acs_07_1yr_s0501)), c(acs_07_1yr_s0501[1,])))
acs_08_key<-data.frame(cbind(c(names(acs_08_1yr_s0501)), c(acs_08_1yr_s0501[1,])))
acs_09_key<-data.frame(cbind(c(names(acs_09_1yr_s0501)), c(acs_09_1yr_s0501[1,])))
# For 06, 07 08, & 09
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; HC01_EST_VC18==blk_pop;
#HC01_EST_VC24==hisp_pop; HC03_EST_VC24==hisp_for_pop;
#HC05_EST_VC24==hisp_non_citizen_pop
acs_10_key<-data.frame(cbind(c(names(acs_10_1yr_s0501)), c(acs_10_1yr_s0501[1,])))
acs_11_key<-data.frame(cbind(c(names(acs_11_1yr_s0501)), c(acs_11_1yr_s0501[1,])))
acs_12_key<-data.frame(cbind(c(names(acs_12_1yr_s0501)), c(acs_12_1yr_s0501[1,])))
acs_13_key<-data.frame(cbind(c(names(acs_13_1yr_s0501)), c(acs_13_1yr_s0501[1,])))
# For 10, 11 12, & 13
#HC01_EST_VC01==totpop; HC03_EST_VC01==for_pop; 
#HC05_EST_VC01==non_citizen_pop; "HC01_EST_VC18==blk_pop;
#HC01_EST_VC28==hisp_pop; HC03_EST_VC28==hisp_for_pop;
#HC05_EST_VC28==hisp_non_citizen_pop

#Subset Data Frames
acs_06_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                  "HC05_EST_VC01","HC01_EST_VC18",
                  "HC01_EST_VC24", "HC03_EST_VC24",
                  "HC05_EST_VC24")
acs_07_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC18",
                   "HC01_EST_VC24", "HC03_EST_VC24",
                   "HC05_EST_VC24")
acs_07_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC18",
                   "HC01_EST_VC24", "HC03_EST_VC24",
                   "HC05_EST_VC24")
acs_08_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC18",
                   "HC01_EST_VC24", "HC03_EST_VC24",
                   "HC05_EST_VC24")
acs_09_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC18",
                   "HC01_EST_VC24", "HC03_EST_VC24",
                   "HC05_EST_VC24")
acs_10_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC21",
                   "HC01_EST_VC28", "HC03_EST_VC28",
                   "HC05_EST_VC28")
acs_11_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC21",
                   "HC01_EST_VC28", "HC03_EST_VC28",
                   "HC05_EST_VC28")
acs_12_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC21",
                   "HC01_EST_VC28", "HC03_EST_VC28",
                   "HC05_EST_VC28")
acs_13_varnames<-c("GEO.id", "GEO.id2", "GEO.display.label",
                   "HC01_EST_VC01","HC03_EST_VC01",
                   "HC05_EST_VC01", "HC01_EST_VC21",
                   "HC01_EST_VC28", "HC03_EST_VC28",
                   "HC05_EST_VC28")
acs_06<- acs_06_est_s0501[, c(acs_06_varnames)]
acs_07<- acs_07_1yr_s0501[, c(acs_07_varnames)]
acs_08<- acs_08_1yr_s0501[, c(acs_08_varnames)]
acs_09<- acs_09_1yr_s0501[, c(acs_09_varnames)]
acs_10<- acs_10_1yr_s0501[, c(acs_10_varnames)]
acs_11<- acs_11_1yr_s0501[, c(acs_11_varnames)]
acs_12<- acs_12_1yr_s0501[, c(acs_12_varnames)]
acs_13<- acs_13_1yr_s0501[, c(acs_13_varnames)]
acs_07_3yr<- acs_07_3yr_s0501[, c(acs_07_varnames)]
acs_08_3yr<- acs_08_3yr_s0501[, c(acs_08_varnames)]
acs_09_3yr<- acs_09_3yr_s0501[, c(acs_09_varnames)]
acs_10_3yr<- acs_10_3yr_s0501[, c(acs_10_varnames)]
acs_11_3yr<- acs_11_3yr_s0501[, c(acs_11_varnames)]
acs_12_3yr<- acs_12_3yr_s0501[, c(acs_12_varnames)]
acs_13_3yr<- acs_13_3yr_s0501[, c(acs_13_varnames)]
acs_09_5yr<- acs_09_5yr_s0501[, c(acs_09_varnames)]
acs_10_5yr<- acs_10_5yr_s0501[, c(acs_10_varnames)]
acs_11_5yr<- acs_11_5yr_s0501[, c(acs_11_varnames)]
acs_12_5yr<- acs_12_5yr_s0501[, c(acs_12_varnames)]
acs_13_5yr<- acs_13_5yr_s0501[, c(acs_13_varnames)]

not_in_2010_1yr<-acs_09$GEO.display.label[acs_09$GEO.display.label %in% acs_10$GEO.display.label==F]
not_in_2011_1yr<-acs_09$GEO.display.label[acs_09$GEO.display.label %in% acs_11$GEO.display.label==F]
not_in_2012_1yr<-acs_09$GEO.display.label[acs_09$GEO.display.label %in% acs_12$GEO.display.label==F]
not_in_2013_1yr<-acs_09$GEO.display.label[acs_09$GEO.display.label %in% acs_13$GEO.display.label==F]

acs_10<-rbind(acs_10, acs_10_3yr[acs_10_3yr$GEO.display.label %in% not_in_2010_1yr==T,])
acs_11<-rbind(acs_11, acs_11_3yr[acs_11_3yr$GEO.display.label %in% not_in_2011_1yr==T,])
acs_12<-rbind(acs_12, acs_12_3yr[acs_12_3yr$GEO.display.label %in% not_in_2012_1yr==T,])
acs_13<-rbind(acs_13, acs_13_3yr[acs_13_3yr$GEO.display.label %in% not_in_2013_1yr==T,])

not_in_2010_3yr<-acs_09$GEO.display.label[acs_09$GEO.display.label %in% acs_10$GEO.display.label==F]
not_in_2011_3yr<-acs_09$GEO.display.label[acs_09$GEO.display.label %in% acs_11$GEO.display.label==F]
not_in_2012_3yr<-acs_09$GEO.display.label[acs_09$GEO.display.label %in% acs_12$GEO.display.label==F]
not_in_2013_3yr<-acs_09$GEO.display.label[acs_09$GEO.display.label %in% acs_13$GEO.display.label==F]

acs_10<-rbind(acs_10, acs_10_5yr[acs_10_5yr$GEO.display.label %in% not_in_2010_3yr==T,])
acs_11<-rbind(acs_11, acs_11_5yr[acs_11_5yr$GEO.display.label %in% not_in_2011_3yr==T,])
acs_12<-rbind(acs_12, acs_12_5yr[acs_12_5yr$GEO.display.label %in% not_in_2012_3yr==T,])
acs_13<-rbind(acs_13, acs_13_5yr[acs_13_5yr$GEO.display.label %in% not_in_2013_3yr==T,])

acs_name_change<- function(x) {
  names(x) <- c("geoid", "state_fips", "state_name",
                "tot_pop", "foreign_pop", 
                "non_citizen_pop", "pct_blk",
                "pct_hisp", "pct_foreign_hisp",
                "pct_noncitizen_hisp")
  return(x)
}

acs_long<-lapply(list(acs_06,acs_07,acs_08,acs_09,
         acs_10, acs_11, acs_12, acs_13), acs_name_change)

for (i in seq(years)){
    acs_long[[i]]$year<-years[i]
  }
acs_long<-do.call(smartbind, acs_long)
acs_long<-acs_long[acs_long$geoid!="Id",]
acs_long<-acs_long[,c("geoid", "state_fips", "state_name", "year",
                      "tot_pop", "foreign_pop", 
                      "non_citizen_pop", "pct_blk",
                      "pct_hisp", "pct_foreign_hisp",
                      "pct_noncitizen_hisp")]

acs_long$pct_foreign<- (as.numeric(acs_long$foreign_pop)/as.numeric(acs_long$tot_pop))*100
acs_long$pct_non_citizen<- (as.numeric(acs_long$non_citizen_pop)/as.numeric(acs_long$tot_pop))*100

not_states_name<-c("Puerto Rico", "District of Columbia")
acs_long<- acs_long[(acs_long$state_name %in% not_states_name==F),]

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
rm('_unemp')
bls_names<-ls(pattern="*_unemp")
bls_long<-ldply(bls_names, get)
bls_long<-bls_long[bls_long$periodName=="Annual",]
bls_long<-bls_long[,c("state", "year", "value")]
names(bls_long)<-c("state_name", "year", "unemp_rate")
rm(list=ls(pattern="_unemp"))
bls_long<-arrange(bls_long, state_name, year)

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

###GUBERNATORIAL ELECTIONS###
gov_elections_long<-do.call(smartbind, gov_elections_files)
gsub()

###GUBERNATORIAL PARTY AFFILIATION###
for (i in seq(years)){
  gov_party_files[[i]]$year<-years[i]
}
gov_party_long<-do.call(smartbind, gov_party_files)
gov_party_long$rep_gov[gov_party_long$party=="R"]<-1
gov_party_long$rep_gov[gov_party_long$party!="R"]<-0

#######################
###MISSING VARIABLES###
#######################
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

#Nebraska
#Create Frequency Time Series
freq_adopt_years<-as.data.frame(table(everify_wide$adopt_year))
names(freq_adopt_years)<-(c("year", "freq"))
freq_adopt_years$year<-as.numeric(as.character(freq_adopt_years$year))
freq_adopt_years[9,]<-freq_adopt_years[8,]
freq_adopt_years[8,]<-rbind(2013, 0)
freq_adopt_years$cumsum<-cumsum(freq_adopt_years$freq)
freq_adopt_years$hazard<-(50-freq_adopt_years$cumsum)

#Create Time Series
freq_adopt_ts<-ts(freq_adopt_years$freq, 
                   start=2006, end=2014, 1)
cumsum_adopt_ts<-ts(freq_adopt_years$cumsum, 
                   start=2006, end=2014, 1)
hazard_adopt_ts<-ts(freq_adopt_years$hazard, 
                   start=2006, end=2014, 1)

###IVs###


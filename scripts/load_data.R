#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############################
###LOAD ORIGINAL DATA FILES###
##############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#####################
###SOURCE PACKAGES###
#####################
source("scripts/load_packages.R")

###############
###LOAD DATA###
###############

###EVERIFY ADOPTION###
#everify timeing
#retrieved from the National Conference of State Legislatures
#http://www.ncsl.org/research/immigration/everify-faq.aspx
everify<-read.csv("data/originals/everify_adoption.csv")
names(everify)<- tolower(names(everify))

###ACS DATA###
#Retrieved from the ACS
#demographic data
#get census api here: http://api.census.gov/data/key_signup.html
#'Working with acs.R' provides a nice overview of using the acs package, 
#'however at this time it is not compatable w/one year estimates.
#install api
#api.key.install(key="1d4e749980069ead295f370d51a74db541f5b7d7")
#create geo.set, use acs.lookup() and geo.make()
#us.states.geo<-geo.make(state = "*")
#tot_pop<-acs.fetch(endyear=2011, span=5, geography=us.states.geo, table.number="B01003")
#Instead table number S0501 for years 2006-2013 retrieved from factfinder

#NATIVITY
acs_nativity_file_paths<-list.files(path="data/originals/acs_data/nativity_and_race", pattern='*.csv', full=T)
acs_nativity_files<-lapply(acs_nativity_file_paths, read.csv)

#URBANICITY
acs_urbanicity_file_paths<-c(list.files(path="data/originals/acs_data/urbanicity_2000", pattern="*ann.csv", full=T), 
                             list.files(path="data/originals/acs_data/urbanicity_2010", pattern="*ann.csv", full=T))
acs_urbanicity_files<-lapply(acs_urbanicity_file_paths, read.csv)
#EDUCAITON
acs_education_file_paths<-c(list.files(path="data/originals/acs_data/education_1year", pattern="*ann.csv", full=T), 
                             list.files(path="data/originals/acs_data/education_3year", pattern="*ann.csv", full=T),
                            list.files(path="data/originals/acs_data/education_5year", pattern="*ann.csv", full=T))
acs_education_files<-lapply(acs_education_file_paths, read.csv)

#INCOME
acs_income_file_paths<-c(list.files(path="data/originals/acs_data/income_1year", pattern="*ann.csv", full=T), 
                             list.files(path="data/originals/acs_data/income_3year", pattern="*ann.csv", full=T),
                         list.files(path="data/originals/acs_data/income_5year", pattern="*ann.csv", full=T))
acs_income_files<-lapply(acs_income_file_paths, read.csv)

#Urbanicity
acs_urbanicity_file_paths<-list.files(path="data/originals/acs_data/urbanicity", pattern='*ann.csv', full=T)
acs_urbanicity_files<-lapply(acs_urbanicity_file_paths, read.csv)

###BLS ECONOMIC DATA###
#Retrieved from the BLS
#API Key  API key is: a2e2cf2da69c44b6b7180cbbef83aba3.
bls_codes<-read.csv("data/originals/bls_data/bls_codes.csv")
payload<-list('seriesid'=bls_codes$code,
              'startyear'=2006,
              'endyear'=2014,
              'catalog'=FALSE,
              'calculations'=T,
              'annualaverage'=TRUE,
              'registrationKey'='a2e2cf2da69c44b6b7180cbbef83aba3')
bls_json <- blsAPI(payload, 2)
bls_files <- fromJSON(bls_json, nullValue = NA)

###POLITICAL VARIABLES###
#Republican Legislature
#retrieved from the Statistical Abstract
#http://statabs.proquest.com/sa/?id=d8b95d58-4917-43f9-8d8e-5a418a29253b
#Original source: http://knowledgecenter.csg.org/kc/category/content-type/content-type/book-states
state_legs<-read.csv("data/originals/state_legs_1990_2014.csv")

#Presidential Elections
#retrieved from the FEC
#http://www.fec.gov/pubrec/electionresults.shtml
pres_file_paths<-list.files(path="data/originals/pres_elections/", pattern='*.csv', full=T)
pres_files<-lapply(pres_file_paths, read.csv)

#Federal Elections
#retrieved from the FEC
#http://www.fec.gov/pubrec/electionresults.shtml
fed_file_paths<-list.files(path="data/originals/fed_elections/", pattern='*.csv', full=T)
fed_files<-lapply(fed_file_paths, read.csv)

#Gubernatorial Elections
#retreived from the Council of State Governments' Book of States
#http://knowledgecenter.csg.org/kc/category/content-type/content-type/book-states
gov_elections_file_paths<-list.files(path="data/originals/gov_elections/", pattern='*.csv', full=T)
gov_elections_files<-lapply(gov_elections_file_paths, read.csv)

#Governor Party Affiliation
#retreived from the Council of State Governments' Book of States
#http://knowledgecenter.csg.org/kc/category/content-type/content-type/book-states
gov_party_file_paths<-list.files(path="data/originals/governors", pattern='*.csv', full=T)
gov_party_files<-lapply(gov_party_file_paths, read.csv)

#Citizen Ideaology
citizen_ideaology<-read.csv("data/originals/citizen_ideaology.csv")

#Campaign Contributions 
campaign_contributions<-read.csv("data/originals/campaign_contributions.csv")

#State Legs Election Frequencies
state_legs_elections_file_paths<-list.files(path="data/originals/state_legs_elections/", pattern='*.csv', full=T)
state_legs_elections_files<-lapply(state_legs_elections_file_paths, read.csv)

#Executive Bracnch Election Frequencies
state_exec_elections_file_paths<-list.files(path="data/originals/state_exec_elections/", pattern='*.csv', full=T)
state_exec_elections_files<-lapply(state_exec_elections_file_paths, read.csv)

###STATE NAMES###
state_names<- read.csv(file="data/originals/state_names_table/state_table.csv")

###SPATIAL###
###retreived from ESRI###
us_states<-readOGR("data/originals/us_states", "states")

###################
###SAVE OUT DATA###
###################
save(everify, acs_nativity_files, bls_files, bls_codes, state_legs,
     pres_files, fed_files, gov_party_files, acs_urbanicity_files,
     gov_elections_files, us_states, campaign_contributions,
     citizen_ideaology, acs_education_files, acs_income_files,
     acs_urbanicity_files, state_exec_elections_files, state_legs_elections_files,
     state_names,
     file="data/loaded_dfs.Rdata")
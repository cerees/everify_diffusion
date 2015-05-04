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

###ACS DEMOGRAPHIC DATA###
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
acs_file_paths<-list.files(path="data/originals/acs_data", pattern='*.csv', full=T)
acs_files<-lapply(acs_file_paths, read.csv)

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

###SPATIAL###
###retreived from ESRI###
us_states<-readOGR("data/originals/us_states", "states")

###SAVE OUT DATA###
save(everify, acs_files, bls_files, bls_codes, state_legs,
     pres_files, fed_files, gov_party_files,
     gov_elections_files, us_states,
     file="data/loaded_dfs.Rdata")
#------------------------#
##########################
###EXPLORATORY ANALYSIS###
##########################
#------------------------#

###########################################
###SOURCE PACKAGES AND LOAD CLEANED DATA###
###########################################
source("scripts/load_packages.R")
load("data/cleaned_dfs.Rdata")

########################################
###DISCRETE TIME EVENT HISTORY MODELS###
########################################
full_data_long$time<- full_data_long$year-2005  #create time variable
full_data_long<- full_data_long%>%
  arrange(state_name, time, adoption)%>%
  select(-state_name)#order for even history analysis
aspatial_model<-
spatial_lag_model<-splogit(adoption~time+pct_blk+pct_hisp+pct_foreign+rep_gov+rep_state_legs,
              wmat=us_states_spmatrix_years, data=full_data_long)
additive_ilag_model<-glm(adoption~time+pct_blk+pct_hisp+pct_foreign+rep_gov+rep_state_legs+
            lagged_adoption+lagged_pct_blk+lagged_pct_hisp+lagged_pct_foreign+
            lagged_rep_gov+lagged_rep_state_legs, data=full_data_long, family = "binomial")
multiplicative_ilag_model_black<-
multiplicative_ilag_model_hisp
multiplicative_ilag_model_foreign
multiplicative_ilag_model_foreign
multiplicative_ilag_model_foreign
multiplicative_ilag_model_foreign

summary(test)
vif(test)
test<-glmulti(adoption~time+pct_blk+pct_hisp+pct_foreign+rep_gov+rep_state_legs+
                lagged_adoption+lagged_pct_blk+lagged_pct_hisp+lagged_pct_foreign+
                lagged_rep_gov+lagged_rep_state_legs, data=full_data_long)
nospace_event_history<-splogit(adoption~time, wmat=us_states_spmatrix_years, data = full_data_long)

nospace_event_history<-
dummy_event_history<-
dvlag_event_history<-
ivlag_event_history<-
# nospace_event_history<-lm(adoption~ time +
#       pct_foreign+ pct_foreign_hisp+citizen_idea+ unemp_rate+ pct_urban+
#       I(agriculture_donations/tot_state_elections)+
#       I(construction_donations/tot_state_elections)+
#       rep_gov+pct_blk, data = full_data_long)
summary(nospace_event_history)
lm.LMtests(nospace_event_history, us_states_lw_years, test=c("LMerr", "LMlag",
                                                             "RLMerr", "RLMlag", "SARMA"))
surv_everify<-Surv(full_data_long$year, full_data_long$adoption)
nospace_coxph<-coxph(surv_everify~ median_income+pct_blk+pct_foreign+pct_urban+rep_gov+rep_state_legs+time, data = full_data_long)
lm.LMtests(residuals(coxph1), )
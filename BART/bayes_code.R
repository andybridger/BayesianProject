#See link for information on bartMachine
#https://www.rdocumentation.org/packages/bartMachine/versions/1.2.6/topics/pd_plot 

#install packages if needed
list.of.packages <- c('devtools', 'ggplot2', 'readr', 'dplyr', 'tidyr', 'sandwich',
                      'tidyverse', 'bartMachine', 'data.table', 'lmtest','rJava',
                      'bartMachine', 'sandwich', 'lmtest', 'olsrr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#download packages if needed
lapply(c('devtools', 'ggplot2', 'readr', 'dplyr', 'tidyr', 'sandwich', 'rJava',
         'tidyverse', 'bartMachine', 'data.table', 'lmtest', 'olsrr'), require, character.only = TRUE)
options(java.parameters = "-Xmx2500m")

#download csv from my Github
urlfile="https://raw.githubusercontent.com/andybridger/BayesianProject/main/BART/epu_data.csv"

epu_data<-read_csv(url(urlfile))

#get details on dataset
str(epu_data)
class(epu_data)

###############################################
################DATA SET 1#####################
###############################################

#for data set 1 exclude all dates prior to 1997M1 and select countries
epu_data_m1 <- epu_data %>%
  filter(Year>=1997) %>%
  select('Australia', 'Brazil', 'Canada', 'Chile', 'Colombia',
         'France', 'Germany', 'Greece', 'India', 
         'Ireland', 'Italy', 'Japan', 'Korea',
         'Netherlands', 'Russia', 'Spain', 'UK',
         'US', 'Mainland China')

#take natual log
epu_data_m1<-log(epu_data_m1)
which(is.na(epu_data_m1))

#demean data
for (i in 1:ncol(epu_data_m1)){
  col_mean <- colMeans(epu_data_m1[,i], na.rm=T)
  epu_data_m1[,i] = epu_data_m1[,i] - col_mean
 }

#assign vector as Australia (predictive variable)
y <- epu_data_m1$Australia
#assign matrix of independent variables
df <- subset(epu_data_m1, 
             select = c('Brazil', 'Canada', 'Chile', 'Colombia',
                        'France', 'Germany', 'Greece', 'India', 
                        'Ireland', 'Italy', 'Japan', 'Korea',
                        'Netherlands', 'Russia', 'Spain', 'UK',
                        'US', 'Mainland China'))

#change to dataframe in order to run in bartMachine
df<- as.data.frame(df)

#build BART regression model using dataset 1 called "bart_machine"
set.seed(42)
bart_machine = bartMachine(df, y, num_burn_in = 2000, num_trees = 200, k=5, q=0.75, nu=10)
summary(bart_machine)

#assumption checking
#check_bart_error_assumptions(bart_machine)
#plot_convergence_diagnostics(bart_machine)

###############################################
################DATA SET 2#####################
###############################################

#for data set 2 exclude all dates prior to 2003M1 and after 2020 and select countries
epu_data_m2 <- epu_data %>%
  filter(Year>=2003) %>%
  filter(Year<2021) %>%
  select('Australia', 'Brazil', 'Canada', 'Chile', 'Colombia',
         'France', 'Germany', 'Greece', 'India', 
         'Ireland', 'Italy', 'Japan', 'Korea',
         'Netherlands', 'Russia', 'Spain', 'UK',
         'US', 'Mainland China','New Zealand', 'Hong Kong', 'Singapore')

#take natual log
epu_data_m2<-log(epu_data_m2)
which(is.na(epu_data_m2))

#demean data
for (i in 1:ncol(epu_data_m2)){
  col_mean <- colMeans(epu_data_m2[,i], na.rm=T)
  epu_data_m2[,i] = epu_data_m2[,i] - col_mean
}

#assign vector as Australia (predictive variable)
y_2 <- epu_data_m2$Australia
#assign matrix of independent variables
df_2 <- subset(epu_data_m2, 
               select = c('Brazil', 'Canada', 'Chile', 'Colombia',
                          'France', 'Germany', 'Greece', 'India', 
                          'Ireland', 'Italy', 'Japan', 'Korea',
                          'Netherlands', 'Russia', 'Spain', 'UK',
                          'US', 'Mainland China', 'New Zealand', 'Hong Kong', 'Singapore'))
#change to dataframe in order to run in bartMachine
df_2<- as.data.frame(df_2)

#build BART regression model for dataset 2
set.seed(91)
bart_machine_2 = bartMachine(df_2, y_2, num_burn_in = 2000, num_trees = 200, k=5, q=0.75, nu=10)
summary(bart_machine_2)

#assumption checking
#check_bart_error_assumptions(bart_machine_2)
#plot_convergence_diagnostics(bart_machine_2)

###############################################
#####################Plots#####################
###############################################

#plot the relative importance of variables in dataset 1 and dataset 2
partial_d1<-par(mfrow=c(1,2))

#Relative importance of top 10 variables for dataset 1
var_importance_d1<-investigate_var_importance(bart_machine, plot = TRUE,
                                              num_replicates_for_avg = 200, num_var_plot = 10)

#Relative importance of top 10 variables for dataset 2
var_importance_d2<-investigate_var_importance(bart_machine_2,plot = TRUE,
                                              num_replicates_for_avg = 200, num_var_plot = 10)

var_importance_d1$avg_var_props
var_importance_d2$avg_var_props

#dataset 1 significance of predictors and model fit
cov_bra<-cov_importance_test(bart_machine, covariates = 'Brazil',
                             num_permutation_samples = 20, plot = FALSE)
cov_can<-cov_importance_test(bart_machine, covariates = 'Canada',
                             num_permutation_samples = 20, plot = FALSE)
cov_chi<-cov_importance_test(bart_machine, covariates = 'Chile',
                             num_permutation_samples = 20, plot = FALSE)
cov_col<-cov_importance_test(bart_machine, covariates = 'Colombia',
                             num_permutation_samples = 20, plot = FALSE)
cov_fra<-cov_importance_test(bart_machine, covariates = 'France',
                             num_permutation_samples = 20, plot = FALSE)
cov_ger<-cov_importance_test(bart_machine, covariates = 'Germany',
                             num_permutation_samples = 20, plot = FALSE)
cov_gre<-cov_importance_test(bart_machine, covariates = 'Greece',
                             num_permutation_samples = 20, plot = FALSE)
cov_ind<-cov_importance_test(bart_machine, covariates = 'India',
                             num_permutation_samples = 20, plot = FALSE)
cov_ire<-cov_importance_test(bart_machine, covariates = 'Ireland',
                             num_permutation_samples = 20, plot = FALSE)
cov_ita<-cov_importance_test(bart_machine, covariates = 'Italy',
                             num_permutation_samples = 20, plot = FALSE)
cov_jap<-cov_importance_test(bart_machine, covariates = 'Japan',
                             num_permutation_samples = 20, plot = FALSE)
cov_kor<-cov_importance_test(bart_machine, covariates = 'Korea',
                             num_permutation_samples = 20, plot = FALSE)
cov_net<-cov_importance_test(bart_machine, covariates = 'Netherlands',
                             num_permutation_samples = 20, plot = FALSE)
cov_rus<-cov_importance_test(bart_machine, covariates = 'Russia',
                             num_permutation_samples = 20, plot = FALSE)
cov_spa<-cov_importance_test(bart_machine, covariates = 'Spain',
                             num_permutation_samples = 20, plot = FALSE)
cov_UK<-cov_importance_test(bart_machine, covariates = 'UK',
                            num_permutation_samples = 20, plot = FALSE)
cov_US<-cov_importance_test(bart_machine, covariates = 'US',
                            num_permutation_samples = 20, plot = FALSE)
cov_china<-cov_importance_test(bart_machine, covariates = 'Mainland China',
                               num_permutation_samples = 20, plot = FALSE)
cov_all<-cov_importance_test(bart_machine, covariates = NULL,
                               num_permutation_samples = 20, plot = FALSE)

#dataset 2 significance of predictors and model fit
cov_bra_2<-cov_importance_test(bart_machine_2, covariates = 'Brazil',
                               num_permutation_samples = 20, plot = FALSE)
cov_can_2<-cov_importance_test(bart_machine_2, covariates = 'Canada',
                               num_permutation_samples = 20, plot = FALSE)
cov_chi_2<-cov_importance_test(bart_machine_2, covariates = 'Chile',
                               num_permutation_samples = 20, plot = FALSE)
cov_col_2<-cov_importance_test(bart_machine_2, covariates = 'Colombia',
                               num_permutation_samples = 20, plot = FALSE)
cov_fra_2<-cov_importance_test(bart_machine_2, covariates = 'France',
                               num_permutation_samples = 20, plot = FALSE)
cov_ger_2<-cov_importance_test(bart_machine_2, covariates = 'Germany',
                               num_permutation_samples = 20, plot = FALSE)
cov_gre_2<-cov_importance_test(bart_machine_2, covariates = 'Greece',
                               num_permutation_samples = 20, plot = FALSE)
cov_ind_2<-cov_importance_test(bart_machine_2, covariates = 'India',
                               num_permutation_samples = 20, plot = FALSE)
cov_ire_2<-cov_importance_test(bart_machine_2, covariates = 'Ireland',
                               num_permutation_samples = 20, plot = FALSE)
cov_ita_2<-cov_importance_test(bart_machine_2, covariates = 'Italy',
                               num_permutation_samples = 20, plot = FALSE)
cov_jap_2<-cov_importance_test(bart_machine_2, covariates = 'Japan',
                               num_permutation_samples = 20, plot = FALSE)
cov_kor_2<-cov_importance_test(bart_machine_2, covariates = 'Korea',
                               num_permutation_samples = 20, plot = FALSE)
cov_net_2<-cov_importance_test(bart_machine_2, covariates = 'Netherlands',
                               num_permutation_samples = 20, plot = FALSE)
cov_rus_2<-cov_importance_test(bart_machine_2, covariates = 'Russia',
                               num_permutation_samples = 20, plot = FALSE)
cov_spa_2<-cov_importance_test(bart_machine_2, covariates = 'Spain',
                               num_permutation_samples = 20, plot = FALSE)
cov_UK_2<-cov_importance_test(bart_machine_2, covariates = 'UK',
                              num_permutation_samples = 20, plot = FALSE)
cov_US_2<-cov_importance_test(bart_machine_2, covariates = 'US',
                              num_permutation_samples = 20, plot = FALSE)
cov_china_2<-cov_importance_test(bart_machine_2, covariates = 'Mainland China',
                                 num_permutation_samples = 20, plot = FALSE)
cov_NZ_2<-cov_importance_test(bart_machine_2, covariates = 'New Zealand',
                              num_permutation_samples = 20, plot = FALSE)
cov_HK_2<-cov_importance_test(bart_machine_2, covariates = 'Hong Kong',
                              num_permutation_samples = 20, plot = FALSE)
cov_sin_2<-cov_importance_test(bart_machine_2, covariates = 'Singapore',
                               num_permutation_samples = 20, plot = FALSE)
cov_all_2<-cov_importance_test(bart_machine_2, covariates = NULL,
                               num_permutation_samples = 20, plot = FALSE)
#make data frame of significance of predictors
Country <- c('Brazil', 'Canada', 'Chile', 'Colombia',
             'France', 'Germany', 'Greece', 'India', 
             'Ireland', 'Italy', 'Japan', 'Korea',
             'Netherlands', 'Russia', 'Spain', 'UK',
             'US', 'Mainland China','New Zealand', 'Hong Kong', 'Singapore')
Dataset1 <- c(cov_bra$pval, cov_can$pval, cov_chi$pval, cov_col$pval,
              cov_fra$pval,cov_ger$pval, cov_gre$pval,cov_ind$pval, 
              cov_ire$pval,cov_ita$pval, cov_jap$pval,cov_kor$pval, 
              cov_net$pval,cov_rus$pval, cov_spa$pval,cov_UK$pval,
              cov_US$pval,cov_china$pval, NA, NA, NA)
Dataset2 <- c(cov_bra_2$pval, cov_can_2$pval, cov_chi_2$pval, cov_col_2$pval,
              cov_fra_2$pval,cov_ger_2$pval, cov_gre_2$pval,cov_ind_2$pval, 
              cov_ire_2$pval,cov_ita_2$pval, cov_jap_2$pval,cov_kor_2$pval, 
              cov_net_2$pval,cov_rus_2$pval, cov_spa_2$pval,cov_UK_2$pval,
              cov_US_2$pval,cov_china_2$pval, cov_NZ_2$pval, cov_HK_2$pval, cov_sin_2$pval)
signif <- data.frame(Country, Dataset1, Dataset2)
print(signif)

#OLS for dataset 1 with HAC covariance matrix
model_1 <- lm(Australia ~.,
              data = epu_data_m1)
coeftest(model_1, vcov=vcovHAC(model_1))
summary(model_1)
ols_test_normality(model_1)

#OLS for dataset 2 with HAC covariance matrix
model_2 <- lm(Australia ~.,
              data = epu_data_m2)
model_2_coef<-coeftest(model_2, vcov=vcovHAC(model_2))
summary(model_2)
ols_test_normality(model_2)

#partial dependence plot for dataset 1
partial_d1<-par(mfrow=c(2,2))
pd_plot(bart_machine, "Japan")
pd_plot(bart_machine, "US")
pd_plot(bart_machine, "Mainland China")
pd_plot(bart_machine, "Russia")

#partial dependence plot for dataset 2
partial_d1<-par(mfrow=c(2,2))
pd_plot(bart_machine_2, "India")
pd_plot(bart_machine_2, "UK")
pd_plot(bart_machine_2, "New Zealand")
pd_plot(bart_machine, "Italy")

#look at interactions
#interaction_investigator(bart_machine, num_replicates_for_avg = 25, num_var_plot = 10, bottom_margin = 5)

#get_var_counts_over_chain(bart_machine)

#get_var_props_over_chain(bart_machine)

#get_var_props_over_chain(bart_machine, type = "splits")

#check number of trees to stability
#rmse_by_num_trees(bart_machine, num_replicates = 20)
#rmse_by_num_trees(bart_machine_2, num_replicates = 20)

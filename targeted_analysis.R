### targeted_analysis.R --- 
### Code:
library(targets)
library(lmtp)
library(riskRegression)
library(prodlim)
library(rtmle)
# NOTE: you need the newest CRAN version of riskRegression (2026.3.11), prodlim (2026.3.11), rtmle (2026.5.21)
# NOTE: the lmtp analysis takes more than 90 minutes to run
tar_make()
tar_load_globals()
tar_load_everything()

plot_unadjusted_vs_counterfactual_Q1
plot_riskRegression_vs_counterfactual_Q1
plot_lmtp_vs_counterfactual_Q1
plot_rtmle_vs_counterfactual_Q1


######################################################################
### targeted_analysis.R ends here

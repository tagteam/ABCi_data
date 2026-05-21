library(targets)
tar_source("functions")
tar_option_set(packages = c("survival",
                            "prodlim",
                            "riskRegression",
                            "rms",
                            "tidyverse",
                            "data.table",
                            "lmtp",
                            "truncnorm",
                            "ggplot2",
                            "gridExtra",
                            "tidyr",
                            "rtmle"))
list(
    #
    # Q1:  generating real world data where time to
    #      event is time from treatment start
    #      and treatment is not randomized
    #
    tar_target(name = dta,
               command = {
                   generate_ABCi_data(n = 10000)
               }),
    #
    # Q1:  calculating counterfactual risks
    #      using 4 copies of a large dataset  
    #      where treatment is set to each value ("A","B","C","D")
    #
    tar_target(name = counterfactual_risks,
               command = {
                   get_counterfactual_risks(
                       n = 100000,
                       verbose = FALSE
                   )
               }),
    # table format
    tar_target(name = table_counterfactual_risks,
               command = {
                   as.data.table(counterfactual_risks)[,.(treatment,time,cause = factor(cause,labels = c("MACE","Death")),absolute_risk)]
               }),
    #
    # unadjusted estimate of risks using Aalen-Johansen estimator
    #
    tar_target(name = unadjusted_risks,
               command = {
                   analyse_Q1_unadjusted(dta)
               }),
    #
    # plot unadjusted risk estmates against counterfactual risks
    #
    tar_target(
        name = plot_unadjusted_vs_counterfactual_Q1,
        command = {
            plot_estimated_vs_counterfactual(
                estimated = as.data.table(
                    unadjusted_risks,
                    cause = 1,
                    times = seq(0,5,0.1)*365.25
                )[,list(treatment = as.character(treat_type),estimate = absolute_risk,time = time)],
                counterfactual = counterfactual_risks
            )
        }
    ),
    #
    # lmtp
    # 
    tar_target(
        name = lmtp_Q1,
        command = {
            analyse_Q1_lmtp(dta)
        },
        cue = tar_cue(mode = "never")
    ),
    tar_target(
        name = plot_lmtp_vs_counterfactual_Q1,
        command = {
            plot_estimated_vs_counterfactual(
                estimated = mutate(filter(lmtp_Q1,event == "MACE"),
                                   estimate = estimate,
                                   time = rep(seq(0,5,0.1),4)*365.25),
                counterfactual = counterfactual_risks
            )
        }
    ),
    #
    # rtmle
    # 
    tar_target(name = rtmle_Q1,
               command = {
                   analyse_Q1_rtmle(dta)
               }),
    tar_target(name = plot_rtmle_vs_counterfactual_Q1,
               command = {
                   plot_estimated_vs_counterfactual(
                       estimated = rtmle_Q1$estimate$Main_analysis[Target_parameter == "Risk",
                                                                   list(
                                                                       treatment = gsub("treatment_","",Protocol),
                                                                       time = 365.25*seq(0.1,5,0.1)[Time_horizon],
                                                                       estimate = Estimate
                                                                   )],
                       counterfactual = counterfactual_risks
                   )
               }),
    #
    # riskRegression G-formula (not doubly robust)
    # 
    tar_target(name = riskRegression_Q1,
               command = {
                   analyse_Q1_riskRegression(dta)
               }),
    tar_target(name = plot_riskRegression_vs_counterfactual_Q1,
               command = {
                   plot_estimated_vs_counterfactual(
                       estimated = riskRegression_Q1[,time := 365.25*time],
                       counterfactual = counterfactual_risks
                   )
               })
)

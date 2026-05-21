analyse_Q1_rtmle <- function(dta){
    requireNamespace("rtmle")
    requireNamespace("data.table")
    setDT(dta)
    # set value 0 for administrative censoring
    dta[event_type==3,event_type := 0]
    # initialize rtmle object
    x <- rtmle_init(
        time_grid = seq(0,5,.1)*365.24,
        weight_truncation = c(0.001,0.999),
        name_outcome = "Y",
        name_censoring = "C",
        name_competing = "D",
        name_id = "id",
        censored_label = 0,
        censored_levels = 1:0
    )
    # add baseline covariates to the object
    x <- add_baseline_data(
        x,
        data = dta[,list(id,sex,age,smok_former,smok_current,diabdur,bmi,hb,hyp,dys,cvd,kidney,panc,
                         treat_status_A_0 = treat_status_A,treat_status_B_0 = treat_status_B,
                         treat_status_C_0 = treat_status_C,treat_status_D_0 = treat_status_D)]
    )
    # add outcome, competing risk and censoring
    x <- add_long_data(
        x,
        outcome_data=dta[event_type == 1,.(id,date = event_time_new)],
        censored_data=dta[event_type == 0,.(id,date = event_time_new)],
        competing_data=dta[event_type == 2,.(id,date = event_time_new)],
        timevar_data=NULL
    )
    # transform data to wide format
    x <- long_to_wide(x,start_followup_date = 0)
    # prepare data for the analysis
    x <- prepare_rtmle_data(x)
    # define the treatment arms of the emulated trial where each
    # person can receive only one treatment 
    x <- protocol(
        x,
        name = "treatment_A", # withold BCD
        intervention = data.table(
            time_node = 0,
            treat_status_A = factor(1,levels = 0:1),
            treat_status_B = factor(0,levels = 0:1),
            treat_status_C = factor(0,levels = 0:1),
            treat_status_D = factor(0,levels = 0:1)
        ),
        expand = FALSE
    )
    x <- protocol(
        x,
        name = "treatment_B", # withold ACD
        intervention = data.table(
            time_node = 0,
            treat_status_A = factor(0,levels = 0:1),
            treat_status_B = factor(1,levels = 0:1),
            treat_status_C = factor(0,levels = 0:1),
            treat_status_D = factor(0,levels = 0:1)
        ),
        expand = FALSE
    )
    x <- protocol(
        x,
        name = "treatment_C", # withold ABD
        intervention = data.table(
            time_node = 0,
            treat_status_A = factor(0,levels = 0:1),
            treat_status_B = factor(0,levels = 0:1),
            treat_status_C = factor(1,levels = 0:1),
            treat_status_D = factor(0,levels = 0:1)
        ),
        expand = FALSE
    )
    x <- protocol(
        x,
        name = "treatment_D", # withold ABC
        intervention = data.table(
            time_node = 0,
            treat_status_A = factor(0,levels = 0:1),
            treat_status_B = factor(0,levels = 0:1),
            treat_status_C = factor(0,levels = 0:1),
            treat_status_D = factor(1,levels = 0:1)
        ),
        expand = FALSE
    )
    # define target_parameter
    x <- target(
        x,
        name = "Outcome_risk",
        estimator = "tmle",
        protocols = c("treatment_A","treatment_B","treatment_C","treatment_D")
    )
    # specify covariates for all nuisance parameter models
    # avoiding overparametrization 
    x <- model_formula(x,exclusion_rules = list("treat_status_*" = "treat*"))
    # set weight truncation to arbitrary values 
    x$tuning_parameters$weight_truncation <- c(0.001,0.999)
    # run the ltmle estimator for all time horizons 
    x <- run_rtmle(
        x,
        time_horizon = 1:50,
        learner = list(
            name = "undersmoothed elastic penalized regression",
            fun = "learn_glmnet",
            args = list(
                alpha = 0.5,
                selector = "undersmooth"
            )
        )
    )
    x
}

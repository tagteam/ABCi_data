analyse_Q1_riskRegression <- function(dta){
    requireNamespace("riskRegression")
    requireNamespace("data.table")
    setDT(dta)
    # set value 0 for administrative censoring
    dta[event_type==3,event_type := 0]
    # re-scale time from days to years
    dta[,time_yrs := event_time_new/365.25]
    # recreate treatment factor
    dta[,treatment := factor(1*treat_status_A+2*treat_status_B+3*treat_status_C+4*treat_status_D,levels = 1:4,labels = c("A","B","C","D"))]
    # fit cause-specific Cox regression 
    fit <- CSC(
        Hist(time_yrs,event_type)~treatment + sex + age + smok_former + smok_current + diabdur + bmi + hb + hyp + dys + cvd + kidney + panc,
        data = dta
    )
    ## fit <- CSC(Hist(time_yrs,event_type)~treatment,data = dta)
    # calculate average treatment effect using G-formula
    ate_fit <- ate(event = fit,treatment = "treatment",estimator = "GFORMULA",data = dta,se = FALSE,verbose = FALSE,times = seq(0,5,.1),cause = 1)
    ate_risks <- summary(ate_fit,short = TRUE)$meanRisk
    ate_risks
}

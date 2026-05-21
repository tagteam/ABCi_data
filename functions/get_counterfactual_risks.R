get_counterfactual_risks <- function(n = 100000,verbose = TRUE){
    dta.dgm <- do.call(
        rbind,
        lapply(c("A","B","C","D"),function(trt.fix){
            generate_ABCi_counterfactual_data(n = n,
                                              trt.fix = trt.fix,
                                              verbose = verbose)[,c("event_time_new","event_type_cr","treatment")]
        })
    )
    ## cr.trt.true<-survfit(Surv(event_time_new,event_type_cr)~1,data=dta.dgm)
    fit_counterfactual_risks <- prodlim(
        Hist(event_time_new,event_type_cr)~treatment,
        data=dta.dgm
    )
    fit_counterfactual_risks
}

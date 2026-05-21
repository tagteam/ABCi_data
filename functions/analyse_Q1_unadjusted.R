analyse_Q1_unadjusted <- function(dta.analysis){
    dta.analysis$event_type_cr<-ifelse(dta.analysis$event_type==3,0,dta.analysis$event_type)
    Q1_unadjusted <- prodlim(Hist(event_time_new,event_type_cr)~treat_type,
                             data=dta.analysis)
    Q1_unadjusted
}

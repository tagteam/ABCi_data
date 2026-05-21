#######################################################
#Q1: Estimates cumulative incidences of mace and death 
#under the strategies of starting treatment A,B,C or D in people with hb>=7.5
#Using TMLE as implemented in the lmtp package
#######################################################

#------------------------------------------------------
#Using LMTP
#------------------------------------------------------
analyse_Q1_lmtp <- function(dta){

    #------------------------------------------------------
    #set data up in the discrete-time wide format needed for lmtp
    #------------------------------------------------------

    dta.tmle<-dta[,c("id","event_time_new","event_type","treat_type",
                              "sex","age","smok_former","smok_current",
                              "diabdur","bmi","hb","hyp","dys","cvd","kidney","panc")]
    dta.tmle$event_type<-ifelse(dta.tmle$event_type==3,0,dta.tmle$event_type)

    dta.tmle.split<-survSplit(Surv(event_time_new/365.25,as.factor(event_type))~.,data=dta.tmle,cut=seq(0,5,0.1))
    dta.tmle.split$event<-ifelse(dta.tmle.split$event=="censor",0,dta.tmle.split$event)
    dta.tmle.split$event<-ifelse(dta.tmle.split$event==2,1,dta.tmle.split$event)
    dta.tmle.split$event<-ifelse(dta.tmle.split$event==3,2,dta.tmle.split$event)

    dta.tmle.split$Y<-ifelse(dta.tmle.split$event==1,1,0)
    dta.tmle.split$D<-ifelse(dta.tmle.split$event==2,1,0)
    dta.tmle.split$C<-ifelse(dta.tmle.split$event==0,1,0)

    dta.tmle.split$tstart<-NULL
    dta.tmle.split$tstop<-NULL
    dta.tmle.split$event<-NULL

    dta.tmle.split<-dta.tmle.split%>%group_by(id)%>%mutate(time=row_number())

    dta.tmle.split<-as.data.frame(dta.tmle.split)

    dta.wide <- pivot_wider(dta.tmle.split, names_from = "time", 
                            values_from = c("C","Y","D"),names_sep=".",names_sort=F)

    maxtime<-max(dta.tmle.split$time)
    dta.wide<-dta.wide[,c("id","treat_type","sex","age","smok_former","smok_current",
                          "diabdur","bmi","hb","hyp","dys","cvd","kidney","panc",
                          paste(rep(c("C","D","Y"),maxtime),rep(c(1:maxtime),each=3),sep="."))]

    paste(rep(c("C","D","Y"),maxtime),rep(c(1:maxtime),each=3),sep=".")

    #if Y_k=1 or D_k=1 then C_[k+1]=NA, for all k
    #This is already the case

    #if Y_k=1 or D_k=1 then C_[k]=1, for all k
    for(k in 1:(maxtime-1)){
        eval(parse(text=paste0("dta.wide$C.",k,"=ifelse(dta.wide$Y.",k,"==1 & dta.wide$D.",k,"==0 & 
  !is.na(dta.wide$Y.",k,")  & !is.na(dta.wide$D.",k,"),
                         1,dta.wide$C.",k,")")))
        
        eval(parse(text=paste0("dta.wide$C.",k,"=ifelse(dta.wide$Y.",k,"==0 & dta.wide$D.",k,"==1 & 
  !is.na(dta.wide$Y.",k,")  & !is.na(dta.wide$D.",k,"),
                         1,dta.wide$C.",k,")")))
    }

    #if Y_k=1 and D_k=0 then Y_[k+1]=1 and D_[k+1]=0, for all k
    #if Y_k=0 and D_k=1 then Y_[k+1]=0 and D_[k+1]=1, for all k
    for(k in 1:(maxtime-1)){
        eval(parse(text=paste0("dta.wide$Y.",k+1,"=ifelse(dta.wide$Y.",k,"==1 & dta.wide$D.",k,"==0 & 
  !is.na(dta.wide$Y.",k,")  & !is.na(dta.wide$D.",k,"),
                         dta.wide$Y.",k,",dta.wide$Y.",k+1,")")))
        
        
        eval(parse(text=paste0("dta.wide$D.",k+1,"=ifelse(dta.wide$Y.",k,"==1 & dta.wide$D.",k,"==0 & 
  !is.na(dta.wide$Y.",k,")  & !is.na(dta.wide$D.",k,"),
                         dta.wide$D.",k,",dta.wide$D.",k+1,")")))
        
        eval(parse(text=paste0("dta.wide$Y.",k+1,"=ifelse(dta.wide$Y.",k,"==0 & dta.wide$D.",k,"==1 &
  !is.na(dta.wide$Y.",k,")  & !is.na(dta.wide$D.",k,"),
                         dta.wide$Y.",k,",dta.wide$Y.",k+1,")")))
        
        eval(parse(text=paste0("dta.wide$D.",k+1,"=ifelse(dta.wide$Y.",k,"==0 & dta.wide$D.",k,"==1 &
  !is.na(dta.wide$Y.",k,")  & !is.na(dta.wide$D.",k,"),
                         dta.wide$D.",k,",dta.wide$D.",k+1,")")))
    }

    #if (C_k=1, Y_k=0, D_k=0) and C_[k+1]=NA then set C_k=0, for all k
    for(k in 1:(maxtime-1)){
        eval(parse(text=paste0("dta.wide$C.",k,"=ifelse(dta.wide$Y.",k,"==0 & dta.wide$D.",k,"==0 & dta.wide$C.",k,"==1 &
  !is.na(dta.wide$Y.",k,")  & !is.na(dta.wide$D.",k,")  & is.na(dta.wide$C.",k+1,"),
                         0,dta.wide$C.",k,")")))
    }

    #if C_k=0 (i.e censored) then set Y_k=NA and D_k=NA, for all k
    for(k in 1:maxtime){
        eval(parse(text=paste0("dta.wide$Y.",k,"=ifelse(dta.wide$C.",k,"==0 & !is.na(dta.wide$C.",k,"),
                         NA,dta.wide$Y.",k,")")))
        
        eval(parse(text=paste0("dta.wide$D.",k,"=ifelse(dta.wide$C.",k,"==0 & !is.na(dta.wide$C.",k,"),
                         NA,dta.wide$D.",k,")")))
    }

    #if C_k=0 (i.e censored) then set C_[k+1]=0, for all k
    for(k in 1:(maxtime-1)){
        eval(parse(text=paste0("dta.wide$C.",k+1,"=ifelse(dta.wide$C.",k,"==0 & !is.na(dta.wide$C.",k,"),
                         0,dta.wide$C.",k+1,")")))
    }
    

    maxtime <- 50

    output <- do.call("rbind",lapply(c("A","B","C","D"),function(trt.fix){

    policy.trt <- eval(parse(text = paste0("function(data, trt) { rep(\"",trt.fix,"\",length(data[[trt]])) }" )))

    lmtp.mace<-lmtp_survival(
        data = dta.wide,
        trt = "treat_type",
        cens = paste0("C.", 1:maxtime),
        compete = paste0("D.", 1:maxtime),
        baseline = c("sex","age","smok_former","smok_current",
                     "diabdur","bmi","hb","hyp","dys","cvd","kidney","panc"),
        outcome = paste0("Y.", 1:maxtime),
        shift = policy.trt,
        folds = 1,
        estimator = "lmtp_tmle"
    )

    lmtp.death<-lmtp_survival(
        data = dta.wide,
        trt = "treat_type",
        cens = paste0("C.", 1:maxtime),
        compete = paste0("Y.", 1:maxtime),
        baseline = c("sex","age","smok_former","smok_current",
                     "diabdur","bmi","hb","hyp","dys","cvd","kidney","panc"),
        outcome = paste0("D.", 1:maxtime),
        shift = policy.trt,
        folds = 1,
        estimator = "lmtp_tmle"
    )

    #save estimates

    tidylmtp.mace<-tidy(lmtp.mace)
    tidylmtp.death<-tidy(lmtp.death)
        out = rbind(tibble(treatment = trt.fix,
                           event = "MACE",
                           estimate = 1-c(1,tidylmtp.mace$estimate),
                           lower = 1-c(1,tidylmtp.mace$conf.high),
                           upper = 1-c(1,tidylmtp.mace$conf.low)),
                    tibble(treatment = trt.fix,
                           event = "Death",
                           estimate = 1-c(1,tidylmtp.death$estimate),
                           lower = 1-c(1,tidylmtp.death$conf.high),
                           upper = 1-c(1,tidylmtp.death$conf.low)))
    }))
    output
}



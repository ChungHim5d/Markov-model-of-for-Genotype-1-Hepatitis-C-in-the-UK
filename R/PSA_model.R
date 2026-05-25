library(tidyverse)
library(readxl)

lifetable = read_xlsx("data/life_table.xlsx")

#name of the states

states = c("Mild", "Moderate", "Compensated", "Decompensated", "HCC",
           "LT", "PLT", "SVRcc", "LRD", "All_cause_death", "SVRmi","SVRmo", "Checks",
           "Discounted_cost", "Discounted_QALYs")

#initialise a matrix: rows = cycles, cols =states
trace = matrix(0,
               nrow = 31,
               ncol = length(states),
               dimnames= list(cycle = 0:30, state = states))

#the inistals at cycle 0
Initials = list(
  "Mild" = 322,
  "Moderate" = 42,
  "Compensated" = 33
)

trace["0", "Mild"] = Initials$Mild
trace["0", "Moderate"] = Initials$Moderate
trace["0", "Compensated"] = Initials$Compensated
trace["0", "Checks"] = sum(trace[,1:12])




last_cycle_number=30


strategy = "PSA"

cost_discount = 0.035
qalys_discount = 0.035

nsim=1000






# ============================================================================
# PARAMETER SAMPLER  —  draws ONE joint set of PSA parameters
# ============================================================================
draw_psa_params = function(){
  list(
    te= treatment_efficacy %>% 
      mutate(PSA_value = rbeta(n(), alpha,beta)),
    
    
    sc = health_costs %>% 
      mutate(PSA_value = ifelse( !Name %in% c("cost_DAA", "cost_PR"),
                                 rgamma(n(), shape = shape, scale = Value / shape),
                                 Value)),
    
    u = health_utilities %>% 
      mutate(PSA_value = rbeta(n(), alpha,beta)),
    
    tp = transition_probs %>% 
      mutate(PSA_value = ifelse(!TransP %in% c("tpHCC2LT", "tpLT2PLT"),
                                suppressWarnings(rbeta(n(), alpha, beta)),
                                Value)) %>%
      mutate(PSA_value = ifelse(TransP == "tpLT2PLT",
                                1 - PSA_value[TransP == "tpLT2LRD"],
                                PSA_value))
  )
}

# ============================================================================
# Run markov, one arm under one parameter set 
# ============================================================================

run_markov_once = function(treatment_type,
                           params            = NULL, #if null, it means we are running deterministic model 
                           last_cycle_number = 30,
                           cost_discount     = 0.035,
                           qalys_discount    = 0.035) {
  
  #------------------------------------------
  #use deterministic value if params is null 
  
  if (is.null(params)) {
    te_df = treatment_efficacy %>% filter(Treatment == treatment_type) %>%
      mutate(use = Value)
    sc_df = health_costs     %>% mutate(use = Value)
    u_df = health_utilities %>% mutate(use = Value)
    tp_df = transition_probs %>% mutate(use = Value)
  } else {
    te_df = params$te %>% filter(Treatment == treatment_type) %>%
      mutate(use = PSA_value)
    sc_df = params$sc %>% mutate(use = PSA_value)
    u_df = params$u %>% mutate(use = PSA_value)
    tp_df = params$tp %>% mutate(use = PSA_value)
  }
  
  #----------------------------------------
  #named vectors
  
  te = setNames(te_df$use, te_df$TransP)
  sc = setNames(sc_df$use, sc_df$Cost)
  u  = setNames(u_df$use,  u_df$Utility)
  tp = setNames(tp_df$use, tp_df$TransP)
  
  
  #----------------------------------------
  # treatment-specific drug cost
  
  tc = if (treatment_type == "DAA") {
    health_costs$Value[health_costs$Cost == "cDAA"]
  } else {
    health_costs$Value[health_costs$Cost == "cPR"]
  }
  
  #-------------------------------------------
  # treatment-specific SVR rates 
  
  treatment_SVRmi = te[paste0(treatment_type, "Mi2SVRmi")]
  treatment_SVRmo = te[paste0(treatment_type, "Mo2SVRmo")]
  treatment_SVRcc = te[paste0(treatment_type, "CC2SVRcc")]
  
  #------------------------------------------------------------------
  trace = matrix(
    0,
    nrow = 31,
    ncol = length(states),
    dimnames = list(cycle = 0:30, state = states)
  )
  
  trace["0", "Mild"] = Initials$Mild
  trace["0", "Moderate"] = Initials$Moderate
  trace["0", "Compensated"] = Initials$Compensated
  trace["0", "Checks"] = sum(trace["0", 1:12])
  

 last_cycle = as.character(last_cycle_number)
 
 
 # ---- markov model
 
 for (i in 0:29){

 
 i_current = as.character(i)
 i_next = as.character(i+1)
 
 last_cycle = as.character(last_cycle_number)
 
 #-----------------------------------------------------------------------------------------------------------
 #Mild
 trace[i_next, "Mild"] = (trace[i_current, "Mild"]*(1-treatment_SVRmi))*(1-tp["tpMi2Mo"]-lifetable[[i+1,2]])
 
 #-----------------------------------------------------------------------------------------------------------
 #Moderate
 trace[i_next, "Moderate"] = trace[i_current, "Moderate"]*(1-treatment_SVRmo)*(1-tp["tpMo2CC"]-lifetable[[i+1,2]])+
   trace[i_current, "Mild"]*(1-treatment_SVRmi)*tp["tpMi2Mo"]
 #-----------------------------------------------------------------------------------------------------------
 #Compensated
 trace[i_next, "Compensated"] = trace[i_current, "Compensated"]*(1-treatment_SVRcc)*(1-tp["tpCC2DC"]-tp["tpCC2HCC"]-lifetable[[i+1,2]])+
   (trace[i_current, "Moderate"]*(1-treatment_SVRmo)*tp["tpMo2CC"])
 #-----------------------------------------------------------------------------------------------------------
 #Decompensated
 trace[i_next, "Decompensated"] = trace[i_current, "Decompensated"]*(1-tp["tpDC2HCC"]-tp["tpDC2LRD"]-tp["tpDC2LT"])+
   (trace[i_current, "Compensated"]*(1-treatment_SVRcc)*tp["tpCC2DC"])+
   (trace[i_current, "SVRcc"]*tp["tpSVRcc2DC"])
 #-----------------------------------------------------------------------------------------------------------
 #HCC
 trace[i_next, "HCC"] = trace[i_current, "HCC"]*(1-tp["tpHCC2LT"]-tp["tpHCC2LRD"])+
   trace[i_current, "Decompensated"]*tp["tpDC2HCC"]+
   (trace[i_current, "Compensated"]*(1-treatment_SVRcc)*tp["tpCC2HCC"])
 #-----------------------------------------------------------------------------------------------------------
 #LT
 trace[i_next, "LT"] =  trace[i_current, "Decompensated"]*tp["tpDC2LT"]+
   trace[i_current, "HCC"]*tp["tpHCC2LT"]
 #-----------------------------------------------------------------------------------------------------------
 #PLT
 trace[i_next, "PLT"] = trace[i_current, "PLT"]*(1-tp["tpPLT2LRD"])+
   trace[i_current, "LT"]*(tp[ "tpLT2PLT"])
 #-----------------------------------------------------------------------------------------------------------
 #SVRcc
 trace[i_next, "SVRcc"] = trace[i_current, "SVRcc"]*(1-tp["tpSVRcc2DC"]-lifetable[[i+1,2]])+
   trace[i_current, "Compensated"]*treatment_SVRcc
 #-----------------------------------------------------------------------------------------------------------
 #LRD
 trace[i_next, "LRD"] =   trace[i_current, "LRD"]+
   trace[i_current, "Decompensated"]*tp["tpDC2LRD"]+
   trace[i_current, "HCC"]*tp["tpHCC2LRD"]+
   trace[i_current, "PLT"]*tp["tpPLT2LRD"]+
   trace[i_current, "LT"]*tp["tpLT2LRD"]
 #-----------------------------------------------------------------------------------------------------------
 #All cause death
 trace[i_next, "All_cause_death"] = trace[i_current, "All_cause_death"]+
   lifetable[[i+1,2]]*(
     trace[i_current, "Mild"]*(1-treatment_SVRmi)+
       trace[i_current, "Moderate"]*(1-treatment_SVRmo)+
       trace[i_current, "Compensated"]*(1-treatment_SVRcc)+
       trace[i_current, "SVRcc"]+
       trace[i_current, "SVRmi"]+
       trace[i_current, "SVRmo"])
 #-----------------------------------------------------------------------------------------------------------
 #SVRmi
 trace[i_next, "SVRmi"] = trace[i_current, "SVRmi"]*(1-lifetable[[i+1,2]])+
   trace[i_current, "Mild"]*treatment_SVRmi
 #-----------------------------------------------------------------------------------------------------------
 #SVRmo
 trace[i_next, "SVRmo"] = trace[i_current, "SVRmo"]*(1-lifetable[[i+1,2]])+
   trace[i_current, "Moderate"]*treatment_SVRmo
 #-----------------------------------------------------------------------------------------------------------
 #Checks
 trace[i_next,"Checks"] = sum(trace[i_next, 1:12])
 #-----------------------------------------------------------------------------------------------------------
 #Discounted cost
 trace[i_current,"Discounted_cost"] = (trace[i_current,"Mild"]*(tc+sc["cMild"])+
                                         trace[i_current,"Moderate"]*(tc+sc["cModerate"])+
                                         trace[i_current,"Compensated"]*(tc+sc["cCom"])+
                                         trace[i_current,"Decompensated"]*(sc["cDecom"])+
                                         trace[i_current,"HCC"]*(sc["cHCC"])+
                                         trace[i_current,"LT"]*(sc["cLT"]+sc["cLTC"])+
                                         trace[i_current, "PLT"]*(sc["cPLT"])+
                                         (trace[i_current, "SVRcc"]+trace[i_current, "SVRmi"]+trace[i_current, "SVRmo"])*sc["cSVR"])/       
                                         (1+cost_discount)^i
 
 
 
 
   trace[last_cycle,"Discounted_cost"] =  (trace[last_cycle,"Mild"]*(tc+sc["cMild"])+
                                           trace[last_cycle,"Moderate"]*(tc+sc["cModerate"])+
                                           trace[last_cycle,"Compensated"]*(tc+sc["cCom"])+
                                           trace[last_cycle,"Decompensated"]*(sc["cDecom"])+
                                           trace[last_cycle,"HCC"]*(sc["cHCC"])+
                                           trace[last_cycle,"LT"]*(sc["cLT"]+sc["cLTC"])+
                                           trace[last_cycle, "PLT"]*(sc["cPLT"])+
                                           (trace[last_cycle, "SVRcc"]+trace[last_cycle, "SVRmi"]+trace[last_cycle, "SVRmo"])*sc["cSVR"])/   #caution here
                                            (1+cost_discount)^as.numeric(last_cycle)
   
   #-----------------------------------------------------------------------------------------------------------
   #discounted qalys
 
 if (i_current == "0"){
   
   trace[i_current,"Discounted_QALYs"] = 0
   
 } else {
   
   trace[i_current,"Discounted_QALYs"] = (trace[i_current, "Mild"]*u["uMild"]+
                                            trace[i_current, "Moderate"]*u["uModerate"]+
                                            trace[i_current, "Compensated"]*u["uCom"]+
                                            trace[i_current, "Decompensated"]*u["uDecom"]+
                                            trace[i_current, "HCC"]*u["uHCC"]+
                                            trace[i_current, "LT"]*u["uLT"]+
                                            trace[i_current, "PLT"]*u["uPLT"]+
                                            trace[i_current, "SVRmi"]*u["uSVRmi"]+
                                            trace[i_current, "SVRmo"]*u["uSVRmo"]+
                                            trace[i_current, "SVRcc"]*u["uSVRcc"])/
                                            (1+qalys_discount)^i
 }
 
 trace[last_cycle,"Discounted_QALYs"] = (trace[last_cycle, "Mild"]*u["uMild"]+
                                           trace[last_cycle, "Moderate"]*u["uModerate"]+
                                           trace[last_cycle, "Compensated"]*u["uCom"]+
                                           trace[last_cycle, "Decompensated"]*u["uDecom"]+
                                           trace[last_cycle, "HCC"]*u["uHCC"]+
                                           trace[last_cycle, "LT"]*u["uLT"]+
                                           trace[last_cycle, "PLT"]*u["uPLT"]+
                                           trace[last_cycle, "SVRmi"]*u["uSVRmi"]+
                                           trace[last_cycle, "SVRmo"]*u["uSVRmo"]+
                                           trace[last_cycle, "SVRcc"]*u["uSVRcc"])/
                                           (1+qalys_discount)^as.numeric(last_cycle)
 #-----------------------------------------------------------------------------------------------------------
 }
 
return(trace)
                     
}


#==============================================================================================
# run paired PSA
#============================================================================================== 
 run_paired_psa = function(
    nsim = 1000,
    last_cycle_number = 30,
    cost_discount = 0.035,
    qalys_discount = 0.035){
   
   PR_list = vector("list", nsim)
   DAA_list = vector("list", nsim)
   
   CE_result = data.frame(
     sim = 1:nsim,
     PR_Cost = NA_real_,
     PR_QALYs = NA_real_,
     DAA_Cost = NA_real_,
     DAA_QALYs = NA_real_
   )
   
   for (n in 1:nsim) {
     params = draw_psa_params()
     
     PR_trace = run_markov_once(
       treatment_type = "PR",
       params = params,
       last_cycle_number = last_cycle_number,
       cost_discount = cost_discount,
       qalys_discount = qalys_discount
     )
     
     DAA_trace = run_markov_once(
       treatment_type = "DAA",
       params = params,
       last_cycle_number = last_cycle_number,
       cost_discount = cost_discount,
       qalys_discount = qalys_discount
     )
   
     PR_list[[n]] = PR_trace
     DAA_list[[n]] = DAA_trace
     
     CE_result[n, "PR_Cost"] =
       sum(PR_trace[, "Discounted_cost"]) / PR_trace["0", "Checks"]
     
     CE_result[n, "PR_QALYs"] =
       sum(PR_trace[, "Discounted_QALYs"]) / PR_trace["0", "Checks"]
     
     CE_result[n, "DAA_Cost"] =
       sum(DAA_trace[, "Discounted_cost"]) / DAA_trace["0", "Checks"]
     
     CE_result[n, "DAA_QALYs"] =
       sum(DAA_trace[, "Discounted_QALYs"]) / DAA_trace["0", "Checks"]
   
   
   
   }
   
     CE_result = CE_result %>%
     mutate(
       Incremental_Cost = DAA_Cost - PR_Cost,
       Incremental_QALYs = DAA_QALYs - PR_QALYs,
       ICER = Incremental_Cost / Incremental_QALYs
     )
     
     return(
       list(
         PR_list = PR_list,
         DAA_list = DAA_list,
         CE_result = CE_result
       )
     )
 }
 


#===============================================================================
# run the full code 
#===============================================================================


 PSA_result = run_paired_psa(nsim = 1000)
 
 
 CE_result = PSA_result$CE_result
 
 
#===============================================================================
#obtain CEAC
 
 Acceptibility_curve = data.frame(
            ceiling = c(seq(0,1000,100), seq(1500,4500, 500), seq(5000,100000,1000)),
            PR_treatment = NA_real_,
            DAA_treatment = NA_real_
 )
 
 
 

 
 for(c in 1:nrow(Acceptibility_curve)){
   
 ceiling_ratio =  Acceptibility_curve[c, "ceiling"]
 
 CE_check = tibble(
 NMB_PR = CE_result$PR_QALYs*ceiling_ratio-CE_result$PR_Cost,
 NMB_DAA = CE_result$DAA_QALYs*ceiling_ratio-CE_result$DAA_Cost
 ) %>% 
 mutate(
   PR_CE  = ifelse(NMB_PR  == pmax(NMB_PR, NMB_DAA), 1, 0),
   DAA_CE = ifelse(NMB_DAA == pmax(NMB_PR, NMB_DAA), 1, 0)
 ) 
   
 PR_treatment_prob = mean(CE_check$PR_CE)
 DAA_treatment_prob = mean(CE_check$DAA_CE)
 
 Acceptibility_curve[c,"PR_treatment"] = PR_treatment_prob 
 Acceptibility_curve[c,"DAA_treatment"] = DAA_treatment_prob 
 
 }
 
 CE_result %>% 
   summarise(
     mean_PR_Cost = mean(PR_Cost),
     mean_DAA_Cost = mean(DAA_Cost),
     mean_PR_QALYs = mean(PR_QALYs),
     mean_DAA_QALYs = mean(DAA_QALYs),
     mean_incremental_cost = mean(Incremental_Cost),
     mean_incremental_qalys = mean(Incremental_QALYs),
     mean_ICER = mean(Incremental_Cost) / mean(Incremental_QALYs)
   )
 

#===================================================================
#make the plot
#===================================================================






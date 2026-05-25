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


#-------------------
i=0

last_cycle_number=30
treatment_type = "DAA"

cost_discount = 0.035
qalys_discount = 0.035


#-------------------
for (i in 0:29){
  
  
    #========================================================================
    #treatment specification 
    if (treatment_type == "DAA"){
      treatment_efficacy_filtered = treatment_efficacy %>% 
                                filter(Treatment == "DAA")
      
      #treatment specific rate 
      
      treatment_SVRmi = treatment_efficacy_filtered$Value[treatment_efficacy_filtered$TransP == "DAAMi2SVRmi"]
      treatment_SVRmo = treatment_efficacy_filtered$Value[treatment_efficacy_filtered$TransP == "DAAMo2SVRmo"]
      treatment_SVRcc = treatment_efficacy_filtered$Value[treatment_efficacy_filtered$TransP == "DAACC2SVRcc"]
      
      treatment_cost = health_costs$Value[health_costs$Cost == "cDAA"]
      
      
    } else if (treatment_type == "PR") {
      treatment_efficacy_filtered = treatment_efficacy %>% 
        filter(Treatment == "PR")
      
      #treatment specific rate 
      
      treatment_SVRmi = treatment_efficacy_filtered$Value[treatment_efficacy_filtered$TransP == "PRMi2SVRmi"]
      treatment_SVRmo = treatment_efficacy_filtered$Value[treatment_efficacy_filtered$TransP == "PRMo2SVRmo"]
      treatment_SVRcc = treatment_efficacy_filtered$Value[treatment_efficacy_filtered$TransP == "PRCC2SVRcc"]
      
      treatment_cost = health_costs$Value[health_costs$Cost == "cPR"]
    }
  
  
      #========================================================================
      #Markov model
  
      i_current = as.character(i)
      i_next = as.character(i+1)
      last_cycle = as.character(last_cycle_number)
   
      trace[i_next, "Mild"] = (trace[i_current, "Mild"]*(1-treatment_SVRmi))*(1-transition_probs$Value[transition_probs$TransP == "tpMi2Mo"]-lifetable[[i+1,2]])
      
      trace[i_next, "Moderate"] = trace[i_current, "Moderate"]*(1-treatment_SVRmo)*(1-transition_probs$Value[transition_probs$TransP == "tpMo2CC"]-lifetable[[i+1,2]])+
                                  trace[i_current, "Mild"]*(1-treatment_SVRmi)*transition_probs$Value[transition_probs$TransP == "tpMi2Mo"]
      
      trace[i_next, "Compensated"] = trace[i_current, "Compensated"]*(1-treatment_SVRcc)*(1-transition_probs$Value[transition_probs$TransP == "tpCC2DC"]-transition_probs$Value[transition_probs$TransP == "tpCC2HCC"]-lifetable[[i+1,2]])+
                                     (trace[i_current, "Moderate"]*(1-treatment_SVRmo)*transition_probs$Value[transition_probs$TransP == "tpMo2CC"])
      
      trace[i_next, "Decompensated"] = trace[i_current, "Decompensated"]*(1-transition_probs$Value[transition_probs$TransP == "tpDC2HCC"]-transition_probs$Value[transition_probs$TransP == "tpDC2LRD"]-transition_probs$Value[transition_probs$TransP == "tpDC2LT"])+
                                       (trace[i_current, "Compensated"]*(1-treatment_SVRcc)*transition_probs$Value[transition_probs$TransP == "tpCC2DC"])+
                                       (trace[i_current, "SVRcc"]*transition_probs$Value[transition_probs$TransP == "tpSVRcc2DC"])
      
      trace[i_next, "HCC"] = trace[i_current, "HCC"]*(1-transition_probs$Value[transition_probs$TransP ==  "tpHCC2LT"]-transition_probs$Value[transition_probs$TransP ==  "tpHCC2LRD"])+
                             trace[i_current, "Decompensated"]*transition_probs$Value[transition_probs$TransP == "tpDC2HCC"]+
                             (trace[i_current, "Compensated"]*(1-treatment_SVRcc)*transition_probs$Value[transition_probs$TransP == "tpCC2HCC"])
      
      trace[i_next, "LT"] =  trace[i_current, "Decompensated"]*transition_probs$Value[transition_probs$TransP ==  "tpDC2LT"]+
                             trace[i_current, "HCC"]*transition_probs$Value[transition_probs$TransP ==  "tpHCC2LT"]
      
      trace[i_next, "PLT"] = trace[i_current, "PLT"]*(1-transition_probs$Value[transition_probs$TransP ==  "tpPLT2LRD"])+
                             trace[i_current, "LT"]*(transition_probs$Value[transition_probs$TransP ==   "tpLT2PLT"])
                             
      trace[i_next, "SVRcc"] = trace[i_current, "SVRcc"]*(1-transition_probs$Value[transition_probs$TransP ==  "tpSVRcc2DC"]-lifetable[[i+1,2]])+
                               trace[i_current, "Compensated"]*treatment_SVRcc
      
      trace[i_next, "LRD"] =   trace[i_current, "LRD"]+
                               trace[i_current, "Decompensated"]*transition_probs$Value[transition_probs$TransP ==  "tpDC2LRD"]+
                               trace[i_current, "HCC"]*transition_probs$Value[transition_probs$TransP ==  "tpHCC2LRD"]+
                               trace[i_current, "PLT"]*transition_probs$Value[transition_probs$TransP ==  "tpPLT2LRD"]+
                               trace[i_current, "LT"]*transition_probs$Value[transition_probs$TransP ==  "tpLT2LRD"]
                               
      trace[i_next, "All_cause_death"] = trace[i_current, "All_cause_death"]+
                                         lifetable[[i+1,2]]*(
                                         trace[i_current, "Mild"]*(1-treatment_SVRmi)+
                                         trace[i_current, "Moderate"]*(1-treatment_SVRmo)+
                                         trace[i_current, "Compensated"]*(1-treatment_SVRcc)+
                                         trace[i_current, "SVRcc"]+
                                         trace[i_current, "SVRmi"]+
                                         trace[i_current, "SVRmo"])

                                         
      trace[i_next, "SVRmi"] = trace[i_current, "SVRmi"]*(1-lifetable[[i+1,2]])+
                               trace[i_current, "Mild"]*treatment_SVRmi
                               
      trace[i_next, "SVRmo"] = trace[i_current, "SVRmo"]*(1-lifetable[[i+1,2]])+
                               trace[i_current, "Moderate"]*treatment_SVRmo
      
      
      trace[i_next,"Checks"] = sum(trace[i_next, 1:12])
      
      trace[i_current,"Discounted_cost"] = (trace[i_current,"Mild"]*(treatment_cost+health_costs$Value[health_costs$Cost == "cMild"])+
                                            trace[i_current,"Moderate"]*(treatment_cost+health_costs$Value[health_costs$Cost == "cModerate"])+
                                            trace[i_current,"Compensated"]*(treatment_cost+health_costs$Value[health_costs$Cost == "cCom"])+
                                            trace[i_current,"Decompensated"]*(health_costs$Value[health_costs$Cost == "cDecom"])+
                                            trace[i_current,"HCC"]*(health_costs$Value[health_costs$Cost == "cHCC"])+
                                            trace[i_current,"LT"]*(health_costs$Value[health_costs$Cost == "cLT"]+health_costs$Value[health_costs$Cost == "cLTC"])+
                                            trace[i_current, "PLT"]*(health_costs$Value[health_costs$Cost == "cPLT"])+
                                            (trace[i_current, "SVRcc"]+trace[i_current, "SVRmi"]+trace[i_current, "SVRmo"])*health_costs$Value[health_costs$Cost == "cSVR"])/
                                            (1+cost_discount)^i
                                
                                  
         
        
      trace[last_cycle,"Discounted_cost"] =  (trace[last_cycle,"Mild"]*(treatment_cost+health_costs$Value[health_costs$Cost == "cMild"])+
                                              trace[last_cycle,"Moderate"]*(treatment_cost+health_costs$Value[health_costs$Cost == "cModerate"])+
                                              trace[last_cycle,"Compensated"]*(treatment_cost+health_costs$Value[health_costs$Cost == "cCom"])+
                                              trace[last_cycle,"Decompensated"]*(health_costs$Value[health_costs$Cost == "cDecom"])+
                                              trace[last_cycle,"HCC"]*(health_costs$Value[health_costs$Cost == "cHCC"])+
                                              trace[last_cycle,"LT"]*(health_costs$Value[health_costs$Cost == "cLT"]+health_costs$Value[health_costs$Cost == "cLTC"])+
                                              trace[last_cycle, "PLT"]*(health_costs$Value[health_costs$Cost == "cPLT"])+
                                              (trace[last_cycle, "SVRcc"]+trace[last_cycle, "SVRmi"]+trace[last_cycle, "SVRmo"])*health_costs$Value[health_costs$Cost == "cSVR"])/
                                             (1+cost_discount)^as.numeric(last_cycle)
        
      if (i_current == "0"){
        
     trace[i_current,"Discounted_QALYs"] = 0
     
      } else {
        
     trace[i_current,"Discounted_QALYs"] = (trace[i_current, "Mild"]*health_utilities$Value[health_utilities$Utility == "uMild"]+
                                            trace[i_current, "Moderate"]*health_utilities$Value[health_utilities$Utility == "uModerate"]+
                                            trace[i_current, "Compensated"]*health_utilities$Value[health_utilities$Utility == "uCom"]+
                                            trace[i_current, "Decompensated"]*health_utilities$Value[health_utilities$Utility == "uDecom"]+
                                            trace[i_current, "HCC"]*health_utilities$Value[health_utilities$Utility == "uHCC"]+
                                            trace[i_current, "LT"]*health_utilities$Value[health_utilities$Utility == "uLT"]+
                                            trace[i_current, "PLT"]*health_utilities$Value[health_utilities$Utility == "uPLT"]+
                                            trace[i_current, "SVRmi"]*health_utilities$Value[health_utilities$Utility == "uSVRmi"]+
                                            trace[i_current, "SVRmo"]*health_utilities$Value[health_utilities$Utility == "uSVRmo"]+
                                            trace[i_current, "SVRcc"]*health_utilities$Value[health_utilities$Utility == "uSVRcc"])/
                                            (1+qalys_discount)^i
  }
  
      trace[last_cycle,"Discounted_QALYs"] = (trace[last_cycle, "Mild"]*health_utilities$Value[health_utilities$Utility == "uMild"]+
                                               trace[last_cycle, "Moderate"]*health_utilities$Value[health_utilities$Utility == "uModerate"]+
                                               trace[last_cycle, "Compensated"]*health_utilities$Value[health_utilities$Utility == "uCom"]+
                                               trace[last_cycle, "Decompensated"]*health_utilities$Value[health_utilities$Utility == "uDecom"]+
                                               trace[last_cycle, "HCC"]*health_utilities$Value[health_utilities$Utility == "uHCC"]+
                                               trace[last_cycle, "LT"]*health_utilities$Value[health_utilities$Utility == "uLT"]+
                                               trace[last_cycle, "PLT"]*health_utilities$Value[health_utilities$Utility == "uPLT"]+
                                               trace[last_cycle, "SVRmi"]*health_utilities$Value[health_utilities$Utility == "uSVRmi"]+
                                               trace[last_cycle, "SVRmo"]*health_utilities$Value[health_utilities$Utility == "uSVRmo"]+
                                               trace[last_cycle, "SVRcc"]*health_utilities$Value[health_utilities$Utility == "uSVRcc"])/
                                            (1+qalys_discount)^as.numeric(last_cycle)
      
      
      
      
  
}





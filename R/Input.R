setwd("~/R_project/markov model Liver disease")
library(tidyverse)

#transition_probs

transition_probs = data.frame(
  Name = c(
    "mild_2_moderate",
    "moderate_2_cc",
    "cc_2_dc",
    "cc_2_hcc",
    "svrcc_2_dc",
    "dc_2_hcc",
    "dc_2_lt",
    "dc_2_lrd",
    "hcc_2_lt",
    "hcc_2_lrd",
    "lt_2_lrd",
    "lt_2_plt",
    "plt_2_lrd"
  ),
  
  TransP = c(
    "tpMi2Mo",
    "tpMo2CC",
    "tpCC2DC",
    "tpCC2HCC",
    "tpSVRcc2DC",
    "tpDC2HCC",
    "tpDC2LT",
    "tpDC2LRD",
    "tpHCC2LT",
    "tpHCC2LRD",
    "tpLT2LRD",
    "tpLT2PLT",
    "tpPLT2LRD"
  ),
  
  Value = c(
    0.025,
    0.037,
    0.039,
    0.014,
    0.003,
    0.014,
    0.030,
    0.130,
    0.000,
    0.430,
    0.210,
    0.790,
    0.057
  ),
  
  Dist = c(
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "N/A",
    "Beta",
    "Beta",
    "N/A",
    "Beta"
  ),
  
  alpha = c(
    38.0609,
    26.8680,
    14.5778,
    1.9186,
    1976.5905,
    1.9816,
    6.5256,
    146.9000,
    NA,
    116.6733,
    16.2762,
    NA,
    22.9017
  ),
  
  beta = c(
    1484.3765,
    699.2952,
    359.2122,
    135.1214,
    589816.9695,
    135.1214,
    210.9945,
    983.1000,
    NA,
    154.6600,
    61.2294,
    NA,
    378.8825
  )
)

#Treatment efficacy
treatment_efficacy = data.frame(
  Name = c(
    "daa_mild_2_svrmi",
    "daa_moderate_2_svrmo",
    "daa_cc_2_svrcc",
    "pr_mild_2_svrmi",
    "pr_moderate_2_svrmo",
    "pr_cc_2_svrcc"
  ),
  
  Treatment = c(
    "DAA",
    "DAA",
    "DAA",
    "PR",
    "PR",
    "PR"
  ),
  
  TransP = c(
    "DAAMi2SVRmi",
    "DAAMo2SVRmo",
    "DAACC2SVRcc",
    "PRMi2SVRmi",
    "PRMo2SVRmo",
    "PRCC2SVRcc"
  ),
  
  Value = c(
    0.9985,
    0.9985,
    0.9704,
    0.48,
    0.40,
    0.25
  ),
  
  Dist = c(
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta"
  ),
  
  alpha = c(
    7,
    7,
    15,
    145,
    32,
    11
  ),
  
  beta = c(
    2,
    2,
    12,
    97,
    31,
    25
  )
)



#health states cos

health_costs = data.frame(
  Name = c(
    "cost_mild_hcv",
    "cost_moderate_hcv",
    "cost_cc",
    "cost_dc",
    "cost_hcc",
    "cost_lt",
    "cost_ltc",
    "cost_plt",
    "cost_svr",
    "cost_DAA",
    "cost_PR"
  ),
  
  Cost = c(
    "cMild",
    "cModerate",
    "cCom",
    "cDecom",
    "cHCC",
    "cLT",
    "cLTC",
    "cPLT",
    "cSVR",
    "cDAA",
    "cPR"
  ),
  
  Value = c(
    201.35,
    1046.15,
    1660.41,
    13306.63,
    11857.78,
    39876.12,
    13799.79,
    2020.80,
    377.90,
    39635.63,
    16543.00
    
  ),
  
  Dist = c(
    "Gamma",
    "Gamma",
    "Gamma",
    "Gamma",
    "Gamma",
    "Gamma",
    "Gamma",
    "Gamma",
    "Gamma",
    "NA",
    "NA"
  ),
  
  shape = c(
    25.6995,
    88.8502,
    24.2342,
    36.0249,
    18.1081,
    89.7536,
    13.7788,
    15.2189,
    28.8141,
    0,
    0
  ),
  
  scale = c(
    5.3698,
    8.0698,
    46.9584,
    253.1582,
    448.8045,
    304.5004,
    686.4168,
    91.0053,
    8.9887,
    0,
    0
  )
)


#health states utility


health_utilities <- data.frame(
  Name = c(
    "utility_mild_hcv",
    "utility_moderate_hcv",
    "utility_cc",
    "utility_dc",
    "utility_hcc",
    "utility_lt",
    "utility_plt",
    "utility_svrmi",
    "utility_svrmo",
    "utility_svrcc"
  ),
  
  Utility = c(
    "uMild",
    "uModerate",
    "uCom",
    "uDecom",
    "uHCC",
    "uLT",
    "uPLT",
    "uSVRmi",
    "uSVRmo",
    "uSVRcc"
  ),
  
  Value = c(
    0.77,
    0.66,
    0.55,
    0.45,
    0.45,
    0.45,
    0.67,
    0.82,
    0.72,
    0.61
  ),
  
  Dist = c(
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta",
    "Beta"
  ),
  
  alpha = c(
    521.2375,
    115.7060,
    47.1021,
    123.7500,
    123.7500,
    123.7500,
    59.2548,
    65.8678,
    58.0608,
    58.0476
  ),
  
  beta = c(
    155.6943,
    59.6063,
    38.5381,
    151.2500,
    151.2500,
    151.2500,
    29.1825,
    14.4588,
    22.5792,
    37.1124
  )
)








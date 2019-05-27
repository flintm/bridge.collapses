# Names and variable names for correlation and linear regression
# Copyright Madeleine Flint, 2016
Bases <- function(TYPE){
 # FAILURE RELATED ------------------------------------------------------------
  if (TYPE == "FAILQ"){
    bases <- c("USGSdUSGSp",                 "USGSdUSGSi",                 "USGSdUSGSip",                  "USGSdVICGd",             "USGSipVICGd",
               "USGSdVICgdPM2",              "USGSipVICGdPM2", 
               "USGSdNLDASgPM2",             "USGSipNLDASgPM2",            "USGSdNLDASbPM2",               "USGSipNLDASbPM2",
               "VICGdPM2NLDASGdPM2",         "VICGdPM2NLDASBdPM2",         "VICBdPM2NLDASGdPM2",           "VICBdPM2NLDASBdPM2")
    T1ls  <- c("Q_FAIL_D_USGS",                     "Q_FAIL_D_USGS",                     "Q_FAIL_D_USGS",                       "Q_FAIL_D_USGS",                 "Q_FAIL_IP_USGS",
               "Q_FAIL_D_USGS",                     "Q_FAIL_IP_USGS",
               "Q_FAIL_D_USGS",                     "Q_FAIL_IP_USGS",           "Q_FAIL_D_USGS",                       "Q_FAIL_IP_USGS",
               "Q_FAILPM2_DVICG", "Q_FAILPM2_DVICG", "Q_FAILPM2_DVICB", "Q_FAILPM2_DVICB")
    T2ls  <- c("Q_FAIL_P_USGS",                "Q_FAIL_I_USGS",                "Q_FAIL_IP_USGS",             "Q_FAIL_DVICG", "Q_FAIL_DVICG",
               "Q_FAILPM2_DVICG", "Q_FAILPM2_DVICG",
               "Q_FAILPM2_NLDASG",      "Q_FAILPM2_NLDASG",      "Q_FAILPM2_NLDASB",      "Q_FAILPM2_NLDASB",
               "Q_FAILPM2_NLDASG",      "Q_FAILPM2_NLDASB",    "Q_FAILPM2_NLDASG",        "Q_FAILPM2_NLDASB")   
  }
  
  if (TYPE == "USGS"){
    bases <- c("HECdHECi",                 "HECdHECp",               "HECiHECp",                    "HECdHECip",             
               "HECdPartInterpd",        "HECipPartInterpd",            "PartInterpdPartInterpip",         
               "HECdPartInterpip",                                   "HECipPartInterpip",
               "PKFQdPKFQp",              "HECdPKFQd",              "HECipPKFQp")
    T1ls  <- c("T_FAIL_D_HECD_USGS",           "T_FAIL_D_HECD_USGS",         "T_FAIL_I_HECP_USGS",              "T_FAIL_D_HECD_USGS",            
               "T_FAIL_D_HECD_USGS",         "T_FAIL_IP_HECP_USGS",         "T_FAIL_D_PARTDUR_USGS",            
               "T_FAIL_D_HECD_USGS",                                     "T_FAIL_IP_HECP_USGS",
               "T_FAIL_D_PKFQD_USGS",          "T_FAIL_D_HECD_USGS",         "T_FAIL_IP_HECP_USGS")
    T2ls  <- c("T_FAIL_I_HECP_USGS",          "T_FAIL_P_HECP_USGS",        "T_FAIL_P_HECP_USGS",               "T_FAIL_IP_HECP_USGS",  
               "T_FAIL_D_PARTDUR_USGS",  "T_FAIL_D_PARTDUR_USGS",         "T_FAIL_IP_PARTDUR_USGS", 
               "T_FAIL_IP_PARTDUR_USGS",                    "T_FAIL_IP_PARTDUR_USGS",
               "T_FAIL_P_PKFQP_USGS",         "T_FAIL_D_PKFQD_USGS",        "T_FAIL_P_PKFQP_USGS")
  }
 
 
  if (TYPE == "USGS-VICG"){
    bases <- c("HECdHECd",                       "HECdHECdPM2",                    "HECipHECdPM2",
               "PKFQdVICgPKFQd",                  "PKFQdVICgPKFQdPM2",                 "PKFQipVICgPKFQdPM2",
               "PKFQdVICbPKFQd",                  "PKFQdVICbPKFQdPM2",                 "PKFQipVICbPKFQdPM2")
    T1ls  <- c("T_FAIL_D_HECD_USGS",                 "T_FAIL_D_HECD_USGS",                 "T_FAIL_IP_HECP_USGS",
               "T_FAIL_D_PKFQD_USGS",                 "T_FAIL_D_PKFQD_USGS",                   "T_FAIL_P_PKFQP_USGS",
               "T_FAIL_D_PKFQD_USGS",                 "T_FAIL_D_PKFQD_USGS",                   "T_FAIL_P_PKFQP_USGS")
    T2ls  <- c("T_FAIL_HEC_DVICG",     "T_FAILPM2_HEC_DVICG", "T_FAILPM2_HEC_DVICG",     
               "T_FAIL_PKFQD_DVICG",     "T_FAILPM2_PKFQD_DVICG",   "T_FAILPM2_PKFQD_DVICG",     
               "T_FAIL_PKFQD_DVICB",   "T_FAILPM2_PKFQD_DVICB", "T_FAILPM2_PKFQD_DVICB")
  }
  
  if (TYPE == "USGS-VICGRapid"){
    bases <- c("USGSdVICRgd",                      "USGSdVICRgdPM2",                       "USGSipVICRgdPM2",
               "USGSdVICRgy",
                "PKFQdVICRgPKFQd",                  "PKFQdVICRgPKFQdPM2",                 "PKFQipVICRgPKFQdPM2",
               "PKFQdVICRgPKFQy",                  "PKFQipVICRgPKFQy")
    T1ls  <- c("Q_FAIL_D_USGS",                    "Q_FAIL_D_USGS",                     "Q_FAIL_IP_USGS",
               "Q_FAIL_D_USGS",
               "T_FAIL_D_PKFQD_USGS",                 "T_FAIL_D_PKFQD_USGS",                   "T_FAIL_P_PKFQP_USGS",
               "T_FAIL_D_PKFQD_USGS",                 "T_FAIL_P_PKFQP_USGS")
    T2ls  <- c("Q_FAIL_DVICG_RAPID",              "Q_FAILPM2_DVICG_RAPID",             "Q_FAILPM2_DVICG_RAPID",
               "Q_FAILYRMAX_DVICG_RAPID",
              "T_FAIL_PKFQD_DVICG_RAPID",     "T_FAILPM2_PKFQD_DVICG_RAPID",   "T_FAILPM2_PKFQD_DVICG_RAPID",     
               "T_FAILYRMAX_PKFQD_DVICG_RAPID",   "T_FAILYRMAX_PKFQD_DVICG_RAPID")
  }
  
 
  if (TYPE == "USGS-BEST-VICg"){
    bases <- c("HECipHECy",                              "HECdHECy",
               "PKFQdVICgPKFQy",                     "PKFQdVICbPKFQy")
    T1ls  <- c("T_FAIL_IP_HECP_USGS",                       "T_FAIL_D_HECD_USGS",
               "T_FAIL_D_HECD_USGS",                        "T_FAIL_D_HECD_USGS")
    T2ls  <- c("T_FAILYRMAX_HEC_DVICG",     "T_FAILYRMAX_HEC_DVICG",
               "T_FAILYRMAX_PKFQD_DVICG",   "T_FAILYRMAX_PKFQD_DVICB")
  }
  
  if (TYPE == "USGS-PART-VICg-PART"){
    bases <- c("USGSpartVICGpart",            "USGSpartVICBpart",                "USGSpartVICGpartPM2",           "USGSpartVICBpartPM2", "USGSpartVICGpartMaxYr", "USGSpartVICBpartMaxYr",
               "VICGpartVICBpart",            "VICGpartPM2VICBpartPM2",          "VICGpartVICGpartPM2",           "VICBpartVICBpartPM2")
    T1ls  <- c("T_FAIL_D_PARTDUR_USGS",       "T_FAIL_D_PARTDUR_USGS",           "T_FAIL_D_PARTDUR_USGS",         "T_FAIL_D_PARTDUR_USGS", "T_FAIL_D_PARTDUR_USGS", "T_FAIL_D_PARTDUR_USGS",           
               "T_FAIL_PARTDUR_DVICG",   "T_FAILPM2_PARTDUR_DVICG",   "T_FAIL_PARTDUR_DVICG",     "T_FAIL_PARTDUR_DVICB")
    T2ls  <- c("T_FAIL_PARTDUR_DVICG",   "T_FAIL_PARTDUR_DVICB",     "T_FAILPM2_PARTDUR_DVICG", "T_FAILPM2_PARTDUR_DVICB", "T_FAILYRMAX_PARTDUR_DVICG", "T_FAILYRMAX_PARTDUR_DVICB",          
               "T_FAIL_PARTDUR_DVICB", "T_FAILPM2_PARTDUR_DVICB", "T_FAILPM2_PARTDUR_DVICG", "T_FAILPM2_PARTDUR_DVICB")
  }
  
  if (TYPE == "VICg"){
    bases <- c("HECdPM2partial")
    T1ls <- c("T_FAILPM2_HEC_DVICG")
    T2ls <- c( "T_FAILPM2_PARTDUR_DVICG")
  }
  
  
  if (TYPE == "VICg-VICb"){
    bases <- c("VICgHECdPM2VICbPKFQdPM2",          "VICgPKFQdPM2VICbPKFQdPM2")
    T1ls <- c("T_FAILPM2_HEC_DVICG",    "T_FAILPM2_PKFQD_DVICG")
    T2ls <- c("T_FAILPM2_PKFQD_DVICB", "T_FAILPM2_PKFQD_DVICB")
  }
  
  if (TYPE == "USGS-NLDAS"){
    bases <- c("HECdNLDASgPKFQdPM2",                       "HECipNLDASgPKFQdPM2",           
               "HECdNLDASgPartPM2",                      "HECipNLDASgPartPM2",              "USGSpartNLDASgPartPM2", "USGSpartNLDASgPartMaxYr",
               "HECdNLDASbPKFQdPM2",                      "HECipNLDASbPKFQdPM2",           
               "HECdNLDASbPartPM2",                      "HECipNLDASbPartPM2",              "USGSpartNLDASbPartPM2", "USGSpartNLDASbPartMaxYr",
               "PKFQdNLDASgPKFQdPM2",            "PKFQipNLDASgPKFQdPM2",
               "PKFQdNLDASbPKFQdPM2",            "PKFQipNLDASbPKFQdPM2")
    T1ls <- c("T_FAIL_D_HECD_USGS",                         "T_FAIL_IP_HECP_USGS", 
              "T_FAIL_D_HECD_USGS",                         "T_FAIL_IP_HECP_USGS",            "T_FAIL_D_PARTDUR_USGS", "T_FAIL_D_PARTDUR_USGS",
              "T_FAIL_D_HECD_USGS",                         "T_FAIL_IP_HECP_USGS", 
              "T_FAIL_D_HECD_USGS",                         "T_FAIL_IP_HECP_USGS",            "T_FAIL_D_PARTDUR_USGS", "T_FAIL_D_PARTDUR_USGS",
              "T_FAIL_D_PKFQD_USGS",                "T_FAIL_P_PKFQP_USGS",
              "T_FAIL_D_PKFQD_USGS",                "T_FAIL_P_PKFQP_USGS")
    T2ls <- c("T_FAILPM2_PKFQD_NLDASG",          "T_FAILPM2_PKFQD_NLDASG",
              "T_FAILPM2_PARTDUR_NLDASG",               "T_FAILPM2_PARTDUR_NLDASG",        "T_FAILPM2_PARTDUR_NLDASG", "T_FAILYRMAX_PARTDUR_NLDASG",
              "T_FAILPM2_PKFQD_NLDASB",            "T_FAILPM2_PKFQD_NLDASB",
              "T_FAILPM2_PARTDUR_NLDASB",             "T_FAILPM2_PARTDUR_NLDASB",      "T_FAILPM2_PARTDUR_NLDASB", "T_FAILYRMAX_PARTDUR_NLDASB",
              "T_FAILPM2_PKFQD_NLDASG",     "T_FAILPM2_PKFQD_NLDASG",     
              "T_FAILPM2_PKFQD_NLDASB",   "T_FAILPM2_PKFQD_NLDASB")
  }
 
  
  if (TYPE == "USGS-BEST-NLDAS"){
    bases <- c("HECipNLDASgPKFQy",                        "HECdNLDASgPKFQy",
               "HECipNLDASbPKFQy",                        "HECdNLDASbPKFQy",
               "PKFQdNLDASgPKFQy",                     "PKFQdNLDASbPKFQy")
    T1ls <- c("T_FAIL_IP_HECP_USGS",                       "T_FAIL_D_HECD_USGS",
              "T_FAIL_IP_HECP_USGS",                       "T_FAIL_D_HECD_USGS",
              "T_FAIL_D_PKFQD_USGS",                        "T_FAIL_D_PKFQD_USGS")
    T2ls <- c("T_FAILYRMAX_PKFQD_NLDASG",          "T_FAILYRMAX_PKFQD_NLDASG",
              "T_FAILYRMAX_PKFQD_NLDASB",       "T_FAILYRMAX_PKFQD_NLDASB",
              "T_FAILYRMAX_PKFQD_NLDASG",          "T_FAILYRMAX_PKFQD_NLDASB")
  }
 
 if (TYPE == "NLDASg-NLDASb"){
   bases <- c("NLDASgPKFQdPM2NLDASbPKFQdPM2",  "NLDASgPartPM2NLDASbPartPM2",  "NLDASgPartMaxYrNLDASbPartMaxYr")
   T1ls <- c("T_FAILPM2_PKFQD_NLDASG",     "T_FAILPM2_PARTDUR_NLDASG",   "T_FAILYRMAX_PARTDUR_NLDASG")
   T2ls <- c("T_FAILPM2_PKFQD_NLDASB",   "T_FAILPM2_PARTDUR_NLDASB", "T_FAILYRMAX_PARTDUR_NLDASB")
 }
  
  if (TYPE == "VIC-NLDAS"){
    bases <- c("VICgHECPM2dNLDASgPKFQdPM2",              "VICgPartPM2NLDASgPartPM2", "VICgPartMaxYrNLDASgPartMaxYr",
               "VICgHECPM2dNLDASbPKFQdPM2",              "VICgPartPM2NLDASbPartPM2", "VICbPartMaxYrNLDASbPartMaxYr",
               "VICgHECyNLDASgPKFQy",                    "VICgHECyNLDASbPKFQy",
               "VICgPKFQdNLDASgPKFQdPM2",                  "VICgPKFQdPM2NLDASgPKFQdPM2",     
               "VICbPKFQdNLDASbPKFQdPM2",                  "VICbPKFQdPM2NLDASbPKFQdPM2")
    T1ls <- c("T_FAILPM2_HEC_DVICG",         "T_FAILPM2_PARTDUR_DVICG",  "T_FAILYRMAX_PARTDUR_DVICG",
              "T_FAILPM2_HEC_DVICG",         "T_FAILPM2_PARTDUR_DVICG", "T_FAILYRMAX_PARTDUR_DVICB",
              "T_FAILYRMAX_HEC_DVICG",      "T_FAILYRMAX_HEC_DVICG",
              "T_FAIL_PKFQD_DVICG",            "T_FAILPM2_PKFQD_DVICG",     
              "T_FAIL_PKFQD_DVICB",          "T_FAILPM2_PKFQD_DVICB")
    T2ls <- c("T_FAILPM2_PKFQD_NLDASG",              "T_FAILPM2_PARTDUR_NLDASG",   "T_FAILYRMAX_PARTDUR_NLDASG",
              "T_FAILPM2_PKFQD_NLDASB",            "T_FAILPM2_PARTDUR_NLDASB",  "T_FAILYRMAX_PARTDUR_NLDASB",
              "T_FAILYRMAX_PKFQD_NLDASG",           "T_FAILYRMAX_PKFQD_NLDASB",
              "T_FAILPM2_PKFQD_NLDASG",                 "T_FAILPM2_PKFQD_NLDASG",     
              "T_FAILPM2_PKFQD_NLDASB",               "T_FAILPM2_PKFQD_NLDASB")
  }
 
 # MAX RELATED ------------------------------------------------------------
 if (TYPE == "MAXQ"){
   bases <- c("USGSdUSGSp",            "USGSdVICGd",                 "USGSpVICGd",            
              "USGSdNLDASGd",          "USGSipNLDASGd",              "USGSdNLDASBd",               "USGSipNLDASBd",
              "VICGdNLDASGd",          "VICGdNLDASBd",               "VICBdNLDASGd",               "VICBdNLDASBd")
   T1ls  <- c("Q_MAX_D_USGS",                 "Q_MAX_D_USGS",                      "Q_MAX_P_USGS",            
              "Q_MAX_D_USGS",                 "Q_MAX_P_USGS",                 "Q_MAX_D_USGS",                      "Q_MAX_P_USGS",
              "Q_MAX_DVICG", "Q_MAX_DVICG",      "Q_MAX_DVICB",    "Q_MAX_DVICB")
   T2ls  <- c("Q_MAX_P_USGS",            "Q_MAX_DVICG",      "Q_MAX_DVICG",
              "Q_MAX_NLDASG",      "Q_MAX_NLDASG",           "Q_MAX_NLDASB",         "Q_MAX_NLDASB",
              "Q_MAX_NLDASG",      "Q_MAX_NLDASB",         "Q_MAX_NLDASG",           "Q_MAX_NLDASB")
 } 
 
 if (TYPE == "USGS-TMAX"){
   bases <- c("HECdHECp",          "PKFQdPKFQp",          "HECdPKFQd",              "HECpPKFQp")
   T1ls  <- c("T_MAX_D_HECD_USGS",       "T_MAX_D_PKFQD_USGS",      "T_MAX_D_HECD_USGS",          "T_MAX_P_HECP_USGS")
   T2ls  <- c("T_MAX_P_HECP_USGS",      "T_MAX_P_PKFQP_USGS",     "T_MAX_D_PKFQD_USGS",         "T_MAX_P_PKFQP_USGS")
 }
 
 if (TYPE == "USGS-VIC-TMAX"){
   bases <- c("HECdHECd",                  "HECpHECd",
             "PKFQdVICgPKFQd",            "PKFQpVICbPKFQd")
   T1ls  <- c("T_MAX_D_HECD_USGS",             "T_MAX_P_HECP_USGS",
              "T_MAX_D_PKFQD_USGS",                 "T_MAX_D_PKFQD_USGS")
   T2ls  <- c("T_MAX_HEC_DVICG", "T_MAX_HEC_DVICG",
              "T_MAX_PKFQD_DVICG",          "T_MAX_PKFQD_DVICB")
 }
  
  if (TYPE == "USGS-VICR-TMAX"){
    bases <- c("PKFQdVICRgPKFQd",            "PKFQpVICRgPKFQd")
    T1ls  <- c("T_MAX_D_HECD_USGS",             "T_MAX_P_HECP_USGS")
    T2ls  <- c("T_MAX_PKFQD_DVICG_RAPID",          "T_MAX_PKFQD_DVICG_RAPID")
  }
 
 if (TYPE == "USGS-NLDAS-TMAX"){
   bases <- c("HECdNLDASgPKFQd",                  "HECpNLDASgPKFQd",
              "HECdNLDASbHECd",                  "HECpNLDASbHECd",
              "PKFQdNLDASgPKFQd",                "PKFQdNLDASbPKFQd")
   T1ls  <- c("T_MAX_D_HECD_USGS",                   "T_MAX_P_HECP_USGS",
              "T_MAX_D_HECD_USGS",                   "T_MAX_D_HECD_USGS",
              "T_MAX_D_PKFQD_USGS",                 "T_MAX_D_PKFQD_USGS")
   T2ls  <- c("T_MAX_PKFQD_NLDASG",           "T_MAX_PKFQD_NLDASG",
              "T_MAX_PKFQD_NLDASB",          "T_MAX_PKFQD_NLDASB",
              "T_MAX_PKFQD_NLDASG",          "T_MAX_PKFQD_NLDASB")
 }
 
 
 if (TYPE == "VIC-NLDAS-TMAX"){
   bases <- c("VICgHECdNLDASgPKFQd",               
              "VICgPKFQdNLDASgPKFQd",             "VICbPKFQdNLDASbPKFQd")
   T1ls  <- c("T_MAX_HEC_DVICG",        
              "T_MAX_PKFQD_DVICG",       "T_MAX_PKFQD_DVICB")
   T2ls  <- c("T_MAX_PKFQD_NLDASG",           
              "T_MAX_PKFQD_NLDASG",            "T_MAX_PKFQD_NLDASB")
 }

 
 # RETURN ---------------------------
  ls.Bases <- list(bases = bases, T1ls = T1ls, T2ls = T2ls)
  return(ls.Bases)
}

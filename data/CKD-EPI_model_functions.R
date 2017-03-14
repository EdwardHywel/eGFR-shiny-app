# Functions for CKD-EPI equation, both adjusted and not, also defines vector of
# original paramaters, rmse function, and redundand rmse_forCKDfunction


rmse <- function(x, y)
  sqrt(mean( (x - y)^2, na.rm = TRUE))


CKD_model = function(a, b_F, b_M, c_F, c_M, d, e, f, Sex, Scr, Age){
  # the CKD-EPI model with all parsamaters as variables
  n = length(Age)
  log_GFR = rep(0,n)
  for(i in 1:n){
    if(Sex[i] == "F"){
      log_GFR[i]  <- log(a) + c_F*log(min(Scr[i]/b_F,1)) + 
        d*log(max(Scr[i]/b_F,1)) + Age[i]*log(e) + log(f)
    }
    else if(Sex[i] == "M"){
      log_GFR[i]  <- log(a) + c_M*log(min(Scr[i]/b_M,1)) + 
        d*log(max(Scr[i]/b_M,1)) + Age[i]*log(e)
    }
    else{
      stop("Sex myst be F or M")
    }
  }
  GFR = exp(log_GFR)
  return(GFR)
}  
CKD_model = Vectorize(CKD_model)

CKD_model_vector <- function(Scr, Age, Sex, par){
  a = par[1]; b_F = par[2]; b_M = par[3]; c_F = par[4]; c_M = par[5]; d = par[6]; e = par[7]
  f = par[8]
  CKD_model(a, b_F, b_M, c_F, c_M, d, e, f, Sex, Scr, Age)
} 

CKD_model_adjusted = function(a, b_F, b_M, c_F, c_M, d, e, f, Sex, Scr, Age, BSA){
  # the CKD-EPI model with all parsamaters as variables
  # includinf the adjustment for BSA
  n = length(Age)
  log_GFR = rep(0,n)
  for(i in 1:n){
    if(Sex[i] == "F"){
      log_GFR[i]  <- log(a) + c_F*log(min(Scr[i]/b_F,1)) + 
        d*log(max(Scr[i]/b_F,1)) + Age[i]*log(e) + log(f)
    }
    else if(Sex[i] == "M"){
      log_GFR[i]  <- log(a) + c_M*log(min(Scr[i]/b_M,1)) + 
        d*log(max(Scr[i]/b_M,1)) + Age[i]*log(e)
    }
    else{
      stop("Sex myst be F or M")
    }
  }
  GFR = exp(log_GFR)
  adjGFR = GFR*BSA/1.73
  return(adjGFR)
}  
CKD_model_adjusted = Vectorize(CKD_model_adjusted)


CKD_model_adjusted_vector <- function(Scr, Age, Sex, BSA, par){
  a = par[1]; b_F = par[2]; b_M = par[3]; c_F = par[4]; c_M = par[5]; d = par[6]; e = par[7]
  f = par[8]
  CKD_model_adjusted(a, b_F, b_M, c_F, c_M, d, e, f, Sex, Scr, Age, BSA)
} 


# Defining the original parameters on the model 
Original_parameters = c(a=141, b_F=0.7, b_M=0.9, c_F=-0.329, c_M=-0.411,
                        d=-1.209, e=0.993, f=1.018)
Original_CKD_model = function(Sex, Scr, Age){
  CKD_model(a=141, b_F=0.7, b_M=0.9, c_F=-0.329, c_M=-0.411, d=-1.209, e=0.993,
            f=1.018, Sex=Sex, Scr=Scr, Age=Age)
}
Original_CKD_model_adjusted = function(Sex, Scr, Age, BSA){
  CKD_model_adjusted(a=141, b_F=0.7, b_M=0.9, c_F=-0.329, c_M=-0.411, d=-1.209, 
                     e=0.993, f=1.018, 
                     Sex=Sex, Scr=Scr, Age=Age, BSA=BSA)
}

# Defining the other calculated parameters on the model, as calculated by Tobias 
Tobias_parameters = c(a=143.3773, b_F=0.81448, b_M=1.06266, c_F=-0.3164, 
                      c_M=-0.2781, d=-1.118, e=0.99305, f=0.85853)
Tobias_CKD_model = function(Sex, Scr, Age){
  CKD_model(a=143.3773, b_F=0.81448, b_M=1.06266, c_F=-0.3164, c_M=-0.2781, 
            d=-1.118, e=0.99305, f=0.85853, Sex=Sex, Scr=Scr, Age=Age)
}
Tobias_CKD_model_adjusted = function(Sex, Scr, Age, BSA){
  CKD_model_adjusted(a=143.3773, b_F=0.81448, b_M=1.06266, c_F=-0.3164, 
                     c_M=-0.2781, d=-1.118, e=0.99305, f=0.85853, 
                     Sex=Sex, Scr=Scr, Age=Age, BSA=BSA)
}



BSA_adjustment <- function(x, BSA)
  x*BSA/1.73






###############################################################################

# Redundant functions

rmse_forCKDmodel  = function(x, data=thedata, adj =F){
  # Computes RSS for a given set of parameters in the CKD model 
  a = x[1]; b_F = x[2]; b_M = x[3]; c_F = x[4]; c_M = x[5]; d = x[6]; 
  e = x[7]; f = x[8]  # Extract parameters from vector x 
  if(adj==T){
    rmse(data$GFR, CKD_model_adjusted(a, b_F, b_M, c_F, c_M, 
                                  d, e, f, data$Sex,
                                  data$Creat, 
                                  data$Age, 
                                  data$SufA))
  }
  else{
    rmse(data$GFR, CKD_model(a, b_F, b_M, c_F, c_M, 
                                  d, e, f, data$Sex,
                                  data$Creat, 
                                  data$Age))
  }  
}









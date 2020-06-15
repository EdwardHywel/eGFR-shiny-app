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


# Defining the original parameters on the model 
Original_CKD_model = function(Sex, Scr, Age){
  CKD_model(a=141, b_F=0.7, b_M=0.9, c_F=-0.329, c_M=-0.411, d=-1.209, e=0.993,
            f=1.018, Sex=Sex, Scr=Scr, Age=Age)
}
Original_CKD_model_adjusted = function(Sex, Scr, Age, BSA){
  CKD_model_adjusted(a=141, b_F=0.7, b_M=0.9, c_F=-0.329, c_M=-0.411, d=-1.209, 
                     e=0.993, f=1.018, 
                     Sex=Sex, Scr=Scr, Age=Age, BSA=BSA)
}




################################################################################
# LM model 
# 

LM_revised_equation <- function(Age, Creat, Sex){
  Creat_mol <- Creat*88.4
  if(Sex=="F"){
    if(Creat_mol<150){
      X = 2.5 + 0.0121*(150-Creat_mol)
    } else if (Creat_mol >=150){
      X = 2.5-0.926*log(Creat_mol/150)
    }
  } else if (Sex=="M"){
    if(Creat_mol<150){
      X = 2.56 + 0.00968*(180-Creat_mol)
    } else if (Creat_mol >=150){
      X = 2.56-0.926*log(Creat_mol/180)
    }
  }
  GFR = exp(X-0.0158*Age+0.438*log(Age))
  return(GFR)
}
LM_revised_equation <- Vectorize(LM_revised_equation)

LM_revised_adj_equation <- function(Age, Creat, Sex, BSA){
  LM_revised_equation(Age, Creat, Sex)*(BSA/1.73)
}
LM_revised_adj_equation <- Vectorize(LM_revised_adj_equation)  






## Get equilibrium abundance
## 
## 
## 
## 

get_Nequil <- function(input_com_params,
                       temperature){
  
  #S <- input_com_params$S
  #al <- input_com_params$alpha_ij
  bopt <- input_com_params$b_opt
  spread <- input_com_params$s
  ab <- input_com_params$a_b
  ad <- input_com_params$a_d
  z <- input_com_params$z
  
  bet<-delt<-0.001
  b0<- ab * exp(-(temperature - bopt)^2/spread)
  d0<- ad * exp(z*temperature)
  rms<-b0-d0
  K<-rms/(bet+delt)

  K
}
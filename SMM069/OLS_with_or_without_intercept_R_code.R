rm(list = ls())

list_pack <- c("MASS","stats")
lapply(list_pack, library, character.only = TRUE)
options(scipen = 999)


################################################################################
###################### Section 1 -- Basic functions ############################
################################################################################

######## Creates a correlation matrix for X ########
sigma.rho <- function (rho_val, m_val) {
  temp <- diag(1,m_val)
  for(i in 2:m_val){
    for(j in 1:(i-1)){
      temp[i,j]<-rho_val^(abs(i-j)) 
      temp[j,i]<-temp[i,j] # symmetric matrix 
    }
  }
  return(temp)
}


####### generate beta values to be both negative and positive ##########
beta_func <- function(m_val){
  m_2<- ceiling((m_val)/2)
  rep(c(1,-1),length.out= m_val)*rep(1:m_2,each=2)[1:(m_val)]                                                
}


####### Data generating process for X with zero mean for each covariate############
sim_x <- function (n_val, rho_val, m_val) {
  sigma.temp <- sigma.rho(rho_val,m_val) 
  # multicoliniarity through \rho
  temp <- mvrnorm(n_val,mu=rep(0,m_val),Sigma=sigma.temp) 
  # centering
  mu <- apply(temp,2,mean)
  temp_zero_mean <- temp-matrix(1,nrow=n_val)%*%matrix(mu,nrow=1)
  return(temp_zero_mean)
}


####### Data generating process for Y given X with zero mean for each covariate and dependent variable Y ########
sim_x_y <-function (n_val, rho_val, m_val, sigma_val) {
  # generate covariates with zero mean for each covariate
  xxx.temp <- sim_x(n_val,rho_val,m_val)
  # generate beta values to be both negative and positive
  beta.temp <- beta_func(m_val)
  #  Computing the Linear Combination of X and beta
  beta_x <- xxx.temp%*%beta.temp
  # generate Y
  yyy.temp <- rnorm(n_val,mean=as.vector(beta_x),sd=sigma_val)
  # Standardisation of Y with zero mean
  yyy.temp_stand <- yyy.temp - mean(yyy.temp) 
  zzz.temp<-cbind(xxx.temp, yyy.temp_stand)
  return(zzz.temp)
}


###### Run MLR and check whether we get the same results when data have zero mean in each covariate and Y ########
n_val_mine <- 100; m_val_mine <- 3 #### set the parameter of your experiment: sample size (n_val_mine) and no. of covariates (m_val_mine)
data <- sim_x_y(n_val=n_val_mine, rho_val=0.2, m_val=m_val_mine, sigma_val=100)
data_X <- data[,-(m_val_mine+1)]; data_Y <- data[,(m_val_mine+1)] #data initialisation for m_val_mine covariates and one dependent variable
out1 <- summary(lm(data_Y~data_X)) # MLR output object when intercept is fitted
coeff1<- out1$coefficients[,"Estimate"] # MLR estimates when intercept is fitted
coeff1 
out2 <- summary(lm(data_Y~data_X-1)) # MLR output object when intercept is set to be zero
coeff2 <-out2$coefficients[,"Estimate"] # MLR estimates when intercept is set to be zero
coeff2
coeff1[-1]-coeff2 #### check if the two estimates are identical 
##### Conclusion 1:  results very much close to each except to some rounding errors  #####
##### Conclusion 2:  except of the rounding errors, MLR estimates with or without setting the intercept to be 0 are identical #####
##################################################################################################################################






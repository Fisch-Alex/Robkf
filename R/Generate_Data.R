Generate_Data = function(n,A,C,Sigma_Add,Sigma_Inn,mu_0 = NULL,  anomaly_loc = integer(0), anomaly_type = character(0), 
                         anomaly_comp = integer(0),  anomaly_strength = NULL){
  
  if (is.null(mu_0)){
    mu_0 = rep(0, nrow(Sigma_Inn))
  } 
  
  if (is.null(anomaly_strength)){
    anomaly_strength = rep(1000,length(anomaly_component))
  }
  
  Innovations =  Sigma_Inn^(1/2) %*% matrix(rnorm(n*nrow(Sigma_Inn)),nrow = nrow(Sigma_Inn))
  Additions =  Sigma_Add^(1/2) %*% matrix(rnorm(n*nrow(Sigma_Add)),nrow = nrow(Sigma_Add))
  
  
  for (ii in 1:length(anomaly_loc)){
    
    
    if (anomaly_type[ii] == "Inn"){
      Innovations[anomaly_comp[ii],anomaly_loc[ii]] =  (Sigma_Inn[anomaly_comp[ii],anomaly_comp[ii]]^(1/2))*anomaly_strength[ii]
    }
    
    if (anomaly_type[ii] == "Add"){
      Additions[anomaly_comp[ii],anomaly_loc[ii]] = (Sigma_Add[anomaly_comp[ii],anomaly_comp[ii]]^(1/2))*anomaly_strength[ii]
    }
    
  }
  
  Y_list = list()
  
  hidden_stats = matrix(0,ncol = ncol(Innovations)+1,nrow = nrow(Innovations))
  hidden_stats[,1] = mu_0
  
  for (ii in 1:ncol(Innovations)) {
    hidden_stats[,ii+1] = A %*% matrix(hidden_stats[,ii],ncol=1) + matrix(Innovations[,ii],ncol = 1)
  }
  
  for (ii in 1:ncol(Additions)) {
    Y_list = c(Y_list,list(C %*% matrix(hidden_stats[,ii+1],ncol=1) + matrix(Additions[,ii],ncol = 1) ))
  }
  
  return(Y_list)
  
}
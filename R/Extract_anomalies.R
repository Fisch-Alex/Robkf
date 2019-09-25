summary_kalman = function(x,time = NULL,horizon = NULL){
  
  if (is.null(time)){
    time = length(x[["particles"]]) - 1
  }
  
  if (is.null(horizon)){
    horizon = x$horizon - 1
  }
  
  x = Anomaly_Smoother(x,time,horizon)
  
  pre_out = Extractanomalies(x)
  
  if (length(pre_out$anomaly_locations) == 0){
    
    cat(paste("At time",time,"out of", length(x[["particles"]]) - 1, "no anomalies have been detected"))
    
  } else {
    
    cat(paste("At time",time,"out of", length(x[["particles"]]) - 1, "the anomalies have been inferred at the following times:"))
    
    cat("\n")
    
    for (ii in 1:length(pre_out$anomaly_locations)){
      
      cat(paste(pre_out$anomaly_locations[ii] , "\t"))
    }
    
  }
  
}




is_anomaly = function(X){
  return(X$which_one != "None" & X$horizon == 1)
}

is_inn = function(X){
  return(X$which_one == "W"  & X$horizon == 1 )
}

get_comp = function(X){
  return(X$comp)
}

is_add = function(X){
  return(X$which_one == "V")
}

Inn_Probs = function(X,q){
  
  indicators = unlist(lapply(X,is_inn))
  
  indices = unlist(lapply(X,get_comp))[which(indicators)]
  
  indices = c(indices,1:q)
  
  freq = as.numeric(table(indices)) - 1
  
  return(freq/length(X))
  
}


Add_Probs = function(X,p){
  
  indicators = unlist(lapply(X,is_add))
  
  indices = unlist(lapply(X,get_comp))[which(indicators)]
  
  indices = c(indices,1:p)
  
  freq = as.numeric(table(indices)) - 1
  
  return(freq/length(X))
  
}


Extract_Number_Of_Anomalies = function(X){
  
  return(mean(unlist(lapply(X,is_anomaly))))
  
}

Extractanomalies = function(X){
  
  output = list()
  
  Anomaly_Prob = unlist(lapply(X[["particles"]],Extract_Number_Of_Anomalies))
  
  output[["anomaly_locations"]] = which(Anomaly_Prob > 0) 
    
  output[["anomaly_probs"]] = Anomaly_Prob[output[["anomaly_locations"]]]
  
  if (length(output[["anomaly_locations"]]) > 0){
    
    output[["anomaly_inn_prob"]] = matrix(unlist(lapply(X[["particles"]][which(Anomaly_Prob > 0) ],Inn_Probs,q=X[["q"]])),byrow = T,ncol=X[["q"]])
    #output[["anomaly_inn_prob"]]  = unlist(lapply(X[["particles"]][which(Anomaly_Prob > 0) ],Inn_Probs,q=X[["q"]]))
    output[["anomaly_add_prob"]] = matrix(unlist(lapply(X[["particles"]][which(Anomaly_Prob > 0) ],Add_Probs,p=X[["p"]])),byrow = T,ncol=X[["p"]])
    #output[["anomaly_add_prob"]]  = unlist(lapply(X[["particles"]][which(Anomaly_Prob > 0) ],Add_Probs,p=X[["p"]]))
  } else {
    
    output[["anomaly_inn_prob"]] = matrix(0,nrow = 0, ncol = X[["q"]])
    
    output[["anomaly_add_prob"]] = matrix(0,nrow = 0, ncol = X[["p"]])
    
  }
  
  output[["anomaly_locations"]] = output[["anomaly_locations"]] - 1
  
  return(output)
  
  

  
}
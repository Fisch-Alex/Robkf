print.ioaorkf = function(x,time = NULL,horizon = 0){
  
  if (is.null(time)){
    time = length(x[["particles"]]) - 1
  }
  
  if (horizon > 0){
    x = Anomaly_Smoother(x,time,horizon)
  }
  
  pre_out = Extractanomalies(x)
  
  if (length(pre_out$anomaly_locations) == 0){
    
    cat(paste("At time",time,"out of", length(x[["particles"]]) - 1, "no anomalies have been detected"))
    
  } else {
    
    cat(paste("At time",time,"out of", length(x[["particles"]]) - 1, "the following anomalies have been inferred"))
    
    cat("\n")
    cat("\n")
    cat("location\t")
    cat("prob.\t\t")
    
    
    for (jj in 1:ncol(pre_out$anomaly_add_prob)){
      cat(paste("prob. add.", jj , "\t"))
    }
    
    for (jj in 1:ncol(pre_out$anomaly_inn_prob)){
      cat(paste("prob. inn.", jj , "\t"))
    }
    
    cat("\n")
    
    for (ii in 1:length(pre_out$anomaly_locations)){
      
      cat(paste(pre_out$anomaly_locations[ii] , "\t\t"))
      
      cat(paste(round(pre_out$anomaly_probs[ii],2) , "\t\t" ))
      
      
      for (jj in 1:ncol(pre_out$anomaly_add_prob)){
        cat(paste(round(pre_out$anomaly_add_prob[ii,jj],2) , "\t\t"))
      }
      
      for (jj in 1:ncol(pre_out$anomaly_inn_prob)){
        cat(paste(round(pre_out$anomaly_inn_prob[ii,jj],2) , "\t\t"))
      }
      
      cat("\n")
      
    }
  }
}
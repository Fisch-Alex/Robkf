summary.ioaorkf = function(x,time = NULL,horizon = NULL){
  
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
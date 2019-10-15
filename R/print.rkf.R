#' Print method for rkf
#'
#' @name print.rkf 
#' @description A function to print the output produced by \code{\link{AORKF_t}}, \code{\link{AORKF_huber}}, or \code{\link{IORKF_huber}}. One can specify a time during the run for which the output should be displayed.
#' @param x An rkf object.
#' @param time A positive integer giving the time at which the output is to be displayed. It defaults to the number of observations.
#' @param conf_level A probability between 0 and 1 giving the confidence level at which the series are to be tested against anomalies. It defaults to 0.95.
#' @return A ggplot object.
#' @export
print.rkf = function(x,time = NULL,conf_level = 0.95){
  
  if (is.null(time)){
    time = length(x[["Y"]]) 
  }
  
  time = as.integer(time)
  
  if (time > length(x[["Y"]]) ){
    stop("Time must be less than the number of observations.")
  }
  
  if (time < 1){
    stop("Time must be positive.")
  }
  
  conf_level = as.numeric(conf_level)
  
  if (conf_level >= 1){
    stop("conf_level must be between 0 and 1")
  }
  
  if (conf_level <= 0){
    stop("conf_level must be between 0 and 1")
  }
  
  scores = abs(Extract_all_anomalies(x))
  
  pre_out = which(scores> qnorm(conf_level))
  
  probs = dnorm(scores[pre_out])
  
  if (length(pre_out) == 0){
    
    cat(paste("At time",time,"out of", length(x[["Y"]]), "no anomalies have been detected"))
    
  } else {
    
    cat(paste("At time",time,"out of", length(x[["Y"]]), "the following anomalies have been inferred"))
    
    cat("\n")
    cat("\n")
    cat("location\t")
    cat("prob.\t\t")
    
    cat("\n")
    
    for (ii in 1:length(pre_out)){
      
      cat(paste(pre_out[ii] , "\t\t"))
      
      cat(paste(probs[ii] , "\t\t" ))
      cat("\n")
      
    }
  }
}
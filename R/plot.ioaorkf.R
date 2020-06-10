#' Plot method for ioarkf
#'
#' @name plot.ioaorkf 
#' @description A function to plot the output of an IOAORKF filter. One can specify a time during the run for which the output should be displayed.
#' @param x An ioaorkf object.
#' @param time A positive integer giving the time at which the output is to be displayed. It defaults to the number of observations
#' @param horizon A positive integer giving the smoothing horizon that is to be used. It must be at least equal to the number of rows of the horizonmatric used to obtain the ioaorkf object.
#' @param subset A list of integers indicating the components of observations which are to be plotted.
#' @param ... Other parameters to be passed to plotting methods.
#' @return A ggplot object.
#' @export
plot.ioaorkf = function(x,time = NULL,horizon = NULL,subset = NULL,...){
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",unexpectedarguments,"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}  
  
  is_observed<-value<-NULL
  
  if (is.null(horizon)){
    horizon = x$horizon
  }
  
  horizon = as.integer(horizon)
  
  if (horizon < x$horizon - 1){
    stop("horizon must be at least the number of rows of the horizonmatrix used to genereate x.")
  }
  
  if (is.null(time)){
    time = length(x[["particles"]]) - 1
  }
  
  time = as.integer(time)

  if (time > length(x[["particles"]]) - 1){
    stop("Time must be less than the number of observations.")
  }
  
  if (time < 1){
    stop("Time must be positive.")
  }
  
  horizon = min(horizon,time)
  
  if (horizon > 0){
    x_new = Anomaly_Smoother(x,time,horizon)
  } else{ 
    x_new = x
  }
  
  pre_out = Extractanomalies(x_new)
  
  mydaf= as.data.frame(t(Reduce(cbind,x[["Y"]])))
  n = nrow(mydaf)
  p = ncol(mydaf)
  
  if (is.null(subset)){
    subset = 1:p
  } 
  
  subset = unique(as.integer(subset))
  
  if (sum(subset %in% 1:p) < length(subset)){
    stop("subset has the wrong dimensions.")
  }
  
  colnames(mydaf) = paste("y",1:p,sep="")
  
  mydaf$x = 1:n
  
  mydaf = mydaf[,c("x",paste("y",subset,sep=""))]
  
  if (time < length(x[["particles"]]) - 1){
    
    molten_daf = molten.X<-melt(mydaf,id="x")
    
    molten.X$is_observed = 0
    molten.X[which(molten.X$x > time),"is_observed"] = 1
    names = as.vector(unique(molten.X$variable))
    
    out<-ggplot(data=molten.X)
    out<-out+aes(x=x,y=value,colour = is_observed)
    out<-out+theme(legend.position="none") 
    out<-out+ scale_colour_gradient(low="black", high="grey")
    out<-out+geom_point()
    out<-out+theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    
    out = out+theme(legend.position="none")
    
    
  } else {
    
    molten_daf = molten.X<-melt(mydaf,id="x")
    names = as.vector(unique(molten.X$variable))
    
    out<-ggplot(data=molten.X)
    out<-out+aes(x=x,y=value)
    out<-out+theme(legend.position="none") 
    out<-out+geom_point()
    out<-out+theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    
  }
  
  if(length(pre_out$anomaly_locations)>0){
    
    for (ii in 1:length(pre_out$anomaly_locations)){
      
      out = out+geom_vline(xintercept = pre_out$anomaly_locations[ii],colour="blue",alpha = sum(pre_out$anomaly_inn_prob[ii,]))+ theme(legend.position="none")
      
      for (jj in 1:p){
        
        Point_Anomalies = pre_out$anomaly_locations[ii] + (jj-1)*n
        
        out = out + geom_point(data = molten_daf[Point_Anomalies,] ,colour="red", size=1.5, alpha = pre_out$anomaly_add_prob[ii,jj])
        
      }
      
    }
    
  }
  
  return(out)
  
}
#' @export
IOAOKF = function(Y,mu_0,Sigma_0=NULL,A,C,s,Number,Particles,Sigma_Add,Sigma_Inn,anom_add_prob,anom_inn_prob,epsilon=0.000001,horizon_matrix)
{
  
  Y_Full_list     = Y
  Particle_Number = Particles
  
  Num_Descendents = Number
  Num_Particles   = Particles
  
  prob_add        = anom_add_prob
  if (length(prob_add) == 1){
    prob_add = rep(prob_add,nrow(Y[[1]]))
  }
  
  prob_inn        = anom_inn_prob
  if (length(prob_inn) == 1){
    prob_inn = rep(prob_inn,nrow(mu_0))
  }
  
  horizon = nrow(horizon_matrix)
  
  Number_of_resamples = colSums(horizon_matrix)
  
  Final_Sigma = Sigma_Limit(Sigma,C,A,Sigma_Inn,Sigma_Add,epsilon)
  
  if(is.null(Sigma_0)){
    Sigma_0 = Final_Sigma
  }
  
  to_sample = list()
  
  for (ii in 1:horizon){
    to_sample[[ii]] = horizon_matrix[ii,]
  }
  
  C_matrix_list = list()
  Sigma_Add_matrix_list = list()
  Sigma_Inn_matrix_list = list()
  
  p = nrow(Sigma_Add)
  q = nrow(Sigma_Inn)
  
  New_matrix  = C
  Full_matrix = C
  New_Inn_Matrix = C %*%  Sigma_Inn %*% t(C)
  
  for (jj in 1:nrow(horizon_matrix)){
    
    C_matrix_list[[jj]] = Full_matrix
    New_matrix          = New_matrix%*%A
    Full_matrix         = rbind(Full_matrix,New_matrix)
    
    Sigma_Add_matrix_list[[jj]] = diag(rep(diag(Sigma_Add),jj),ncol=p*jj)
    Sigma_Inn_matrix_list[[jj]] = New_Inn_Matrix
    
    Even_Newer_New_Matrix = matrix(0,ncol=ncol(New_Inn_Matrix)+nrow(C),nrow=ncol(New_Inn_Matrix)+nrow(C)) 
    Even_Newer_New_Matrix[(1+nrow(C)):nrow(Even_Newer_New_Matrix),(1+nrow(C)):nrow(Even_Newer_New_Matrix)] = New_Inn_Matrix
    
    New_Inn_Matrix = Full_matrix  %*%  Sigma_Inn   %*% t(Full_matrix) + Even_Newer_New_Matrix
    
  }
  
  Y_expanded = list()
  
  for (jj in 1:nrow(horizon_matrix)){
    
    Considered_Y = Y_Full_list[jj:1]
    
    Full_Y_list = list()
    
    New_Y = matrix(0,ncol=1,nrow=0)
    
    for (ii in 1:length(Considered_Y)){
      
      New_Y = rbind(Considered_Y[[ii]],New_Y)
      
      Full_Y_list[[ii]] = New_Y
      
    }
    
    Y_expanded[[jj]] = Full_Y_list
    
  } 
  
  for (jj in (nrow(horizon_matrix)+1):length(Y_Full_list) ){
    
    Considered_Y = Y_Full_list[jj:(jj-nrow(horizon_matrix)+1)]
    
    Full_Y_list = list()
    
    New_Y = matrix(0,ncol=1,nrow=0)
    
    for (ii in 1:length(Considered_Y)){
      
      New_Y = rbind(Considered_Y[[ii]],New_Y)
      
      Full_Y_list[[ii]] = New_Y
      
    }
    
    Y_expanded[[jj]] = Full_Y_list
    
  }
  
  sigma_tilde = get_sigma_tilde(Final_Sigma,C,A,Sigma_Add,Sigma_Inn)
  
  sigma_hat   = get_sigma_hat(Final_Sigma,C_matrix_list,A,Sigma_Add_matrix_list,Sigma_Inn,Sigma_Inn_matrix_list,horizon_matrix)
  
  #### forget abut the stuff below:
  
  precision = solve( C %*% ( A %*%  Sigma_0  %*% t(A)  +  Sigma_Inn ) %*%  t(C) + Sigma_Add )
  
  Out = Robust_filter(Y_expanded, C_matrix_list, Sigma_Add_matrix_list, Sigma_Inn_matrix_list, A, Sigma_Inn, Sigma_Add, s, Num_Descendents, Num_Particles, to_sample, Number_of_resamples, sigma_tilde, sigma_hat, mu_0, Sigma_0, horizon, prob_inn, prob_add, Particle_Number, Y_Full_list)
  
  return(Out)
  
}
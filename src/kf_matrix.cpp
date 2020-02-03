#include <RcppEigen.h>
#include <list>
#include <iostream>
#include "kf.h"

std::list<Eigen::MatrixXd>  kf_matrix(const Eigen::MatrixXd& mu_old,
					const Eigen::MatrixXd& Sigma_old,
					const Eigen::MatrixXd& y,
					const Eigen::MatrixXd& A,
					const Eigen::MatrixXd& b,
					const Eigen::MatrixXd& C,
					const Eigen::MatrixXd& d,
					const Eigen::MatrixXd& R,
					const Eigen::MatrixXd& Q)	
{

  Eigen::MatrixXd m = A.transpose()*mu_old + b;
  Eigen::MatrixXd P = A.transpose()*Sigma_old*A + Q;
  Eigen::MatrixXd I = Eigen::MatrixXd::Identity(C.rows(),C.rows());  
  Eigen::MatrixXd K = (C.transpose()*P*C+R).inverse()*C.transpose()*P;
  Eigen::MatrixXd Sigma_new = K.transpose()*Gamma*K + (I - K.transpose()*C.transpose()) * P * (I - C * K);
  Eigen::MatrixXd mu_new = m + K.transpose() * (y - C.transpose() * m - d);

  std::list<Eigen::MatrixXd> result;
  result.push_back(mu_new);
  result.push_back(Sigma_new);
  return(result);

}
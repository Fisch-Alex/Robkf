#include <RcppEigen.h>
#include <list>
#include <iostream>

#include "iorkf_huber.h"

// [[Rcpp::export]]
std::list<Eigen::MatrixXd> iorkf_huber_matrix(const Eigen::MatrixXd& mu_old,
					const Eigen::MatrixXd& Sigma_old,
					const Eigen::MatrixXd& y,
					const Eigen::MatrixXd& A,
					const Eigen::MatrixXd& b,
					const Eigen::MatrixXd& C,
					const Eigen::MatrixXd& d,
					const Eigen::MatrixXd& R,
					const Eigen::MatrixXd& Q,
					const double& h)	
{

  Eigen::MatrixXd I = Eigen::MatrixXd::Identity(C.rows(),C.rows());

  Eigen::MatrixXd m = A.transpose()*mu_old + b;
  Eigen::MatrixXd P = A.transpose()*Sigma_old*A + Q;

  Eigen::MatrixXd Y_GAP  = y - C.transpose()*m - d;
  Eigen::MatrixXd M      = P*C*(((C.transpose())*P*C   +   R).inverse());
  Eigen::MatrixXd Update = (I - C.transpose()*M)*Y_GAP;

  double magnitude = Update.norm();

  if (magnitude > h)
  {
    Update = Update * (h/magnitude);
  }

  Eigen::MatrixXd Sigma_new = (I - M * (C.transpose())) * P;
  Eigen::MatrixXd mu_new    = m + C.inverse()*(Y_GAP - Update);
  

  std::list<Eigen::MatrixXd> result;
  result.push_back(mu_new);
  result.push_back(Sigma_new);
  return(result);
}

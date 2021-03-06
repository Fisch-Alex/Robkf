#include <RcppEigen.h>
#include <list>
#include <iostream>

std::list<Eigen::MatrixXd>  kf_matrix(const Eigen::MatrixXd& mu_old,const Eigen::MatrixXd& Sigma_old, const Eigen::MatrixXd& y, const Eigen::MatrixXd& A, const Eigen::MatrixXd& b, const Eigen::MatrixXd& C, const Eigen::MatrixXd& d, const Eigen::MatrixXd& R, const Eigen::MatrixXd& Q);

std::list<std::list<Eigen::MatrixXd> > kf_list(const Eigen::MatrixXd& mu_init, const Eigen::MatrixXd& Sigma_init, const std::list<Eigen::MatrixXd>& ys, const Eigen::MatrixXd& A, const Eigen::MatrixXd& b, const Eigen::MatrixXd& C, const Eigen::MatrixXd& d, const Eigen::MatrixXd& R, const Eigen::MatrixXd& Q);
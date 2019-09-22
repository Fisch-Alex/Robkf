#include "Particle.h"
#include <RcppEigen.h>

std::list< std::list < struct Particle > > Kalman_filter( const std::list<std::list<Eigen::MatrixXd> > & Y_expanded, const std::list<Eigen::MatrixXd> & C_list, const std::list<Eigen::MatrixXd> & Sigma_Add_list, 
const std::list<Eigen::MatrixXd> & Sigma_Inn_Contribution, const Eigen::MatrixXd & A, const Eigen::MatrixXd & Sigma_Inn, const Eigen::MatrixXd & Sigma_Add, const double & s, const int & Num_Descendents, const int & Num_Particles,
const std::list<std::vector<int> > & to_sample, const std::vector<int> & Number_of_resamples, const std::vector<double> & sigma_tilde, const std::vector<double> & sigma_hat, const Eigen::MatrixXd & mu_0, const Eigen::MatrixXd & Sigma_0,
const int & horizon, const std::vector <double> & prob_inn, const std::vector <double> & prob_add, int Particle_Number, const std::list<Eigen::MatrixXd> & Y_Full_list)
{

	std::list<std::list<struct Particle> > Output;

	std::list<struct Particle> New_Particles;
	std::list<Eigen::MatrixXd> Y_list;
	std::list<Eigen::MatrixXd>::const_iterator Y_Full_it = Y_Full_list.begin();

	New_Particles = Initial_list(mu_0,Sigma_0,Particle_Number);

	Output.push_back(New_Particles); 

	int counter;

	std::list<std::list<Eigen::MatrixXd> >::const_iterator Y_iterator = Y_expanded.begin();

	for (counter = 0; counter < horizon-1; counter++)
	{

		Y_list.push_front(*Y_Full_it);		

		New_Particles = Kalman_step(*Y_iterator, counter+1, Output, A, C_list, Sigma_Add_list, Sigma_Inn_Contribution, sigma_hat, sigma_tilde, to_sample, Sigma_Inn, Sigma_Add, Num_Descendents, s, prob_inn, 
					    prob_add, Num_Particles, Y_list, Number_of_resamples);
		Output.push_back(New_Particles);
		Y_iterator++;
		Y_Full_it++;

	}

	

	for (counter = horizon-1; counter < Y_expanded.size(); counter++)
	{

		Y_list.push_front(*Y_Full_it);		

		New_Particles = Kalman_step(*Y_iterator,horizon,Output, A, C_list, Sigma_Add_list, Sigma_Inn_Contribution, sigma_hat, sigma_tilde, to_sample, Sigma_Inn, Sigma_Add, Num_Descendents, s, prob_inn, 
					    prob_add, Num_Particles, Y_list, Number_of_resamples);
		Output.push_back(New_Particles);
		Y_iterator++;
		Y_Full_it++;

	}

	return(Output);

};
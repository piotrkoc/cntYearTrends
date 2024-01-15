// bayesian latent trait model 6 for estimating democratic mood / satis
// item intercepts / biases and slopes / factor loadings 
// item-country intercepts
// covariation between intercepts and slopes
// single autoregressive error
// responses are modelled as beta-binomial count
// item intercept and slope expectations fixed to identify scale and location
// non-centered parameters for theta, delta, lambda and gamma
// ragged array of theta (varying times-series by unit)

data{
  int<lower=1> N;               	// number of national survey opinions
  int<lower=1> J;               	// number of countries
  int<lower=1> K;  	        		// number of items
  int<lower=1> P;  	        		// number of items-country combinations  
  int<lower=1> R;  	        		// number of national opinion estimates
  array [N] int<lower=1,upper=J> jj;   	// country j for opinion n
  array [N] int<lower=1,upper=K> kk;   	// item k for opinion n
  array [N] int<lower=1,upper=P> pp;   	// item-country p for opinion n
  array [N] int<lower=1,upper=R> rr;   	// estimate r for opinion n
  array [N] int<lower=1> x;   			    // vector of survey responses, count
  array [N] int<lower=1> samp;				// vector of sample sizes
  array [K] int<lower=1> it_len;			// number of countries for each item
  array [J] int<lower=1> est_pos;			// indicator showing cntry start for estimate vector	
  array [J] int<lower=1> len_theta_ts;		// indicator showing length of each cntry estimated time series
  real mn_resp_log;				    // observed response mean proportion on logit scale
}

parameters{
  real<lower=0> sigma_theta;	    // opinion evolution error SD
  vector[P] delta_ncp;			    // redundant parameters for item-country effects
  vector[R] theta_ncp; 	            // redundant parameters for mood
  row_vector[J] theta_init;			// initial latent traits for year -1
  real<lower=0> phi;				// dispersion parameter
  corr_matrix[2] Omega;           	// correlation matrix for item pars
  vector<lower=0>[2] tau;         	// cor -> cov conversion
  real<lower=0> sigma_delta;	    // item-country intercept error SD
  real mu_lambda;         		    // item intercept expectation
  matrix[K,2] Gamma_ncp;			// non-centered parameters for item parameters
}

transformed parameters{
  vector[R] theta; 	                // R-vector of theta values	
  vector[K] lambda;                 // K estimated item intercepts
  vector[K] gamm;                   // K estimated item slopes
  vector<lower=0,upper=1>[N] eta; 	// fitted values, on logit scale
  matrix[2,2] Sigma;			    // variance-covariance matrix for item ints and slopes
  matrix[K,2] Gamma;				// matrix of item intercepts and slopes 
  vector<lower=0>[N] beta_par1;		// beta shape par 1
  vector<lower=0>[N] beta_par2;		// beta shape par 2  
  vector[P] delta;			        // P item-country effects
  real mu_gamm;                     // item slope expectation
  delta = sigma_delta * delta_ncp;
  mu_gamm = 1;						// fix item slope expectation at 1
  for (j in 1:J) {                  // structural model of mood
    theta[est_pos[j]] = theta_init[j] 
	+ sigma_theta * theta_ncp[est_pos[j]];
	for (i in 1:(len_theta_ts[j]-1)) {
	  theta[(est_pos[j]+i)] = theta[(est_pos[j]+i-1)] 
	  + sigma_theta * theta_ncp[(est_pos[j]+i)];
	}
  }
  // item parameter models with with non-centered parameters
  Sigma = quad_form_diag(Omega, tau);  
  for (k in 1:K) 
    Gamma[k] = [ mu_lambda , mu_gamm ] + Gamma_ncp[k] * Sigma;
  lambda = Gamma[,1];
  gamm = Gamma[,2];
  eta = inv_logit(lambda[kk] + gamm[kk] .* theta[rr] + delta[pp]);  // fitted values model
  beta_par1 = phi * eta; 				// reparamaterise beta par 1
  beta_par2 = phi * (1 - eta); 			// reparamaterise beta par 2
}

model{
  x ~ beta_binomial(samp, beta_par1, beta_par2);  // response model
  phi ~ gamma(3, 0.04); 				
  sigma_theta ~ normal(0, 1); 
  sigma_delta ~ normal(0, 1); 
  tau ~ normal(0, 1);
  Omega ~ lkj_corr(2);
  mu_lambda ~ normal(mn_resp_log, 0.5);
  theta_init ~ normal(0, 1);
  theta_ncp ~ normal(0, 1);
  to_vector(Gamma_ncp) ~ normal(0, 1);
  int pos;					        // local variable indicating which item to evaluate	
  pos = 1;
  for (k in 1:K) {			        // standard normal prior for item-country effects within items
    segment(delta_ncp, pos, it_len[k]) ~ normal(0, 1);
    pos = pos + it_len[k];
  }
}

generated quantities {
  vector[N] x_pred;					// fitted data to check model
  vector[N] log_lik; 				// log lik for WAIC calc
  for (i in 1:N) {
    x_pred[i] = beta_binomial_rng(samp[i], beta_par1[i], beta_par2[i]);
    log_lik[i] = beta_binomial_lpmf(x[i] | samp[i], beta_par1[i], beta_par2[i]); 
  }
}


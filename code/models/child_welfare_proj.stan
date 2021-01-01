data {
  int<lower=0> N; //observations (years) * states
  int<lower=0> M; //projection steps (years) * states
  int<lower=0> S; //states
  int<lower=0> R; // regions
  int<lower=0> Y; //number of years
  int<lower=0> P; //projection steps 
  int<lower=0> K; //number of covariates
  vector[N] y; //outcome (log ent pc)
  int<lower=0> state_i[N]; //state index
  int<lower=0> year_i[N]; //year index
  int<lower=0> region_i[N]; //region index
  int<lower=0> state_j[M]; //state index for projections
  int<lower=0> year_j[M]; //year index for projections
  int<lower=0> region_j[M]; //region index for projections
  matrix[N,K] X; // covariates
  vector[Y] year_c; // centered year variable, includes projection years
  int<lower=0> region_s[S]; // census division
  matrix[S,K] last_obs;
  matrix[S,K] second_last_obs;
  matrix[S,K] third_last_obs;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_eps;
}
parameters {
  vector[S] beta0; 
  real beta[K,R,Y]; // coefficient on covars for outcome
  real<lower=0> sigma_y;
  matrix[S,Y] eps;
}

transformed parameters {
  vector[N] mu;
  vector[N] eps_i;
  
  for(i in 1:N){
    //get the eps in the vector form
    eps_i[i] = eps[state_i[i], year_i[i]];
    mu[i] = beta0[state_i[i]] + to_row_vector(beta[1:K,region_i[i],year_i[i]])*to_vector(X[i,1:K])+eps_i[i];
  }


}

model {
  
  y ~ normal(mu, sigma_y);
  beta0 ~ normal(0,3); 
    for(k in 1:K){
      for(r in 1:R){
           to_vector(beta[k,r,1:Y]) ~ normal(0,sigma_beta); 
      }
  }
  
  sigma_y ~ normal(0,1);

    //RW1 model on eps
  for(s in 1:S){
     eps[s,1] ~ normal(0, sigma_eps);
     eps[s,2:Y] ~ normal(eps[s,1:(Y-1)], sigma_eps);
  }
}

generated quantities{
    //project forward P years
  vector[M] y_p; // projection of FC
  vector[M] mu_p; // projection of FC expected value
  matrix[M,K] X_p; // projection of covars
  real mu_x_sp[S,P,K]; // projection of E(X) 
  matrix[M,K] mu_x_j; // projection of E(X) in vector form
  matrix[S,P] eps_p; // projection of eps
  vector[M] eps_j; //projection of eps in vector form
  real beta_p[K,R,P]; // coefficient on covars for outcome
  
  for(k in 1:K){
    for(s in 1:S){
      mu_x_sp[s,1,k] = (last_obs[s,k] + second_last_obs[s,k] + third_last_obs[s,k])/3;
      mu_x_sp[s,2,k] = (mu_x_sp[s,1,k]+ last_obs[s,k] + second_last_obs[s,k])/3;
      mu_x_sp[s,3,k] = (mu_x_sp[s,1,k]+ mu_x_sp[s,2,k]+ last_obs[s,k])/3;
      for(p in 4:P){
        mu_x_sp[s,p,k] = (mu_x_sp[s,p-1,k]+ mu_x_sp[s,p-2,k]+ mu_x_sp[s,p-3,k])/3;
      }
    }
  }
  
  //projection of beta
    for(k in 1:K){
    for(r in 1:R){
      beta_p[k,r,1] = 2*beta[k,r,Y] - beta[k,r,Y-1]; 
      beta_p[k,r,2] = 2*beta_p[k,r,1] - beta[k,r,Y]; 
      for(p in 3:P){
        beta_p[k,r,p] = 2*beta_p[k,r,p-1] - beta_p[k,r,p-2];
      }
    }
  }

  //projectin of eps
    for(s in 1:S){
      eps_p[s,1] = eps[s,Y];

    for(p in 2:P){
      eps_p[s,p] = eps_p[s,p-1];
    }
  }

  for(j in 1:M){
      for(k in 1:K){
          mu_x_j[j,k] = mu_x_sp[state_j[j], year_j[j], k];
          X_p[j,k] = mu_x_j[j,k];  
      }
      eps_j[j] = eps_p[state_j[j], year_j[j]];
      mu_p[j] = beta0[state_j[j]] + to_row_vector(beta_p[1:K,region_j[j],year_j[j]])*to_vector(X_p[j,1:K])+ eps_j[j];
      y_p[j] = mu_p[j]; 
  }
  
  
}

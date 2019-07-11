# FUNCTION -----
# Estimates sample size for logistic regression after providing:
#   OR = odds ratio
#   pie = event proportion
#   z_beta = z for beta where beta = P(type II error)

sample_size <- function(OR = 1.3, pie = 0.07, z_beta = 0.84){
  
  z_alpha = 1.645 # assuming alpha = 0.05-level test 
  gamma = log(OR)
  sigma = (1+(1+(gamma^2))*exp((5*(gamma^2))/4))/(1+exp((-gamma^2)/4))
  n = (((z_alpha + z_beta*exp((-gamma^2)/4))^2)*(1+2*pie*sigma))/(pie*(gamma^2))
  return(n)
  
}

# EXAMPLES -----
sample_size(OR=1.3, pie=0.07, z_beta=0.84)
sample_size(OR=0.6, pie=0.01, z_beta=0.84)

# NOTES -----

# power = 1 - probability(type II error)
#
# z_betas for different powers:
#   70% = 0.52
#   80% = 0.84
#   90% = 1.28


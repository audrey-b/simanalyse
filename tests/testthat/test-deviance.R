constants  <- list(T = 4,                       #number of release occasions
                   R = c(192, 154, 131, 184))   #number of releases

params_M0 <- list(p   = rep(0.5, 4),            #capture probability
                  phi = c(0.8, 0.7, 0.9, 0.8))  #survival probability
code <- "
for(i in 1:T){
 #multinomial likelihood
 m[i,(i+1):(T+2)] ~ dmulti(q[i,(i+1):(T+2)], R[i])
 #multinomial probabilities
 q[i,(i+1)] = phi[i]*p[i]                              #diagonal
 for(j in 1:i){q[i,j] = 0}                             #lower triangle
 q[i,(T+2)] = 1-sum(q[i,1:(T+1)])}                     #never recaptured
for(i in 1:(T-1)){                                     #upper triangle
 for(j in (i+2):(T+1)){q[i,j] = prod(phi[i:(j-1)])*prod(1-p[i:(j-2)])*p[j-1]}}" 

priors_M0 <- "for(j in 1:T){
                p[j] = p0              #capture probability is constant
                phi[j] ~ dunif(0,1)}   #prior on survival probability
                
              p0 ~ dunif(0,1)" #prior on capture probability

monitor_M0 <- c("phi", "p0")

set.seed(20200624)

sims <- sims::sims_simulate(code=code,
                            constants=constants,
                            parameters=params_M0,
                            monitor="m",
                            nsims = 2,                  #number of datasets per setup
                            exists = NA,                  #allow to overwrite directory
                            ask = FALSE)      #do not ask if ok to overwrite directory 

analyse <- sma_analyse(sims, 
                       code, 
                       code.add=priors_M0,
                       mode = simanalyse::sma_set_mode("quick"),  # the mcmc mode
                       monitor=c("phi", "p0")) 

params_M0 <- list(p0 = 0.5,            #capture probability
                  phi = c(0.8, 0.7, 0.9, 0.8))  #survival probability

ev <- sma_evaluate(analyse,
                   parameters= params_M0,
                   measures = c("all"), 
                   alpha = 0.05)

saveRDS(ev, "Evaluate_results/result7.rds")

ev1 <- readRDS("Evaluate_results/result7.rds")

test_that("evaluate-deviance",{
  
  
  
  expect_equal(ev, ev1)
  
})
set.seed(12)


prior <- function(theta) dbeta(theta, 1,1)
likelihood <- function(theta, n) dbinom(n, size = 20, prob = theta)
observed <- 14
P <- function(theta, n) prior(theta) * likelihood(theta, n)
N <- 20000

samples <- vector(mode = "numeric", length = N)
accepted <- vector(mode =  "numeric", length = N)
samples[1] <- 0.5


run_sampler <- function(sd) {
  start <- Sys.time()
  for (i in 1:(N-1)) {
    current <- samples[i]
    proposal <- current + rnorm(1, 0, sd)
    p_accept <- ifelse(proposal > 1 | proposal < 0, 0, P(proposal, observed) / P(current, observed))
    accepted[i] <- ifelse(p_accept > 1, 1, rbinom(1,1, p_accept))
    samples[i+1] = ifelse(accepted[i], proposal, current)
  }
  end <- Sys.time()
  cat(end - start)
}
run_sampler(0.2)


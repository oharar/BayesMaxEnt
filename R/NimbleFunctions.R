#' Internal Nimble code
#'
#' These should be internal functions, but if they are not exported,
#' they cannot always be found.
#'
#' As they are exported they need a help page.
#'
#' @export
#' @param ... Please ignore. I'm just trying to make R happy.
#' @return Functions for Nimble
#' @examples
#' \dontrun{
#' NimbleMaxEnt(Data=X, code=MaxNetcode, adaptInterval=1e3,
#' nchains=1, nburnin = 1e3, niter=6e3, thin=1)
#' }

MaxNetcode <- nimble::nimbleCode({
  for(j in 1:k) {
    ratetau[j] <- MeanLambda*reg[j]
    sigma2[j] ~ dexp(rate=ratetau[j])
#    sigma2[j] <- ratetau[j]
  }
  omega[1:k, 1:k] <- diag(1/sigma2[1:k]) # precision
  alpha ~ dnorm(0, sd = 1)
  beta[1:k] ~ dmnorm(zeros[1:k], omega[1:k, 1:k])
  linpred[1:n] <- alpha + X[1:n, 1:k] %*% beta[1:k]
  logit(Prob[1:n]) <- linpred[1:n]
  y[1:n] ~ dbern_vec(p=Prob[1:n], W[1:n])
})

#' @export
#' @describeIn MaxNetcode vectorised weighted Bernoulli density using in the Nimble code
dbern_vec <- nimble::nimbleFunction( ## Define the distribution
  run = function(x = double(1), p = double(1), wt = double(1),
                 log = integer(0, default = 0)) { #
    returnType(double(0))
    logProbs <- wt*dbinom(x=x, prob=p, size=1, log = TRUE)
    logProb <- sum(logProbs)
    #    logProb <- sum(dbinom(x=x, size=1, prob=p, log = TRUE))
    if(log) return(logProb)
    else return(exp(logProb))
  })

#' @export
#' @describeIn MaxNetcode vectorised random Bernoulli distribution used by Nimble
rbern_vec <- nimble::nimbleFunction( ## Define a simulation function, optionally.
  run = function(n = integer(0), p = double(1), wt = double(1)) {
    returnType(double(1))
    if(n != 1) print("rbern_vec only allows n = 1; using n = 1.")
    smp <- rbinom(n, p, size=1)
    return(smp)
  })



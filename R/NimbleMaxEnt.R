#' Run one chain of a Bayesian MaxEnt model
#'
#' This should normally be used within \code{FitMaxEnt()}
#'
#' @export
#' @import nimble
#' @importFrom stats dbinom model.matrix rbinom rnorm
#' @param Data Data (usually created by SetUpMaxEnt()).
#' @param code Nimble code
#' @param adaptInterval Integer. How many iterations to adapt for. Defaults to 1000
#' @param ... Other arguments passed to runMCMC() (e.g. niter, nburnin)
#' @return A coda object.
#' @examples
#' \dontrun{
#' NimbleMaxEnt(Data=X, code=MaxNetcode, adaptInterval=1e3,
#' nchains=1, nburnin = 1e3, niter=6e3, thin=1)
#' }

NimbleMaxEnt <- function(Data, code, adaptInterval=1e3, ...) {
  # remove this once it's all working
  #  source("VectorisedNimbleMaxEnt.R")

  Inits <- list(alpha = rnorm(1, log(1/(1+1/mean(Data$Data$y))), 1),
                beta = rnorm(Data$Const$k, 0, sd=0.1), sigma2=rep(1, Data$Const$k))

  Model <- nimbleModel(code = code, data = Data$Data,
                       constants = Data$Const, inits = Data$Inits)

  CmyModel <- compileNimble(Model, showCompilerOutput = FALSE)
  ConfModel <- configureMCMC(CmyModel)

  Targets <- c("alpha", paste0("beta[", 1:Data$Const$k, "]"))
  ConfModel$addMonitors(Targets)

  ConfModel$addSampler(target = c("alpha", "beta"), type = "RW_block",
                       control = list(adaptInterval = adaptInterval))

  myMCMC <- buildMCMC(ConfModel, monitors = Targets)
  CmyMCMC <- compileNimble(myMCMC)
  mcmc <- runMCMC(CmyMCMC, samplesAsCodaMCMC=TRUE, inits=Inits, ...)
  return(mcmc)
}

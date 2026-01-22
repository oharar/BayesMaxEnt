#' Fit Bayesian MaxEnt model with MCMC
#'
#' @import coda
#' @export
#' @param maxdat Data (usually created by SetUpMaxEnt()).
#' @param parallel Logical. Should the chains be run in parallel? Defaults to FALSE
#' @param nchains Integer. How many chains to run. Defaults to 1
#' @param nCores Integer. How many cores to use. Defaults to 1, ignored if parallel=FALSE
#' @param ... Other arguments passed to runMCMC() (e.g. niter, nburnin)
#' @return A coda object.
#' @examples
#' \dontrun{
#' FitMaxEnt(Data=X, code=MaxNetcode,
#' adaptInterval=5, nchains=1, nburnin = 5, niter=10, thin=1)
#' }

FitMaxEnt <- function(maxdat, parallel=FALSE, nchains=1, nCores=nchains, ...) {
  # if(parallel) {
  #   warning("parallel not currently working, so setting to FALSE")
  #   parallel <- FALSE
  # }
  if(parallel & nchains>1) {
    nclusters <- min(c(parallel::detectCores(), nCores, nchains))
    this_cluster <- parallel::makeCluster(nclusters)
    parallel::clusterExport(cl = this_cluster,
                            varlist = c("nimbleModel", "dbern_vec",
                                        "rbern_vec", "MaxNetcode"))

    output <- parallel::parLapply(cl = this_cluster, X = 1:nclusters,
                                  NimbleMaxEnt, Data=maxdat, code=MaxNetcode, ...)
    parallel::stopCluster(this_cluster)
    output <- as.mcmc.list(output)

  } else {
      output <- NimbleMaxEnt(Data=maxdat, code=MaxNetcode, nchains=nchains, ...)
  }
  output

}

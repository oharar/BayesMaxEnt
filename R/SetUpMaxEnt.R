#' Function to set up data for use in a Bayesian MaxEnt
#'
#' This formats the data, including creating the basis functions that MaxNet uses.
#' The code is partly taken from the \code{maxnet::maxnet()} function.
#'
#' To change the basis functions (e.g. only include linear terms) use \code{classes=}
#' (e.g. \code{classes="l"} for linear terms).
#'
#' @export
#' @param p a vector of 1 (for presence) or 0 (for background).
#' @param data a matrix or data frame of predictor variables.
#' @param f a formula to determine the features to be used. Has a default, so can be ignored
#' @param regmult a constant to adjust regularization.
#' @param lambda Nimble code
#' @param regfun a function to compute regularization constant for each feature. Defaults tov maxnet's default
#' @param addsamplestobackground if T, add to the background any presence sample that is not already there.
#' @param ... Other arguments, passed to \code{maxnet.formula()}
#' @return A list with lists Const, Data, and Inits
#' @examples
#' \dontrun{
#' NimbleMaxEnt(Data=ToNimble, code=MaxNetcode, adaptInterval=1e3,
#' nchains=1, nburnin = 1e3, niter=6e3, thin=1)
#' }

SetUpMaxEnt <- function(p, data, f = maxnet.formula(p, data, ...), regmult = 1,
                        lambda=1, regfun = maxnet.default.regularization,
                        addsamplestobackground=TRUE, ...) {
  if (anyNA(data))
    stop("NA values in data table. Please remove them and rerun.")
  if (addsamplestobackground) {
    pdata <- data[p == 1, ]
    ndata <- data[p == 0, ]
    toadd <- apply(pdata, 1, function(rr) !any(apply(ndata,
                                                     1, function(r) identical(r, rr))))
    p <- c(p, rep(0, sum(toadd)))
    data <- rbind(data, pdata[toadd, ])
  }

  # Set up data for Maxnet & Nimble.
  mm <- model.matrix(f, data)
  reg <- regfun(p, mm) * regmult
  weights <- p + (1 - p) * 100

  Const <- list(n = nrow(mm), k = ncol(mm), MeanLambda = lambda,
                zeros=rep(0,ncol(mm)))
  Data = list(y = p, X = mm, W=rep(1,nrow(mm)), reg=reg)
  Inits = list(alpha = rnorm(1, log(1/(1+1/mean(p))), 1),
               beta = rnorm(Const$k, 0, sd=0.1), sigma2=rep(1, ncol(mm)))

  res <- list(Const=Const, Data=Data, Inits=Inits)
  res
}

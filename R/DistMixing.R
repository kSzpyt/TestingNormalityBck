#' Sampling from two distributions
#' @export
#' @description This function is used to generate samples from two distributions and combining them.
#' @param n sample length
#' @param family_1 name of the fist distribution
#' @param par_1 list of parameters 1
#' @param family_2 name of the second distribution
#' @param par_2 list of parameters 2
#' @param p percentage split of data
#' @details asbnfkjasbfkjasbdf
#' @return Function returns parameters of used distributions and combinde sample
#' \item{fam1}{Name of first distribution}
#' \item{pars1}{Parameters of first distribution}
#' \item{fam2}{Name of second distribution}
#' \item{pars2}{Parameters of second distribution}


DistMixing <- function(n, family_1, par_1, family_2, par_2, p = .5) UseMethod("DistMixing")


DistMixing.default <- function(n, family_1, par_1, family_2, par_2, p = .5){

  dist_names <- c("beta", "binom", "cauchy", "chisq", "exp", "f",
                  "gamma", "geom", "hyper", "logis", "lnorm",
                  "nbinom", "norm", "pois", "t", "tukey", "unif",
                  "weibull", "wilcox", "signrank")

  stopifnot(is.numeric(n))
  stopifnot(is.numeric(p) && p > 0)
  stopifnot(is.list(par_1) | is.vector(par_1))
  stopifnot(is.list(par_2) | is.vector(par_2))
  stopifnot(family_1 %in% dist_names)
  stopifnot(family_2 %in% dist_names)

  index <- sample(c(1,2), n, prob = c(p, 1-p), replace = TRUE)
  x1 <- length(index[index == 1])
  x2 <- length(index[index == 2])

  eval(parse(text = paste0("my_rfam1 <- r", family_1)))
  result1 <- do.call(my_rfam1, as.list(c(x1, par_1)))

  eval(parse(text = paste0("my_rfam2 <- r", family_2)))
  result2 <- do.call(my_rfam2, as.list(c(x2, par_2)))
  res <- list(vec = c(result1, result2), fam1 = family_1, fam2 = family_2,
              pars1 = unlist(par_1), pars2 = unlist(par_2))


  class(res)<-append(class(res),'DistMixing')

  return(res)
}

print.DistMixing <- function(x){
  # dodawanie sprawdzania czy x jest klasy distmixing
  cat("Mix of following distributions:\n")
  print(x$fam1)
  cat("With following parameters:\n")
  print(x$pars1)
  cat("and distribution:\n")
  print(x$fam2)
  cat("With following parameters:\n")
  print(x$pars2)
  cat("Generated sample:\n")
  print(x$vec)
}

summary.DistMixing <- function(x){

  meanValue <- mean(x$vec)
  sdValue <- sd(x$vec)
  minValue <- min(x$vec)
  maxValue <- max(x$vec)
  kurtosisValue <- moments::kurtosis(x$vec)
  skewValue <- moments::skewness(x$vec)

  round(data.frame(meanValue, sdValue, minValue, maxValue, kurtosisValue, skewValue),3)

}

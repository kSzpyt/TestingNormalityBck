#' Return data frame with results fasfasfasfasfasf
#' @export
#' @param x sample to test
#' @details asbnfkjasbfkjasbdf
#' @return Function returns set of outputs (p.value) from normality tests:
#' \itemize{
#'     \item Shapiro
#'     \item JarqueBera
#'     \item AndersonDarling
#'     \item KolmogorovSmirnov
#'     \item PearsonChiSquare
#' }


NormalityTesting <- function(x) UseMethod("NormalityTesting")

NormalityTesting.default <- function(x){

  stopifnot(is.numeric(x))

  shapiro.p <- shapiro.test(x)$p.value
  jb.p <- tseries::jarque.bera.test(x)$p.value
  ad.p <- nortest::ad.test(x)$p.value
  ks.p <- nortest::lillie.test(x)$p.value
  chi.p <- nortest::pearson.test(x)$p.value
  res <- round(c("Shapiro" = shapiro.p,
                 "JarqueBera" = jb.p,
                 "AndersonDarling" = ad.p,
                 "KolmogorovSmirnov" = ks.p,
                 "PearsonChiSquare" = chi.p),4)

  class(res) <- append(class(res), 'NormalityTesting')
  return(res)

}

NormalityTesting.DistMixing <- function(x){

  shapiro.p <- shapiro.test(x$vec)$p.value
  jb.p <- tseries::jarque.bera.test(x$vec)$p.value
  ad.p <- nortest::ad.test(x$vec)$p.value
  ks.p <- nortest::lillie.test(x$vec)$p.value
  chi.p <- nortest::pearson.test(x$vec)$p.value
  res <- round(c("Shapiro" = shapiro.p,
                 "JarqueBera" = jb.p,
                 "AndersonDarling" = ad.p,
                 "KolmogorovSmirnov" = ks.p,
                 "PearsonChiSquare" = chi.p),4)

  class(res) <- append(class(res), 'NormalityTesting')
  return(res)

}

print.NormalityTesting <- function(x){
  cat("Result of normality tests (H0 - distribution is normal):\n")
  print(data.frame(p.value = x))
}

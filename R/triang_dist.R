#' Triangular Distribution Density
#' @param x Vector of quantiles
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#' @return Density values
#' @export
dtriang <- function(x, min, max, mode) {
  if (any(min > max)) stop("min must be less than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")

  h <- 2 / (max - min)

  # Usamos ifelse para vectorización completa
  dens <- ifelse(x < min | x > max, 0,
                 ifelse(x <= mode, (2 * (x - min)) / ((max - min) * (mode - min)),
                        (2 * (max - x)) / ((max - min) * (max - mode))))
  dens
}

#' Triangular Distribution Function
#' @param q Vector of quantiles
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#' @return Cumulative probabilities
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min > max)) stop("min must be less than max")

  prob <- ifelse(q < min, 0,
                 ifelse(q < mode, (q - min)^2 / ((max - min) * (mode - min)),
                        ifelse(q <= max, 1 - (max - q)^2 / ((max - min) * (max - mode)), 1)))
  prob
}

#' Triangular Quantile Function
#' @param p Vector of probabilities
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#' @return Quantiles
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")

  p_corte <- (mode - min) / (max - min)

  cuantil <- ifelse(p < p_corte,
                    min + sqrt(p * (max - min) * (mode - min)),
                    max - sqrt((1 - p) * (max - min) * (max - mode)))
  cuantil
}

#' Random Generation for Triangular Distribution
#' @param n Number of observations
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#' @return Random variates
#' @export
rtriang <- function(n, min, max, mode) {
  # Implementación mediante el método de inversión [cite: 75, 77]
  u <- runif(n)
  qtriang(u, min, max, mode)
}

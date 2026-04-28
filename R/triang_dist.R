#' @export
dtriang <- function(x, min, max, mode) {
  if (min > max) stop("El minimo no puede ser mayor que el maximo")
  if (mode < min || mode > max) stop("La moda debe estar entre min y max")

  #un vector de ceros del mismo tamaño que x
  resultado <- rep(0, length(x))

  #altura h
  h <- 2 / (max - min)

  for (i in 1:length(x)) {
    cur_x <- x[i]

    if (cur_x >= min && cur_x < mode) {
      resultado[i] <- (2 * (cur_x - min)) / ((max - min) * (mode - min))
    } else if (cur_x == mode) {
      resultado[i] <- h
    } else if (cur_x > mode && cur_x <= max) {
      resultado[i] <- (2 * (max - cur_x)) / ((max - min) * (max - mode))
    }
  }

  return(resultado)
}

#' @export
ptriang <- function(q, min, max, mode) {
  if (min > max) stop("Error en los limites")

  prob <- rep(0, length(q))

  for (i in 1:length(q)) {
    cur_q <- q[i]

    if (cur_q < min) {
      prob[i] <- 0
    } else if (cur_q >= min && cur_q < mode) {
      prob[i] <- (cur_q - min)^2 / ((max - min) * (mode - min))
    } else if (cur_q >= mode && cur_q <= max) {
      prob[i] <- 1 - (max - cur_q)^2 / ((max - min) * (max - mode))
    } else {
      prob[i] <- 1
    }
  }
  return(prob)
}

#' @export
qtriang <- function(p, min, max, mode) {
  if (any(p < 0 | p > 1)) stop("La probabilidad p debe estar entre 0 y 1")

  # Punto de corte en la probabilidad
  p_corte <- (mode - min) / (max - min)
  cuantil <- rep(0, length(p))

  for (i in 1:length(p)) {
    if (p[i] < p_corte) {
      cuantil[i] <- min + sqrt(p[i] * (max - min) * (mode - min))
    } else {
      cuantil[i] <- max - sqrt((1 - p[i]) * (max - min) * (max - mode))
    }
  }
  return(cuantil)
}

#' @export
rtriang <- function(n, min, max, mode) {

  u <- runif(n)
  return(qtriang(u, min, max, mode))
}

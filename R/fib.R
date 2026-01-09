fib_before_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || n %% 1 != 0) {
    stop("n must be a single integer", call. = FALSE)
  }
  if (n <= 0) {
    stop("n must be a positive integer", call. = FALSE)
  }

  fibs <- c(0L, 1L)

  while (tail(fibs, 1) < n) {
    fibs <- c(fibs, fibs[length(fibs)] + fibs[length(fibs) - 1])
  }

  fibs[fibs < n]
}

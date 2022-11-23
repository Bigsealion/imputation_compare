create.data <- function(beta = 1, sigma2 = 1, n = 50,
                        run = 1) {
  set.seed(seed = run)
  x <- rnorm(n)
  y <- beta * x + rnorm(n, sd = sqrt(sigma2))
  cbind(x = x, y = y)
}

make.missing <- function(data, p = 0.5){
  rx <- rbinom(nrow(data), 1, p)
  data[rx == 0, "x"] <- NA
  data
}

test.impute <- function(data, m = 5, method = "norm", ...) {
  imp <- mice(data, method = method, m = m, print = FALSE, ...)
  fit <- with(imp, lm(y ~ x))
  tab <- summary(pool(fit), "all", conf.int = TRUE)  # if m==1, parameter 'all' will return a ERROR
  as.numeric(tab[2, c("estimate", "2.5 %", "97.5 %")])
}

simulate <- function(runs = 10) {
  res <- array(NA, dim = c(2, runs, 3))
  dimnames(res) <- list(c("norm.predict", "norm.nob"),
                        as.character(1:runs),
                        c("estimate", "2.5 %","97.5 %"))
  for(run in 1:runs) {
    data <- create.data(run = run)
    data <- make.missing(data)
    res[1, run, ] <- test.impute(data, method = "norm.predict",
                                 m = 2)
    res[2, run, ] <- test.impute(data, method = "norm.nob")
  }
  res
}

res <- simulate(10)
apply(res, c(1, 3), mean, na.rm = TRUE)
true <- 1
RB <- rowMeans(res[,, "estimate"]) - true
PB <- 100 * abs((rowMeans(res[,, "estimate"]) - true)/ true)
CR <- rowMeans(res[,, "2.5 %"] < true & true < res[,, "97.5 %"])
AW <- rowMeans(res[,, "97.5 %"] - res[,, "2.5 %"])

RMSE <- sqrt(rowMeans((res[,, "estimate"] - true)^2))
data.frame(RB, PB, CR, AW, RMSE)

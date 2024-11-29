df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
nll_lm <- function(par, data) {
  beta <- par[1:4]
  sigma <- par[5]
  X <- cbind(1, data$x1, data$x2, data$x3)
  y_pred <- X %*% beta
  nll <- -sum(dnorm(data$y, mean = y_pred, sd = sigma, log = TRUE))

  return(nll)
}
initial_beta <- c(mean(df$y), 0, 0, 0)
initial_sigma <- sd(df$y)
initial_par <- c(init_beta, init_sigma)
fit <- optim(
  par = init_par,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = c(rep(-Inf, 4), 1e-6),
  upper = rep(Inf, 5)
)
mle_beta <- fit$par[1:4]
mle_sigma <- fit$par[5]
mle_beta
mle_sigma
X <- cbind(1, df$x1, df$x2, df$x3)

beta_matrix <- solve(t(X) %*% X) %*% t(X) %*% df$y
beta_matrix
mle_beta
residuals <- df$y - X %*% beta_matrix
sigma_matrix <- sqrt(sum(residuals^2) / nrow(df))
sigma_matrix
mle_sigma
fit_hessian <- optim(
  par = init_par,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = c(rep(-Inf, 4), 1e-6),
  upper = rep(Inf, 5),
  hessian = TRUE
)
vcov_matrix <- solve(fit_hessian$hessian)
se <- sqrt(diag(vcov_matrix))
se[1:4]
lm_fit <- lm(y ~ x1 + x2 + x3, data = df)
lm_beta <- coef(lm_fit)
lm_sigma <- summary(lm_fit)$sigma



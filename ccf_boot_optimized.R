ccf_boot_optimized <- function(x,
                               y,
                               lag.max = NULL,
                               plot = c("Pearson", "Spearman", "none"),
                               level = 0.95,
                               B = 1000,
                               smooth = NULL,  
                               cl = 1L, 
                               ...) {
  
  # Perform various checks
  namex <- deparse(substitute(x))[1L]
  namey <- deparse(substitute(y))[1L]
  if (is.matrix(x) || is.matrix(y)) {
    stop("x and y should be univariate time series only.")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("data should not contain missing values.")
  }
  nx <- length(x)
  ny <- length(y)
  B <- as.integer(B)
  if (B <= 0) {
    stop("number of bootstrap resamples B must be positive.")
  }
  plt <- match.arg(plot)
  
  # Parallelization and smoothing decision
  bootparallel <- nx >= 5000 || ny >= 5000
  if (is.null(smooth)) smooth <- bootparallel
  
  # Function for bootstrap calculation
  bootstrap_func <- function(b, phetax, phetay, Zx, Zy, nx, ny, lag.max) {
    xboot <- arima.sim(list(order = c(length(phetax), 0, 0), ar = phetax), n = nx,
                       innov = sample(Zx, size = nx, replace = TRUE))
    yboot <- arima.sim(list(order = c(length(phetay), 0, 0), ar = phetay), n = ny,
                       innov = sample(Zy, size = ny, replace = TRUE))
    xrankboot <- rank(xboot)
    yrankboot <- rank(yboot)
    attributes(xboot) <- attributes(xrankboot) <- attributes(x)
    attributes(yboot) <- attributes(yrankboot) <- attributes(y)
    rPboot <- ccf(xboot, yboot, lag.max = lag.max, plot = FALSE)$acf[, 1, 1]
    rSboot <- ccf(xrankboot, yrankboot, lag.max = lag.max, plot = FALSE)$acf[, 1, 1]
    cbind(rPboot, rSboot)
  }
  
  ### Function logic for creating ranks and filtering
  xrank <- rank(x)
  yrank <- rank(y)
  attributes(xrank) <- attributes(x)
  attributes(yrank) <- attributes(y)
  tmp <- ccf(x, y, lag.max = lag.max, plot = FALSE)
  lags <- tmp$lag[, 1, 1]
  rP <- tmp$acf[, 1, 1]
  rS <- ccf(xrank, yrank, lag.max = lag.max, plot = FALSE)$acf[, 1, 1]
  phetax <- ARest(x, ...)
  phetay <- ARest(y, ...)
  
  # AR filtering
  Zx <- if (length(phetax) > 0) {
    names(phetax) <- paste0(rep("phi_", length(phetax)), 1:length(phetax))
    tmp <- stats::filter(x, phetax, sides = 1)
    x[(length(phetax) + 1):nx] - tmp[length(phetax):(nx - 1)]
  } else x
  Zx <- Zx - mean(Zx)
  
  Zy <- if (length(phetay) > 0) {
    names(phetay) <- paste0(rep("phi_", length(phetay)), 1:length(phetay))
    tmp <- stats::filter(y, phetay, sides = 1)
    y[(length(phetay) + 1):ny] - tmp[length(phetay):(ny - 1)]
  } else y
  Zy <- Zy - mean(Zy)
  
  # Bootstrap with parallelization (if applicable)
  if (bootparallel) {
    if (is.null(cl) || cl == 1L) {
      cl <- parallel::makeCluster(parallel::detectCores())
    }
    CCFs <- parallel::parSapply(cl, 1:B, bootstrap_func, phetax, phetay, Zx, Zy, nx, ny, lag.max, simplify = "array")
    parallel::stopCluster(cl)
  } else {
    CCFs <- sapply(1:B, bootstrap_func, phetax, phetay, Zx, Zy, nx, ny, lag.max, simplify = "array")
  }
  
  # Confidence regions
  alpha <- 1 - level
  crP <- apply(CCFs[, 1, ], 1, function(x) qnorm(1 - alpha / 2, sd = sd(x)))
  if (smooth) crP <- loess(crP ~ lags, span = 0.25)$fitted
  crP <- rbind(-crP, crP)
  
  crS <- apply(CCFs[, 2, ], 1, function(x) qnorm(1 - alpha / 2, sd = sd(x)))
  if (smooth) crS <- loess(crS ~ lags, span = 0.25)$fitted
  crS <- rbind(-crS, crS)
  
  RESULT <- data.frame(Lag = lags,
                       r_P = rP,
                       lower_P = crP[1,], upper_P = crP[2,], # Pearson
                       r_S = rS,
                       lower_S = crS[1,], upper_S = crS[2,]) # Spearman
  
  # Plotting
  if (plt == "Pearson") {
    TMP <- RESULT[, grepl("_P", names(RESULT))]
  }
  if (plt == "Spearman") {
    TMP <- RESULT[, grepl("_S", names(RESULT))]
  }
  if (plt == "Pearson" || plt == "Spearman") {
    matplot(lags, TMP, type = "n",
            xlab = "Lag", ylab = "CCF",
            main = paste0(plt, " correlation of ", namex, "(t + Lag)", " and ", namey, "(t)\n",
                          "with ", level*100, "% bootstrap confidence region"),
            las = 1)
    grid(nx = 2, ny = NULL, lty = 1)
    
    polygon(x = c(lags, rev(lags)),
            y = c(TMP[, 2], rev(TMP[, 3])),
            col = adjustcolor("deepskyblue", alpha.f = 0.80),
            border = NA)
    
    lines(lags, TMP[, 1], type = "h")
    
    isoutside <- (TMP[, 1] < TMP[, 2]) | (TMP[, 3] < TMP[, 1])
    points(lags, TMP[, 1], pch = c(1, 16)[1 + isoutside])
    
    return(invisible(RESULT))
  } else {
    return(RESULT)
  }
}



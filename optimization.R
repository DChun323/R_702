library(funtimes)
library(parallel)

set.seed(123)
x <- rnorm(1000)
y <- rnorm(1000)


result_original <- ccf_boot(x, y, lag.max = 10, plot = "Pearson", B = 1000)
result_optimized <- ccf_boot_optimized3(x, y, lag.max = 10, plot = "Pearson", B = 1000)


system.time({
  result_original <- ccf_boot(x, y, lag.max = 10, plot = "Pearson", B = 1000)
})


system.time({
  result_optimized <- ccf_boot_optimized2(x, y, lag.max = 10, plot = "Pearson", B = 1000)
})

identical(result_original, result_optimized)

all.equal(result_original, result_optimized)



set.seed(123)
x1 <- rnorm(10000)
y1 <- rnorm(10000)

system.time({
  result_original <- ccf_boot(x1, y1, lag.max = 10, plot = "Pearson", B = 1000)
})


system.time({
  result_optimized <- ccf_boot_optimized3(x1, y1, lag.max = 10, plot = "Pearson", B = 1000)
})


system.time({
  result_optimized <- ccf_boot_optimized4(x1, y1, lag.max = 10, plot = "Pearson", B = 1000)
})

summary(result_original)

summary(result_optimized)








ccf_boot_optimized2 <- function(x,
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
  
  # parallelization and smoothing decision
  if (nx < 1000 && ny < 1000) {
    bootparallel <- FALSE  
    smooth <- FALSE  
  } else {
    bootparallel <- TRUE  
    if (is.null(smooth)) smooth <- TRUE  
  }
  
  ### Function
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
  if (length(phetax) > 0) {
    names(phetax) <- paste0(rep("phi_", length(phetax)), c(1:length(phetax)))
    tmp <- stats::filter(x, phetax, sides = 1)
    Zx <- x[(length(phetax) + 1):nx] - tmp[length(phetax):(nx - 1)]
  } else {
    Zx <- x
  }
  Zx <- Zx - mean(Zx)
  if (length(phetay) > 0) {
    names(phetay) <- paste0(rep("phi_", length(phetay)), c(1:length(phetay)))
    tmp <- stats::filter(y, phetay, sides = 1)
    Zy <- y[(length(phetay) + 1):ny] - tmp[length(phetay):(ny - 1)]
  } else {
    Zy <- y
  }
  Zy <- Zy - mean(Zy)
  
  ### Bootstrap
  if (bootparallel) {
    if (is.null(cl) || cl == 1L) {
      cores <- parallel::detectCores()  
      cl <- parallel::makeCluster(cores)  
    }
    
    CCFs <- parallel::parSapply(cl, 1:B, function(b) {
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
    }, simplify = "array")
    
    parallel::stopCluster(cl)
  } else {
    CCFs <- sapply(1:B, function(b) {
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
    }, simplify = "array")
  }
  
  # Confidence regions
  alpha <- 1 - level
  crP <- apply(CCFs[, 1, ], 1, function(x) qnorm(1 - alpha / 2, sd = sd(x)))
  if (smooth) {
    crP <- loess(crP ~ lags, span = 0.25)$fitted
  }
  crP <- rbind(-crP, crP)
  
  crS <- apply(CCFs[, 2, ], 1, function(x) qnorm(1 - alpha / 2, sd = sd(x)))
  if (smooth) {
    crS <- loess(crS ~ lags, span = 0.25)$fitted
  }
  crS <- rbind(-crS, crS)
  
  RESULT <- data.frame(Lag = lags,
                       r_P = rP,
                       lower_P = crP[1,], upper_P = crP[2,], # Pearson
                       r_S = rS,
                       lower_S = crS[1,], upper_S = crS[2,]) # Spearman
  
  ### Plotting
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






ccf_boot_optimized3 <- function(x,
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
  
  # Check for matrix inputs
  if (is.matrix(x) || is.matrix(y)) {
    stop("x and y should be univariate time series only.")
  }
  
  # Handle missing values
  if (any(is.na(x)) || any(is.na(y))) {
    stop("data should not contain missing values.")
  }
  
  # Get lengths of x and y
  nx <- length(x)
  ny <- length(y)
  B <- as.integer(B)
  
  # Check for valid bootstrap sample size
  if (B <= 0) {
    stop("number of bootstrap resamples B must be positive.")
  }
  
  # Set plotting method
  plt <- match.arg(plot)
  
  # Parallelization and smoothing decision
  if (nx < 5000 && ny < 5000) {
    bootparallel <- FALSE  
    smooth <- FALSE  
  } else {
    bootparallel <- TRUE  
    if (is.null(smooth)) smooth <- TRUE  
  }
  
  # Compute ranks of x and y
  xrank <- rank(x)
  yrank <- rank(y)
  attributes(xrank) <- attributes(x)
  attributes(yrank) <- attributes(y)
  
  # Compute cross-correlation for original data
  tmp <- ccf(x, y, lag.max = lag.max, plot = FALSE)
  lags <- tmp$lag[, 1, 1]
  rP <- tmp$acf[, 1, 1]
  rS <- ccf(xrank, yrank, lag.max = lag.max, plot = FALSE)$acf[, 1, 1]
  
  # AR model residuals for x and y
  phetax <- ARest(x, ...)
  phetay <- ARest(y, ...)
  
  # Apply AR filtering on x if necessary
  Zx <- if (length(phetax) > 0) {
    names(phetax) <- paste0(rep("phi_", length(phetax)), c(1:length(phetax)))
    tmp <- stats::filter(x, phetax, sides = 1)
    x[(length(phetax) + 1):nx] - tmp[length(phetax):(nx - 1)]
  } else {
    x
  }
  
  Zx <- Zx - mean(Zx)
  
  # Apply AR filtering on y if necessary
  Zy <- if (length(phetay) > 0) {
    names(phetay) <- paste0(rep("phi_", length(phetay)), c(1:length(phetay)))
    tmp <- stats::filter(y, phetay, sides = 1)
    y[(length(phetay) + 1):ny] - tmp[length(phetay):(ny - 1)]
  } else {
    y
  }
  
  Zy <- Zy - mean(Zy)
  
  ### Bootstrap
  if (bootparallel) {
    if (is.null(cl) || cl == 1L) {
      cl <- parallel::makeCluster(parallel::detectCores())  # Dynamically detect cores
    }
    
    CCFs <- parallel::parSapply(cl, 1:B, function(b) {
      xboot <- arima.sim(list(order = c(length(phetax), 0, 0), ar = phetax), n = nx,
                         innov = sample(Zx, size = nx, replace = TRUE))
      yboot <- arima.sim(list(order = c(length(phetay), 0, 0), ar = phetay), n = ny,
                         innov = sample(Zy, size = ny, replace = TRUE))
      
      # Rank bootstrap samples
      xrankboot <- rank(xboot)
      yrankboot <- rank(yboot)
      attributes(xboot) <- attributes(xrankboot) <- attributes(x)
      attributes(yboot) <- attributes(yrankboot) <- attributes(y)
      
      # Compute Pearson and Spearman correlations for bootstrapped samples
      rPboot <- ccf(xboot, yboot, lag.max = lag.max, plot = FALSE)$acf[, 1, 1]
      rSboot <- ccf(xrankboot, yrankboot, lag.max = lag.max, plot = FALSE)$acf[, 1, 1]
      cbind(rPboot, rSboot)
    }, simplify = "array")
    
    parallel::stopCluster(cl)
  } else {
    CCFs <- sapply(1:B, function(b) {
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
    }, simplify = "array")
  }
  
  # Confidence intervals for Pearson and Spearman correlations
  alpha <- 1 - level
  crP <- apply(CCFs[, 1, ], 1, function(x) qnorm(1 - alpha / 2, sd = sd(x)))
  crS <- apply(CCFs[, 2, ], 1, function(x) qnorm(1 - alpha / 2, sd = sd(x)))
  
  if (smooth) {
    crP <- loess(crP ~ lags, span = 0.25)$fitted
    crS <- loess(crS ~ lags, span = 0.25)$fitted
  }
  
  # Confidence regions
  crP <- rbind(-crP, crP)
  crS <- rbind(-crS, crS)
  
  RESULT <- data.frame(Lag = lags,
                       r_P = rP,
                       lower_P = crP[1,], upper_P = crP[2,], # Pearson
                       r_S = rS,
                       lower_S = crS[1,], upper_S = crS[2,]) # Spearman
  
  # Plotting
  if (plt == "Pearson" || plt == "Spearman") {
    TMP <- RESULT[, grepl(plt, names(RESULT))]
    matplot(lags, TMP, type = "n",
            xlab = "Lag", ylab = "CCF",
            main = paste0(plt, " correlation of ", namex, "(t + Lag)", " and ", namey, "(t)\n",
                          "with ", level * 100, "% bootstrap confidence region"),
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

#################################################################################
library(future.apply)

ccf_boot_opti <- function(x,
                     y,
                     lag.max = NULL,
                     plot = c("Pearson", "Spearman", "none"),
                     level = 0.95,
                     B = 1000,
                     smooth = FALSE,
                     cl = NULL,
                     ...)
{
  ### Perform various checks
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
  
  ### Setup parallelization
  if (!is.null(cl)) {
    plan(cluster, workers = cl)
  } else {
    plan(multisession)
  }
  
  ### Precompute ranks
  xrank <- rank(x)
  yrank <- rank(y)
  attributes(xrank) <- attributes(x)
  attributes(yrank) <- attributes(y)
  
  ### Compute initial cross-correlations
  tmp <- ccf(x, y, lag.max = lag.max, plot = FALSE)
  lags <- tmp$lag[,1,1]
  rP <- tmp$acf[,1,1]
  rS <- ccf(xrank, yrank, lag.max = lag.max, plot = FALSE)$acf[,1,1]
  
  ### Prewhiten data
  phetax <- ARest(x, ...)
  phetay <- ARest(y, ...)
  
  Zx <- if (length(phetax) > 0) {
    tmp <- stats::filter(x, phetax, sides = 1)
    x[(length(phetax) + 1):nx] - tmp[length(phetax):(nx - 1)]
  } else {
    x
  }
  Zx <- Zx - mean(Zx)
  
  Zy <- if (length(phetay) > 0) {
    tmp <- stats::filter(y, phetay, sides = 1)
    y[(length(phetay) + 1):ny] - tmp[length(phetay):(ny - 1)]
  } else {
    y
  }
  Zy <- Zy - mean(Zy)
  
  ### Bootstrap
  CCFs <- future_sapply(1:B, function(b) {
    xboot <- arima.sim(list(order = c(length(phetax), 0, 0), ar = phetax), n = nx,
                       innov = sample(Zx, size = nx, replace = TRUE))
    yboot <- arima.sim(list(order = c(length(phetay), 0, 0), ar = phetay), n = ny,
                       innov = sample(Zy, size = ny, replace = TRUE))
    xrankboot <- rank(xboot)
    yrankboot <- rank(yboot)
    attributes(xboot) <- attributes(xrankboot) <- attributes(x)
    attributes(yboot) <- attributes(yrankboot) <- attributes(y)
    rPboot <- ccf(xboot, yboot, lag.max = lag.max, plot = FALSE)$acf[,1,1]
    rSboot <- ccf(xrankboot, yrankboot, lag.max = lag.max, plot = FALSE)$acf[,1,1]
    cbind(rPboot, rSboot)
  }, simplify = "array")
  
  ### Confidence regions
  alpha <- 1 - level
  crP <- apply(CCFs[,1,], 1, function(x) qnorm(1 - alpha / 2, sd = sd(x)))
  if (smooth) {
    crP <- loess(crP ~ lags, span = 0.25)$fitted
  }
  crP <- rbind(-crP, crP)
  
  crS <- apply(CCFs[,2,], 1, function(x) qnorm(1 - alpha / 2, sd = sd(x)))
  if (smooth) {
    crS <- loess(crS ~ lags, span = 0.25)$fitted
  }
  crS <- rbind(-crS, crS)
  
  ### Result
  RESULT <- data.frame(Lag = lags,
                       r_P = rP, 
                       lower_P = crP[1,], upper_P = crP[2,], 
                       r_S = rS, 
                       lower_S = crS[1,], upper_S = crS[2,])
  
  ### Plotting
  if (plt == "Pearson" || plt == "Spearman") {
    TMP <- RESULT[,grepl(ifelse(plt == "Pearson", "_P", "_S"), names(RESULT))]
    matplot(lags, TMP, type = "n",
            xlab = "Lag", ylab = "CCF",
            main = paste0(plt, " correlation of ", namex, "(t + Lag)", " and ", namey, "(t)\n",
                          "with ", level*100, "% bootstrap confidence region"),
            las = 1)
    grid(nx = 2, ny = NULL, lty = 1)
    polygon(x = c(lags, rev(lags)),
            y = c(TMP[,2], rev(TMP[,3])),
            col =  adjustcolor("deepskyblue", alpha.f = 0.80),
            border = NA)
    lines(lags, TMP[,1], type = "h")
    isoutside <- (TMP[,1] < TMP[,2]) | (TMP[,3] < TMP[,1])
    points(lags, TMP[,1], pch = c(1, 16)[1 + isoutside])
    return(invisible(RESULT))
  } else {
    return(RESULT)
  }
  
  ### Cleanup
  if (is.null(cl)) plan(sequential)
}



#####################################################################################
library(future.apply)

ccf_boot2 <- function(x,
                     y,
                     lag.max = NULL,
                     plot = c("Pearson", "Spearman", "none"),
                     level = 0.95,
                     B = 1000,
                     smooth = FALSE,
                     cl = NULL,
                     ...)
{
  ### Perform various checks
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
  
  ### Setup parallelization
  if (!is.null(cl)) {
    plan(cluster, workers = cl)
  } else {
    plan(multisession)
  }
  
  ### Precompute ranks
  xrank <- rank(x)
  yrank <- rank(y)
  attributes(xrank) <- attributes(x)
  attributes(yrank) <- attributes(y)
  
  ### Compute initial cross-correlations
  tmp <- ccf(x, y, lag.max = lag.max, plot = FALSE)
  lags <- tmp$lag[,1,1]
  rP <- tmp$acf[,1,1]
  rS <- ccf(xrank, yrank, lag.max = lag.max, plot = FALSE)$acf[,1,1]
  
  ### Prewhiten data
  phetax <- ARest(x, ...)
  phetay <- ARest(y, ...)
  
  prewhiten <- function(series, pheta) {
    if (length(pheta) > 0) {
      tmp <- stats::filter(series, pheta, sides = 1)
      result <- series[(length(pheta) + 1):length(series)] - tmp[length(pheta):(length(series) - 1)]
    } else {
      result <- series
    }
    result - mean(result)
  }
  
  Zx <- prewhiten(x, phetax)
  Zy <- prewhiten(y, phetay)
  
  ### Bootstrap
  CCFs <- future_sapply(1:B, function(b) {
    xboot <- arima.sim(list(order = c(length(phetax), 0, 0), ar = phetax), n = nx,
                       innov = sample(Zx, size = nx, replace = TRUE))
    yboot <- arima.sim(list(order = c(length(phetay), 0, 0), ar = phetay), n = ny,
                       innov = sample(Zy, size = ny, replace = TRUE))
    xrankboot <- rank(xboot)
    yrankboot <- rank(yboot)
    attributes(xboot) <- attributes(xrankboot) <- attributes(x)
    attributes(yboot) <- attributes(yrankboot) <- attributes(y)
    rPboot <- ccf(xboot, yboot, lag.max = lag.max, plot = FALSE)$acf[,1,1]
    rSboot <- ccf(xrankboot, yrankboot, lag.max = lag.max, plot = FALSE)$acf[,1,1]
    cbind(rPboot, rSboot)
  }, simplify = "array")
  
  ### Confidence regions
  alpha <- 1 - level
  calc_cr <- function(CCFs, smooth, lags) {
    cr <- apply(CCFs, 1, function(x) quantile(x, probs = c(alpha / 2, 1 - alpha / 2)))
    if (smooth) {
      cr <- apply(cr, 1, function(row) loess(row ~ lags, span = 0.25)$fitted)
    }
    cr
  }
  
  crP <- calc_cr(CCFs[,1,], smooth, lags)
  crS <- calc_cr(CCFs[,2,], smooth, lags)
  
  ### Result
  RESULT <- data.frame(Lag = lags,
                       r_P = rP, 
                       lower_P = crP[1,], upper_P = crP[2,], 
                       r_S = rS, 
                       lower_S = crS[1,], upper_S = crS[2,])
  
  ### Plotting
  if (plt == "Pearson" || plt == "Spearman") {
    TMP <- RESULT[,grepl(ifelse(plt == "Pearson", "_P", "_S"), names(RESULT))]
    matplot(lags, TMP, type = "n",
            xlab = "Lag", ylab = "CCF",
            main = paste0(plt, " correlation of ", namex, "(t + Lag)", " and ", namey, "(t)\n",
                          "with ", level*100, "% bootstrap confidence region"),
            las = 1)
    grid(nx = 2, ny = NULL, lty = 1)
    polygon(x = c(lags, rev(lags)),
            y = c(TMP[,2], rev(TMP[,3])),
            col =  adjustcolor("deepskyblue", alpha.f = 0.80),
            border = NA)
    lines(lags, TMP[,1], type = "h")
    isoutside <- (TMP[,1] < TMP[,2]) | (TMP[,3] < TMP[,1])
    points(lags, TMP[,1], pch = c(1, 16)[1 + isoutside])
    return(invisible(RESULT))
  } else {
    return(RESULT)
  }
  
  ### Cleanup
  if (is.null(cl)) plan(sequential)
}

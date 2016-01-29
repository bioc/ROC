# ROC curve
    setClass("rocc", representation(sens = "numeric", spec = "numeric",
        rule = "function", cuts = "numeric", markerLabel = "character",
        caseLabel = "character"))
    setGeneric("plot")

# sept 2007 -- options to be supported are add, jit, line, show.thresh
# they should be defaulted and all else goes legitimately to plot.default

setMethod("plot", "rocc", function(x, y, ..., add=FALSE, jit=FALSE, 
  line=TRUE, show.thresh=FALSE, threshCex=.7, threshYsh=-0.04,
  threshDig=2 ) {
  rocObj = x
  x = 1 - rocObj@spec
  y = rocObj@sens
  if (jit) {
	x = jitter(x)
	y = jitter(y)
	}
  if (add) {
    if (line) lines(x, y, ...)
    else points(x, y, ...)
    }
  else if (!add) {
    if (line) plot(x, y, type="l", ...)
    else plot(x, y, ...)
  }
  else stop("supplied add parameter not logical")
  if (show.thresh) text(x, y + threshYsh, 
                as.character(round(rocObj@cuts, threshDig)),
                cex =threshCex)
  invisible(list(x.used=x, y.used=y))
})

setGeneric("lines", useAsDefault=lines)
setMethod("lines", "rocc", function(x, y, ..., jit=FALSE,
  show.thresh = FALSE, threshCex=.7, threshYsh=-0.04,
  threshDig=2 ) {
   plot(x, add=TRUE, line=TRUE, jit=jit, show.thresh=show.thresh,
      threshCex=threshCex, threshYsh=threshYsh, threshDig=threshDig,
      ...) })

setGeneric("points", useAsDefault=points)
setMethod("points", "rocc", function(x, y, ..., jit=FALSE,
  show.thresh = FALSE, threshCex=.7, threshYsh=-0.04,
  threshDig=2 ) {
   plot(x, add=TRUE, line=FALSE, jit=jit, show.thresh=show.thresh,
      threshCex=threshCex, threshYsh=threshYsh, threshDig=threshDig,
      ...) })
  
  

##    setMethod("plot", c("rocc", "missing"), function (x, y, ...)
#    {
#        nna <- function(x) !is.na(x)
#        object <- x
#        dots <- list(...)
#        dNames <- names(dots)
#        show.thresh <- FALSE
#        add <- FALSE
#        jit <- FALSE
#        line <- FALSE
#        if (nna(ind <- match("show.thresh", dNames))) {
#            show.thresh <- dots$show.thresh
#            dots <- dots[-ind]
#            dNames <- dNames[-ind]
#        }
#        if (nna(ind <- match("add", dNames))) {
#            add <- dots$add
#            dots <- dots[-ind]
#            dNames <- dNames[-ind]
#        }
#        if (nna(ind <- match("jit", dNames))) {
#            jit <- dots$jit
#            dots <- dots[-ind]
#            dNames <- dNames[-ind]
#        }
#        if (nna(ind <- match("line", dNames))) {
#            line <- dots$line
#            dots <- dots[-ind]
#            dNames <- dNames[-ind]
#        }
#        x <- 1 - object@spec
#        y <- object@sens
#        if (!line) {
#            if (add) {
#                if (!jit)
#                    points(x, y, ...)
#                else points(jitter(x), jitter(y), ...)
#            }
#            else {
#                knownArgs <- list(x = x, y = y, xlab = paste("1-spec:",
#                    object@markerLabel), ylab = paste("sens:", object@caseLabel))
#                do.call("plot.default", unlist(list(knownArgs, dots),
#                    recursive = FALSE))
#            }
#        }
#        else {
#            if (add) {
#                if (!jit)
#                    lines(x, y, ...)
#                else lines(jitter(x), jitter(y), ...)
#            }
#            else {
#                knownArgs <- list(x = x, y = y, xlab = paste("1-spec:",
#                    object@markerLabel), ylab = paste("sens:", object@caseLabel),
#                    type = "l")
#                do.call("plot.default", unlist(list(knownArgs, dots),
#                    recursive = FALSE))
#            }
#        }
#        if (show.thresh)
#            text(x, y - 0.04, as.character(round(object@cuts, 3)),
#                cex = 0.7)
#    })

"AUC" <-
function (rocobj)
{
    x <- 1 - rocobj@spec
    y <- rocobj@sens
    if (x[1] > x[length(x)]) {
      x <- rev(x)
      y <- rev(y)
    }
    trapezint(x, y, 0, 1)
}
"AUCi" <-
function (rocobj)
{
    f <- function(x) ROC(rocobj, x)
    integrate(f, 0, 1)$value
}

"ROC" <-
function (rocobj, t0)
{
    approx(1 - rocobj@spec, rocobj@sens, t0, rule = 2, ties = max)$y
}

"dxrule.sca" <-
function (x, thresh)
ifelse(x > thresh, 1, 0)

"pAUC" <-
function (rocobj, t0)
{
    x <- 1 - rocobj@spec
    y <- rocobj@sens
    if (x[1] > x[length(x)]) {
      x <- rev(x)
      y <- rev(y)
    }
    trapezint(x, y, 0, t0)
}

"pAUCi" <-
function (rocobj, t0)
{
    f <- function(x) ROC(rocobj, x)
    integrate(f, 0, t0)$value
}

"rocdemo.sca" <-   function (truth, data, rule=NULL, cutpts = NA,
                             markerLabel = "unnamed marker",
                             caseLabel = "unnamed diagnosis") {
  if (!all(sort(unique(truth)) == c(0, 1)))
    stop("'truth' variable must take values 0 or 1")
  if (any(is.na(cutpts))) {
    message("NA in cutpts forces recomputation using smallest gap")
    udata <- unique(sort(data))
    delta <- min(diff(udata))/2
    cutpts <- c(udata - delta, udata[length(udata)] + delta)
  }
  np <- length(cutpts)
  ## if rule is not given or is dxrule.sca use the faster C implementation
  if(is.null(rule) | isTRUE(all.equal(rule,dxrule.sca))) {
    if (options()$verbose) cat("verbose: using C.\n")
    rocResult <- .C("ROC", as.integer(truth), as.double(data), as.double(cutpts),
                    as.integer(length(truth)),
                    as.integer(length(cutpts)),
                    spec=double(np), sens=double(np), PACKAGE="ROC")
    spec <- rocResult$spec
    sens <- rocResult$sens
    rule <- dxrule.sca
  }
  ## is user provided a rule, use the classical R implementation
  else {
    sens <- rep(NA, np)
    spec <- rep(NA, np)
    for (i in 1:np) {
      pred <- rule(data, cutpts[i])
      sens[i] = mean(pred[truth == 1])
      spec[i] = mean(1 - pred[truth == 0])
    }
  }
  new("rocc", spec = spec, sens = sens, rule = rule, cuts = cutpts,
      markerLabel = markerLabel, caseLabel = caseLabel)
}

"trapezint" <-
function (x, y, a, b)
{
    if (length(x) != length(y))
        stop("length x must equal length y")
    y <- y[x >= a & x <= b]
    x <- x[x >= a & x <= b]
    if (length(unique(x)) < 2)
        return(NA)
    ya <- approx(x, y, a, ties = max, rule = 2)$y
    yb <- approx(x, y, b, ties = max, rule = 2)$y
    x <- c(a, x, b)
    y <- c(ya, y, yb)
    h <- diff(x)
    lx <- length(x)
    0.5 * sum(h * (y[-1] + y[-lx]))
}



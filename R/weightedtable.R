# This code is modified the table function in the R base package
# The base package is part of R which is licensed under GPL-3

#' \code{WeightedTable}
#' @description Generalisation of the \code{table} function in base to handle weights
#' @param ... one or more objects which can be interpretated as factors, or a list or dataframe whose components can be so interpreted
#' @param weights numeric vector of sampling weights
#' @param exclude levels to remove for all factors in \code{...}
#' @param useNA whether to include \code{NA} values in the table
#' @param dnn the names to given to the dimensions in the result
#' @param deparse.level controls how the default \code{dnn} is constructed.
#' @export

WeightedTable <- function (...,
                           weights = NULL,
                           exclude = if (useNA == "no") c(NA, NaN),
                           useNA = c("no", "ifany", "always"),
                           dnn = list.names(...),
                           deparse.level = 1)
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm))
            seq_along(l)
        else nm == ""
        dep <- vapply(l[fixup], function(x) switch(deparse.level +
            1, "", if (is.symbol(x)) as.character(x) else "",
            deparse(x, nlines = 1)[1L]), "")
        if (is.null(nm))
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    if (!missing(exclude) && is.null(exclude))
        useNA <- "always"
    useNA <- match.arg(useNA)
    args <- list(...)
    if (!length(args))
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args))
            dnn <- if (!is.null(argn <- names(args)))
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens))
            lens <- length(a)
        else if (length(a) != lens)
            stop("all arguments must have the same length")
        cat <- if (is.factor(a)) {
            if (any(is.na(levels(a))))
                a
            else {
                if (is.null(exclude) && useNA != "no")
                  addNA(a, ifany = (useNA == "ifany"))
                else {
                  if (useNA != "no")
                    a <- addNA(a, ifany = (useNA == "ifany"))
                  ll <- levels(a)
                  a <- factor(a, levels = ll[!(ll %in% exclude)],
                    exclude = if (useNA == "no")
                      NA)
                }
            }
        }
        else {
            a <- factor(a, exclude = exclude)
            if (useNA != "no")
                addNA(a, ifany = (useNA == "ifany"))
            else a
        }
        nl <- length(ll <- levels(cat))
        dims <- c(dims, nl)
        if (prod(dims) > .Machine$integer.max)
            stop("attempt to make a table with >= 2^31 elements")
        dn <- c(dn, list(ll))
        bin <- bin + pd * (as.integer(cat) - 1L)
        pd <- pd * nl
    }

    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin))
        bin <- bin + 1L
    wbins <- 1
    if (!is.null(weights))
    {
        wbins <- rep(0, pd)
        for (i in 1:length(weights))
           wbins[bin[i]] <- wbins[bin[i]] + weights[i]
        #cat("wbins:", wbins, "\n")
        y <- array(wbins, dims, dimnames = dn)
    } else
    {
        y <- array(tabulate(bin, pd), dims, dimnames = dn)
    }
    class(y) <- "table"
    y
}

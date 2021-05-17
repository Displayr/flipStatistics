#' Scatterplot matrix
#'
#' Creates a matrix of scatterplot from variable pairs
#' @param x A dataframe containing the variables to be plotted.
#' @param seed A seed used to generate random variables for jitter.
#' @param symbol One of "Small dots", "Dots", "Big dots", "Circles".
#' @param fit.type One of "None", "LOESS", or "Straight".
#' @param modifications One of "None", "Jitter", or "Enlarge points with multiple observations".
#' @param weights A numeric vector with length equal to the number of rows in \code{x}.
#' @importFrom graphics pairs abline lines par points rect text
#' @importFrom stats lm loess rnorm
#' @importFrom hash hash has.key values
#' @export
ScatterplotMatrix <- function(x, seed = 123, symbol = "Small dots",
                              modifications = "None", fit.type = "None", weights = 1:NROW(x))
{
    pch <- if (symbol == "Small dots") {
        "."
    } else if (symbol == "Dots") {
        20
    } else if (symbol == "Big dots") {
        19
    } else if (symbol == "Circles") {
        "O"
    } else
        stop(paste("Symbol not found:", symbol))

    set.seed(seed)
    panel.scatter <- function(x, y, pch = par("pch"), ...)
    {
        if (modifications == "None")
        {
            points(x, y, pch = pch, ...)
        }
        else if (modifications == "Jitter")
        {
            jitter.x <- x + rnorm(length(x)) * (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 0.01
            jitter.y <- y + rnorm(length(y)) * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) * 0.01
            points(jitter.x, jitter.y, pch = pch, ...)
        }
        else if (modifications == "Enlarge points with multiple observations")
        {
            c.hash <- hash()
            x.hash <- hash()
            y.hash <- hash()
            for (i in seq(x))
            {
                key <- paste(x[i], y[i], collapse = ",")

                if (has.key(key, c.hash))
                    c.hash[[key]] <- c.hash[[key]] + weights[i]
                else
                {
                    c.hash[[key]] <- weights[i]
                    x.hash[[key]] <- x[i]
                    y.hash[[key]] <- y[i]
                }
            }
            cex <- pmax(1, 10 * sqrt(values(c.hash) / sum(weights)))
            points(values(x.hash), values(y.hash), pch = pch, cex = cex, ...)
        }
        else
            stop(paste("Point setting not handled:", modifications))

        if (fit.type == "Straight")
        {
            reg <- lm(y ~ x, weights = weights)
            abline(reg, col = "red", ...)
        }
        else if (fit.type == "LOESS")
        {
            ok <- is.finite(x) & is.finite(y)
            if (any(ok))
            {
                xok <- x[ok]
                yok <- y[ok]
                j <- order(xok)
                suppressWarnings(fitted.y <- loess(yok ~ xok, weights = weights[ok])$fitted)
                lines(xok[j], fitted.y[j], col = "red", ...)
            }
        }
    }

    # Histograms for the diagonal
    panel.hist <- function(x, ...)
    {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- weights::wtd.hist(x, plot = FALSE, weight = weights)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts
        y <- y / max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "gray")
    }

    # Correlations for the upper triangle
    panel.cor <- function(x, y, digits = 2, ...)
    {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- weights::wtd.cors(x, y, weight = weights)
        txt <- FormatAsReal(r, decimals = digits)
        text(0.5, 0.5, txt, cex = 1.2)
    }

    # Variable names/labels in the diagonal
    panel.label <- function(x, y, labels, cex, font, ...)
    {
        text(0.5, 0.85, labels, cex = 1.2)
    }

    res <- pairs(x,
               pch = pch,
               lower.panel = panel.scatter,
               diag.panel = panel.hist,
               upper.panel = panel.cor,
               text.panel = panel.label,
               gap = 0.5)
    class(res) <- c("visualization-selector", class(res))
    return(res)
}


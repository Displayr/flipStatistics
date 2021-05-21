#' Scatterplot matrix
#'
#' Creates a matrix of scatterplot from variable pairs
#' @param x A dataframe containing the variables to be plotted.
#' @param weights A numeric vector with length equal to the number of rows in \code{x}.
#' @param seed A seed used to generate random variables for jitter.
#' @param modifications One of "None", "Jitter", or "Enlarge points with multiple observations".
#' @param fit.type Character; type of line of best fit to show in the scatterplot in the 
#'   lower triangle of the graphic. One of "None", "LOESS", or "Straight".
#' @param fit.line.type Character; One of "solid", "dot", "dash, "dotdash", or length of dash "2px", "5px".
#' @param fit.line.width Numeric; Line width of line of best fit.
#' @param fit.line.color Color of line of best fit.
#' @param fit.line.opacity Opacity of trend line as an alpha value (0 to 1).
#' @param point.symbol Character; symbol used in scatterplots in the lower triangle of the graphic
#'  See \url{https://plotly-r.com/working-with-symbols.html} for a list of symbol names.
#' @param point.size Size of points in scatterplots in pixel. If point sizes vary by frequency, then
#'  this parameter specfies the maximum size of the points.
#' @param point.color Color of the points in the scatterplots.
#' @param point.opacity Opacity of the points in the scatterplots as an alpha value (0 to 1).
#' @param histogram.color Color of the histogram bars shown in the diagonal panels.
#' @param histogram.opacity Opeacity of the histogram bars as an alpha value (0 to 1).
#' @param correlation.decimals Number of decimals used to show correlation values in the upper trangle.
#' @param correlation.font.family Font family of correlation values shown in the upper triangle.
#' @param correlation.font.color Font color of correlation values.
#' @param correlation.font.size Font size of correlation values in pixels.
#' @param label.font.family Font family of the variable name labels shown in the diagonal panels.
#' @param label.font.color Font color of the variable name labels.
#' @param label.font.size Font size of the variable name labels in units of pixels.
#' @param hovertext.font.family Font family of hover text (tooltips). Font color will be automatically
#'  determined based on the color of the points or histograms.
#' @param hovertext.font.size Font size of hover text in pixels.
#' @param tick.font.family Font family of ticks labels (across all panel).
#' @param tick.font.color Font color of the tick labels.
#' @param tick.font.size Font size tick labels in pixels.
#' @param tick.length Length of tick labels in pixels.
#' @param tick.format Format of the ticklabels in D3 (e.g. ".1f"). Leave blank for plotly to 
#'  automatically set format based on values.
#' @param panel.outline Logical; whether of not to show outline around each of the panels.
#' @param panel.line.width Line width of panel.outllne in pixels.
#' @param panel.line.color Color of panel lines.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param background.fill.color Background color in character format (e.g. "black") or a hex code.
#' @param background.fill.opacity Background opacity as an alpha value (0 to 1).
#' @param charting.area.fill.color Charting area background color as
#' a named color in character format (e.g. "black") or a hex code.
#' @param charting.area.fill.opacity Charting area background opacity as an alpha value (0 to 1).
#' @param panel.gap Space between the panels of the scatterplot matrix as a proportion of the total graphic.
#' @param margin.top Margin between charting area and the top of the graphic in pixels.
#' @param margin.bottom Margin between charting area and the bottom of the graphic in pixels.
#' @param margin.left Margin between charting area and the left of the graphic in pixels.
#' @param margin.right Margin between charting area and the right of the graphic in pixels.
#' @param margin.autoexpand Logical; Whether extra space can be added to the margins
#'      to allow space for axis labels or other chart elements.
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param zoom.enable Logical; whether to enable zoom on the chart.
#' @importFrom plotly plot_ly add_trace layout subplot toRGB config
#' @importFrom stats lm loess rnorm
#' @importFrom hash hash has.key values
#' @importFrom flipTransformations AsNumeric
#' @export
ScatterplotMatrix <- function(x, weights = 1:NROW(x), seed = 123,
                              modifications = "Enlarge points with multiple observations", 
                              fit.type = "None",
                              fit.line.type = "solid",
                              fit.line.width = 2,
                              fit.line.color = "#990000",
                              fit.line.opacity = 1,
                              point.symbol = "circle",
                              point.size = 10,
                              point.color = "#5C9AD3",
                              point.opacity = 0.4,
                              histogram.color = "#5C9AD3",
                              histogram.opacity = 1.0,
                              correlation.decimals = 2,
                              background.fill.color = "transparent",
                              background.fill.opacity = 1,
                              charting.area.fill.color = "transparent",
                              charting.area.fill.opacity = 1,
                              global.font.family = "Arial",
                              global.font.color = "#222222",
                              label.font.family = global.font.family,
                              label.font.color = global.font.color,
                              label.font.size = 12,
                              correlation.font.family = global.font.family,
                              correlation.font.color = global.font.color,
                              correlation.font.size = 12,
                              hovertext.font.family = global.font.family,
                              hovertext.font.size = 10,
                              tick.font.family = global.font.family,
                              tick.font.color = global.font.color,
                              tick.font.size = 9,
                              tick.length = 3,
                              tick.format = "",
                              panel.outline = TRUE,
                              panel.line.width = 1,
                              panel.line.color = "#BBBBBB",
                              panel.gap = 0.01,
                              margin.left = 20, 
                              margin.right = 20,
                              margin.top = 20,
                              margin.bottom = 20, 
                              margin.autoexpand = TRUE,
                              modebar.show = FALSE,
                              tooltip.show = TRUE,
                              zoom.enable = FALSE)

{
    set.seed(seed)
    if (is.null(ncol(x)) || ncol(x) < 2)
        stop("Input data must contain at least two variables.")
    x <- suppressWarnings(AsNumeric(x, binary = FALSE))
    n <- ncol(x)
    labels <- colnames(x)
    label.font <- list(family = label.font.family, color = label.font.color, size = label.font.size)
    correlation.font <- list(family = correlation.font.family, color = correlation.font.color,
        size = correlation.font.size)
    blank.axis <- list(range = c(0, 1), showgrid = FALSE, showticklabels = FALSE, mirror = TRUE,
        showline = panel.outline, linewidth = panel.line.width, linecolor = panel.line.color,
        zeroline = FALSE, fixedrange = !zoom.enable) 
    tick.font <- list(family = tick.font.family, color = tick.font.color, size = tick.font.size)
    hist.hover <- list(font = list(family = hovertext.font.family, size = hovertext.font.size,
        color = autoFontColor(histogram.color)), bgcolor = histogram.color)
    point.hover <- list(font = list(family = hovertext.font.family, size = hovertext.font.size,
        color = autoFontColor(point.color)), bgcolor = point.color)
    fit.hover <- list(font = list(family = hovertext.font.family, size = hovertext.font.size,
        color = autoFontColor(fit.line.color)), bgcolor = fit.line.color)
    point.marker <- list(symbol = point.symbol, size = point.size,
        color = toRGB(point.color, alpha = point.opacity),
        line = list(color = toRGB(point.color, alpha = point.opacity)))
    fit.line.marker <- list(dash = fit.line.type, width = fit.line.width, shape = 'spline',
         color = fit.line.color, opacity = fit.line.opacity)

    panels <- list()
    k <- 1
    for (i in 1:n)
        for (j in 1:n)
        {
            if (i == j)
                panels[[k]] <- panel_hist(x[,i], weights = weights, 
                    label = labels[i], label.font = label.font,
                    color = histogram.color, opacity = histogram.opacity,
                    hover.style = hist.hover, axis = blank.axis)
            else if (j > i)
                panels[[k]] <- panel_cor(x[,j], x[,i], weights = weights, 
                    decimals = correlation.decimals, font = correlation.font, axis = blank.axis)
            else
                panels[[k]] <- panel_scatter(x[,j], x[,i], x.name = labels[j], y.name = labels[i],
                    weights, modifications,
                    fit.type, fit.line.marker, fit.hover,
                    point.marker, point.hover,
                    xaxis = list(range = range(x[,j], na.rm = TRUE, finite = TRUE) * c(0.9, 1.1),
                        showgrid = FALSE, tickfont = tick.font, tickformat = tick.format,
                        showline = panel.outline, mirror = TRUE, ticks = "outside", ticklen = tick.length,
                        linewidth = panel.line.width, linecolor = panel.line.color,
                        showticklabels = i == n, tickcolor = panel.line.color, fixedrange = !zoom.enable),
                    yaxis = list(range = range(x[,i], na.rm = TRUE, finite = TRUE) * c(0.9, 1.1),
                        showgrid = FALSE, tickfont = tick.font, tickformat = tick.format,
                        showline = panel.outline, mirror = TRUE, ticks = "outside", ticklen = tick.length,
                        linewidth = panel.line.width, linecolor = panel.line.color,
                        showticklabels = j == 1, tickcolor = panel.line.color, fixedrange = !zoom.enable))
            k <- k + 1
        }

    h.offset <- c(panel.gap, rep(0, max(0, n - 2)), panel.gap)[1:n]
    w.offset <- c(panel.gap, rep(0, max(0, n - 2)), panel.gap)[1:n]
    if (panel.gap >= 1/(2*n))
        stop("'Panel gap' should be between 0 and 1/(2n) (",
             round(1/(2*n), 4), ")")
    res <- subplot(panels, nrows = n, margin = panel.gap,
        heights = rep(1/n, n) - h.offset, 
        widths  = rep(1/n, n) - w.offset)

    res <- config(res, displayModeBar = modebar.show)
    res$sizingPolicy$browser$padding <- 0
    res <- layout(res, showlegend = FALSE,
        margin = list(l = margin.left, r = margin.right, t = margin.top, b = margin.bottom,
            autoexpand = margin.autoexpand),
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = if (tooltip.show) "closest" else FALSE,
        hoverlabel = list(namelength = -1, bordercolor = "transparent",
            font = list(size = hovertext.font.size, family = hovertext.font.family, color ="#222222")))
    attr(res, "can-run-in-root-dom") <- TRUE
    class(res) <- c(class(res), "visualization-selector")
    return(res)
}

panel_cor <- function(x, y, weights, decimals, font, axis)
{
    r <- weights::wtd.cors(x, y, weight = weights)
    txt <- FormatAsReal(r, decimals = decimals)
    pp <- plot_ly()
    pp <- add_trace(pp, x = 0.5, y = 0.5, text = txt, type = "scatter", mode = "text",
            hoverinfo = "skip", textfont = font)
    pp <- layout(pp,
        xaxis = axis, yaxis = axis)
    return(pp)
}


panel_hist <- function(x, weights, label, label.font, color, opacity, hover.style, axis)
{
    h <- weights::wtd.hist(x, plot = FALSE, weight = weights)
    breaks <- h$breaks
    nB <- length(breaks)
    x0 <- (breaks[-nB] + breaks[-1])/2
    y <- h$counts
    y <- y / sum(y)
    hover.text = sprintf("P(%.2f < x < %.2f) = %.2f", breaks[-nB], breaks[-1], y)
    xaxis <- axis
    yaxis <- axis
    xaxis$range <- range(x, na.rm = TRUE, finite = TRUE) * c(0.9, 1.1)
    yaxis$range <- c(0, 1.5 * max(y))

    pp <- plot_ly()
    pp <- add_trace(pp, x = x0, y = y, type = "bar", showlegend = FALSE,
        marker = list(color = toRGB(color, alpha = opacity)),
        name = label, hoverinfo = "name+text", text = hover.text, hoverlabel = hover.style)
    pp <- layout(pp, xaxis = xaxis, yaxis = yaxis, 
        annotations = list(text = label, showarrow = FALSE, font = label.font,
        x = 0.5, y = 0.85, xref = "paper", yref = "paper", xanchor = "center", yanchor = "middle"))
    return(pp)
}


panel_scatter <- function(x, y, x.name, y.name, weights, modifications, 
    fit.type, fit.line.marker, fit.hover.style,
    point.marker, point.hover.style, xaxis, yaxis)
{
    sz.pts <- 1
    if (modifications == "None")
    {
        x.pts <- x
        y.pts <- y
    }
    else if (modifications == "Jitter")
    {
        x.pts <- x + rnorm(length(x)) * (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 0.01
        y.pts <- y + rnorm(length(y)) * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) * 0.01
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
        x.pts <- values(x.hash)
        y.pts <- values(y.hash)
        sz.const <- point.marker$size
        sz.raw <- sqrt(values(c.hash) / sum(weights))
        sz.min <- min(sz.raw, na.rm = TRUE)
        sz.max <- max(sz.raw, na.rm = TRUE)
        sz.scaled <- (sz.raw - sz.min)/(sz.max - sz.min) * sz.const
        point.marker$sizemode = "Area"
        point.marker$size <- sz.scaled
    }
    else
        stop(paste("Point setting not handled:", modifications))

    pp <- plot_ly(x = x.pts, y = y.pts, type = "scatter", mode = "markers", 
        marker = point.marker, hoverinfo = "x+y", hoverlabel = point.hover.style,
        hovertemplate = paste0(x.name, ": %{x}<br>", y.name, ": %{y}<extra></extra>"),
        cliponaxis = FALSE)

    if (fit.type == "Straight")
    {
        reg <- lm(y ~ x, weights = weights)
        pp <- add_trace(pp, x = x, y = reg$fitted.values, type = "scatter", mode = "markers+lines",
                marker = list(color = fit.line.marker$color, opacity = 0),
                line = fit.line.marker, name = "Fitted",
                hovertemplate = paste0(x.name, ": %{x}<br>", y.name, ": %{y}"),
                hoverinfo = "all", hoverlabel = fit.hover.style)
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
            pp <- add_trace(pp, x = xok[j], y = fitted.y[j], type = "scatter", mode = "markers+lines",
                marker = list(color = fit.line.marker$color, opacity = 0),
                line = fit.line.marker, name = "Fitted", 
                hovertemplate = paste0(x.name, ": %{x}<br>", y.name, ": %{y}"),
                hoverinfo = "all", hoverlabel = fit.hover.style)
        }
    }
    pp <- layout(pp, xaxis = xaxis, yaxis = yaxis)
    return(pp)
}

#' use black or white for good contrast against colors
#'
#' @param colors vector of colors which will be the background color of the
#' @importFrom grDevices col2rgb rgb2hsv
autoFontColor <- function (colors)
{
    if (is.null(colors))
        return (NULL)
    tmp.rgb <- col2rgb(colors)
    tmp.lum <- apply(tmp.rgb, 2, function(x) return(0.299*x[1] + 0.587*x[2] + 0.114*x[3]))
    return(ifelse(tmp.lum > 126, "#2C2C2C", "#FFFFFF"))
}

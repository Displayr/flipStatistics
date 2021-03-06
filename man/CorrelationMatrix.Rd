% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariancecorrelation.R
\name{CorrelationMatrix}
\alias{CorrelationMatrix}
\title{\code{CorrelationMatrix}}
\usage{
CorrelationMatrix(
  input.data,
  use.names = FALSE,
  ignore.columns = "",
  missing.data = "Use partial data",
  spearman = FALSE,
  filter = NULL,
  weights = NULL,
  show.cell.values = "Automatic",
  colors = NULL,
  colors.min.value = -1,
  colors.max.value = 1,
  row.labels = "Yes",
  column.labels = "Yes",
  input.type = NULL,
  categorical.as.binary = FALSE
)
}
\arguments{
\item{input.data}{Either a \code{\link{data.frame}}, a \code{\link{list}} of
\code{\link{data.frame}}s and/or \code{\link{vector}}s, or a \code{\link{matrix}}.}

\item{use.names}{Whether to use names in place of labels.}

\item{ignore.columns}{A list of names of columns to ignore. When \code{input.data}
is a \code{\link{matrix}}, rows are also ignored. Typically \code{c("NET", "Total", "SUM")}.}

\item{missing.data}{Treatment of missing data. Options are \code{"Use partial data"}, \code{"Error if missing data"},
or \code{"Exclude cases with missing data"}.}

\item{spearman}{Boolean whether to compute Spearman's correlation instead of Pearson's correlation.}

\item{filter}{An optional logical vector specifying a subset of values to be used.}

\item{weights}{An optional vector of sampling weights.}

\item{show.cell.values}{Either \code{"Yes"}, \code{"No"} or \code{"Automatic"}. \code{"Automatic"} displays
values if there are <= 10 rows in the matrix.}

\item{colors}{A vector of colors used to create the colorbar.
If not specified it will default to \code{RdBu} from colorbrewer.}

\item{colors.min.value}{Lower bound of the colorbar}

\item{colors.max.value}{Upper bound of the colorbar}

\item{row.labels}{Either \code{"Yes"} or \code{"No"} indicating whether row labels should be displayed.}

\item{column.labels}{Either \code{"Yes"} or \code{"No"} indicating whether row labels should be displayed.}

\item{input.type}{Deprecated. Now automatically deduced from \code{input.data}.}

\item{categorical.as.binary}{Whether to convert factors to dummy binary variables, or else their
levels are converted to integers.}
}
\description{
Produces a correlation matrix from columns of data.
}

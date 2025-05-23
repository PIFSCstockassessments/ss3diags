% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSplotGeneric.R
\name{SSplotGeneric}
\alias{SSplotGeneric}
\title{Generic SSplot function}
\usage{
SSplotGeneric(
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 12,
  cex.main = 1,
  pt.cex = 1,
  plotdir = NULL,
  filenameprefix = "",
  par = list(mar = c(5, 4, 1, 1) + 0.1, family = "sans"),
  verbose = TRUE,
  plot = TRUE,
  print = deprecated(),
  print_plot = FALSE,
  png = deprecated(),
  use_png = print_plot,
  pdf = deprecated(),
  use_pdf = FALSE,
  new = TRUE,
  add = FALSE
)
}
\arguments{
\item{pwidth}{Default width of plot printed to plot in units of \code{punits}}

\item{pheight}{Height of plot printed to plot in units of \code{punits }}

\item{punits}{Measurement units for \code{pwidth} and \code{pheight}. Default is \code{"in"}.
\itemize{
\item \code{"px"} (pixels)
\item \code{"in"} (inches)
\item \code{"cm"} (centimeters)
\item \code{"mm"} (millimeters)
}}

\item{res}{Resolution for plots printed to files.}

\item{ptsize}{Point size for plotted text in plots printed in files. See \code{help("png")}
for more details}

\item{cex.main}{Character expansion for plot titles.}

\item{pt.cex}{Character expansion for points in plots.}

\item{plotdir}{Directory where output plot file will be written. By default,
it will be the directory where the model was run.}

\item{filenameprefix}{Additional text to append to output plot file name.
It will be separated from default name by an underscore.}

\item{par}{A numerical vector of the form c(bottom, left, top, right) which
gives the number of lines of margin to be specified on the four sides of
the plot,which is passed to \code{par()}. Entering \code{NULL} passes plot's default
\code{par()} values (which depends on whether the main title is included or not)}

\item{verbose}{Flag to print additional diagnostic messages to R console}

\item{plot}{DEPRECATED. By default, TRUE, Plots (and subplots) are drawn to the
plot device. The option to explicitly disable this option (FALSE), is not
implemented. This option flag will be defunct in a future version}

\item{print}{DEPRECATED, please use \code{print_plot}.}

\item{print_plot}{Flag to enable plot graphic device to print to PNG or PNG files.}

\item{png}{DEPRECATED. Please use \code{use_png}.}

\item{use_png}{Enables plots to be generated to PNG files. Defaults to print value}

\item{pdf}{DEPRECATED. Please use \code{use_pdf}.}

\item{use_pdf}{Enables plots to be generated to pdf file.}

\item{new}{Deprecated. New plot windows are created by default (TRUE), and the
option to disable this, via FALSE, is unused.}

\item{add}{suppresses \code{par()} to create multiplot figs}
}
\description{
Generalized version of ss3diags SSplots functions. Not intended for
production
}
\keyword{internal}

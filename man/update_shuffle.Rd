% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update.R
\name{update_shuffle}
\alias{update_shuffle}
\title{Update Shuffle Instance}
\usage{
update_shuffle(shuffleId, session = shiny::getDefaultReactiveDomain())
}
\arguments{
\item{shuffleId}{The id of the shuffle container.}

\item{session}{The \code{session} object passed to function given to shinyServer.}
}
\description{
Use in a \strong{Shiny} app to update cards layout.
}
\note{
Can be useful when grid layout haven't been initialized correctly.
}

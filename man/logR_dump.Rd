% Generated by roxygen2: do not edit by hand
\name{logR_dump}
\alias{logR_dump}
\title{Query whole logR table}
\usage{
logR_dump(.conn = getOption("logR.conn"), .table = getOption("logR.table"),
  .schema = getOption("logR.schema"))
}
\arguments{
\item{.conn}{DBI connection.}

\item{.table}{character.}

\item{.schema}{character.}
}
\value{
data.table, if non-zero length then ordered by \emph{logr_id} field.
}
\description{
Easy way to fetch all logs from logR table.
}


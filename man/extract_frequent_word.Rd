% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_mining_functions.R
\name{extract_frequent_word}
\alias{extract_frequent_word}
\title{Extract frequently observed words}
\usage{
extract_frequent_word(data, n = 20, ...)
}
\arguments{
\item{data}{A document-feature matrix (dfm) object through the quanteda package.}

\item{n}{A number of top n features (terms or words) frequently observed.}

\item{...}{Further arguments passed to \code{quanteda.textstats::textstat_frequency}.}
}
\description{
Extract frequently observed top n features (terms or words).
}
\examples{
\dontrun{
dfm <- processed_tokens \%>\% construct_dfm()

dfm \%>\% extract_frequent_word()
}

}

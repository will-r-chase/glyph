% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summon.R
\name{summon}
\alias{summon}
\title{Draw a summoning circle diagram}
\usage{
summon(seed_probs = c(0.3, 0.5, 0.1, 0.1), inscribed_probs = c(0.2,
  0.3, 0.3, 0.2), sec_shape_probs = c(0.4, 0.6),
  third_shape_probs = c(0.5, 0.5))
}
\arguments{
\item{seed_probs}{probabilities of choosing each shape (none, circle, diamond, square), numeric vector of length 4}

\item{inscribed_probs}{probabilities for how many inscribed shapes (0, 1, 2, 3), numeric vector of length 4}

\item{sec_shape_probs}{probabilities of second inscribed shape, numeric vector of length 2}

\item{third_shape_probs}{probabilities of third inscribed shape, numeric vector of length 2}
}
\value{
a ggplot of the summoning diagram
}
\description{
Draw a summoning circle diagram
}

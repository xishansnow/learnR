#' @title
#' Happiness Dataset
#'
#' @description
#' A data frame containing a happiness survey and demographic data.
#' This data are fictitious.
#'
#' @source
#' The data were randomly generated using functions from the
#' \href{https://cran.r-project.org/web/packages/wakefield/index.html}{wakefield}
#' package.
#'
#' @format A data frame with 460 rows and 11 variables:
#' \describe{
#'   \item{\code{ID}}{character. A unique identifier.}
#'   \item{\code{Date}}{date. Date of the interview.}
#'   \item{\code{Sex}}{factor. Sex coded as \code{Male} or \code{Female}.}
#'   \item{\code{Race}}{factor. Race coded as an 8-level factor.}
#'   \item{\code{Age}}{integer. Age in years.}
#'   \item{\code{Education}}{factor. Education coded as a 13-level factor.}
#'   \item{\code{Income}}{double. Annual income in US dollars.}
#'   \item{\code{IQ}}{double. Adult intelligence quotient. This
#'   variable has a large amount of missing data.}
#'   \item{\code{Zip}}{character. USPS Zip code.}
#'   \item{\code{Children}}{integer. Number of children.}
#'   \item{\code{Happy}}{factor. Agreement with the statement
#'   "I am happy most of the time", coded as \code{Strongly Disagree} ,
#'   \code{Disagree}, \code{Neutral}, \code{Agree}, or
#'   \code{Strongly Agree}.}
#' }
"happiness"

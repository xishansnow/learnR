#' @title Visualize a data frame
#'
#' @description
#' \code{plot.contents} visualizes the variables in a data frame.
#'
#' @details
#' For each variable, the plot displays
#' \itemize{
#'   \item{type (\code{numeric},
#'               \code{integer},
#'               \code{factor},
#'               \code{ordered factor},
#'               \code{logical}, or \code{date})}
#'   \item{percent of available (and missing) cases}
#' }
#' Variables are sorted by type and the total number of variables
#' and cases are printed in the caption.
#' @param x an object of class \code{contents}.
#' @param ... additional arguments (not currently used).
#' @import ggplot2
#' @method plot contents
#' @export
#' @return a \code{ggplot2} graph
#' @seealso See \link{contents}.
#' @examples
#' df_info <- contents(happiness)
#' plot(df_info)

plot.contents <- function(x, ...){
  if (!inherits(x, "contents"))
    stop("Object must be of class 'contents'")

  classes <- x$varinfo$type

  pct_n <-  100 *(1 - x$varinfo$pct_miss)

  df <- data.frame(var = x$varinfo$varname,
                   classes = classes,
                   pct_n = pct_n,
                   classes_n = as.numeric(as.factor(classes)))

  ggplot(df,
         aes(x=stats::reorder(.data[["var"]], .data[["classes_n"]]),
             y=.data[["pct_n"]], fill=.data[["classes"]])) +
    geom_bar(stat="identity") +
    labs(x="", y="Percent Available",
         title=x$dfname,
         caption=paste(x$nrow, "cases",
                        x$ncol, "variables"),
         fill="Type") +
    guides(fill = guide_legend(reverse=TRUE)) +
    scale_y_continuous(breaks=seq(0, 100, 20)) +
    coord_flip() +
    theme_minimal()
}

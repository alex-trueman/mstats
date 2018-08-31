#' Normal score transformation of data.
#'
#' Takes a vector of values x and calculates their normal scores. Returns a list
#' with the scores and an ordered table of original values and scores, which is
#' useful as a back-transform table. See backtr().
#'
#' @author Alex M Trueman
#' Modified from Ashton Shortridge, May/June, 2008.
#' https://msu.edu/~ashton/research/code/nscore.R
#' Originally based on the GSLIB code for nscore function.
#'
#' @param df Data frame.
#' @param d Column in data frame to be transformed.
#' @param na.rm Boolean, remove NAs from d. Defult is TRUE.
#'
#' @return Named list of data frames with transformed data and transformation
#'     table.
#' @export
#' @importFrom dplyr mutate tibble
#' @importFrom magrittr %>%
#' @importFrom rlang enquo quo_name !!
#' @importFrom stats na.omit qqnorm
#'
nscore <- function(df, d, na.rm = TRUE) {

    d <- enquo(d)
    d_str <- quo_name(d)

    if(na.rm) {
        data <- df %>%
            na.omit(!!d)
    } else {
        data <- df
    }

    # Transform the data.
    data <- data %>%
        mutate(nscore = qqnorm(!!d, plot.it = FALSE)$x)


    # Create transform table for back-transformation.
    transform_table <- tibble(
        x = sort(data[,d_str]),
        nscore = sort(data[,"nscore"]))

    return (list(nscore = data, transform_table = transform_table))

}

#' Back-transform normal score transformed data.
#'
#' Given a vector of normal scores and a normal score object (from nscore), the
#' function returns a vector of back-transformed values. One major issue is how
#' to extrapolate to the tails. Options  other than none may result in
#' dramatically incorrect tail estimates!
#'
#' Tails options:
#'     'none': No extrapolation; more extreme score values will revert to the
#'     original min and max values.
#'     'equal': Calculate magnitude in std deviations of the scores about
#'     initial data mean. Extrapolation is linear to these deviations.  Will be
#'     based upon deviations from the mean of the original hard data - possibly
#'     quite dangerous!
#'     'separate': This calculates a separate sd for values above and below the
#'     mean.
#'
#' @author Alex M Trueman
#' Modified from Ashton Shortridge, May/June, 2008.
#' https://msu.edu/~ashton/research/code/nscore.R
#' Originally based on the GSLIB code for backtr function.
#'
#' @param scores Numeric vector of normal score transformed data.
#' @param transform_table Dataframe with column of raw data matched to column of
#'     normal score data. Is output of `nscore` function.
#' @param tails Treatment of distribution tails: "none", "separate", or "equal".
#' @param draw Plot the distributions.
#'
#' @return Numeric vector of back=transformed values.
#' @export
#' @importFrom graphics plot
#' @importFrom stats approxfun sd
backtr <- function(scores, transform_table, tails = "none", draw = FALSE) {

    if(tails == "separate") {
        x_mean <- mean(transform_table$x)
        x_small <- transform_table$x < x_mean
        x_large <- transform_table$x > x_mean
        sd_small <- sqrt(sum((transform_table$x[x_small] - x_mean)^2) /
                (length(transform_table$x[x_small]) - 1))
        sd_large <- sqrt(sum((transform_table$x[x_large] - x_mean)^2) /
                (length(transform_table$x[x_large]) - 1))
        x_min <- mean(transform_table$x) + (min(scores) * sd_small)
        x_max <- mean(transform_table$x) + (max(scores) * sd_large)
        # Check if max and min are LESS extreme than the initial data - if so, use
        # the initial data in their place.
        if(x_max > min(transform_table$x)) {
            x_min <- min(transform_table$x)
        }
        if(x_max < max(transform_table$x)) {
            x_max <- max(transform_table$x)
        }
    } else if (tails == "equal") { # Assume symmetric distribution around mean.
        x_mean <- mean(transform_table$x)
        x_sd <- sd(transform_table$x)
        x_min <- mean(transform_table$x) + (min(scores) * x_sd)
        x_max <- mean(transform_table$x) + (max(scores) * x_sd)
        # Check if max and min are LESS extreme than the initial data - if so, use
        # the initial data in their place.
        if(x_max > min(transform_table$x)) {
            x_min <- min(transform_table$x)
        }
        if(x_max < max(transform_table$x)) {
            x_max <- max(transform_table$x)
        }
    } else { # No extrapolation.
        x_min <- min(transform_table$x)
        x_max <- max(transform_table$x)
    }

    sc_min <- min(scores)
    sc_max <- max(scores)
    x <- c(x_min, transform_table$x, x_max)
    sc_n <- c(sc_min, transform_table$nscore, sc_max)

    if(draw) {plot(sc_n, x, main = "Transform Function")}

    back_xf <- approxfun(sc_n, x) # Develop the back transform function.
    val <- back_xf(scores)

    return(val)

}

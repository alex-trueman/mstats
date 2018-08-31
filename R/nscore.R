
#' Make normal score transformation model.
#'
#' Takes a vector of values x and calculates their normal scores. Returns an
#' ordered table of original values and scores, which is useful as a
#' back-transform model. See backtr().
#'
#' @author Alex M Trueman
#' Modified from Ashton Shortridge, May/June, 2008.
#' https://msu.edu/~ashton/research/code/nscore.R
#' Originally based on the GSLIB code for nscore function.
#'
#' @param x Numeric vector to be transformed.
#' @param na.rm Boolean, remove NAs from x. Defult is TRUE.
#'
#' @return Dataframe transformation model.
#' @export
#' @importFrom stats na.omit qqnorm
#'
nscore_model <- function(x, na.rm = TRUE) {

    if(na.rm) {x <- na.omit(x)}

    # Create transform table for back-transformation.
    nscore <- qqnorm(x, plot.it = FALSE)$x
    model <- data.frame(x = sort(x), nscore = sort(nscore))

    return(model)

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
#' @param model Dataframe with column of raw data matched to column of
#'     normal score data. Is output of `nscore` function.
#' @param tails Treatment of distribution tails: "none", "separate", or "equal".
#' @param draw Plot the distributions.
#'
#' @return Numeric vector of back-transformed values.
#' @export
#' @importFrom graphics plot
#' @importFrom stats approxfun sd
backtr <- function(model, tails = "none", draw = FALSE) {

    if(tails == "separate") {
        x_mean <- mean(model$x)
        x_small <- model$x < x_mean
        x_large <- model$x > x_mean
        sd_small <- sqrt(sum((model$x[x_small] - x_mean)^2) /
                (length(model$x[x_small]) - 1))
        sd_large <- sqrt(sum((model$x[x_large] - x_mean)^2) /
                (length(model$x[x_large]) - 1))
        x_min <- mean(model$x) + (min(scores) * sd_small)
        x_max <- mean(model$x) + (max(scores) * sd_large)
        # Check if max and min are LESS extreme than the initial data - if so, use
        # the initial data in their place.
        if(x_max > min(model$x)) {
            x_min <- min(model$x)
        }
        if(x_max < max(model$x)) {
            x_max <- max(model$x)
        }
    } else if (tails == "equal") { # Assume symmetric distribution around mean.
        x_mean <- mean(model$x)
        x_sd <- sd(model$x)
        x_min <- mean(model$x) + (min(scores) * x_sd)
        x_max <- mean(model$x) + (max(scores) * x_sd)
        # Check if max and min are LESS extreme than the initial data - if so, use
        # the initial data in their place.
        if(x_max > min(model$x)) {
            x_min <- min(model$x)
        }
        if(x_max < max(model$x)) {
            x_max <- max(model$x)
        }
    } else { # No extrapolation.
        x_min <- min(model$x)
        x_max <- max(model$x)
    }

    sc_min <- min(scores)
    sc_max <- max(scores)
    x <- c(x_min, model$x, x_max)
    sc_n <- c(sc_min, model$nscore, sc_max)

    if(draw) {plot(sc_n, x, main = "Transform Function")}

    back_xf <- approxfun(sc_n, x) # Develop the back transform function.
    val <- back_xf(scores)

    return(val)

}

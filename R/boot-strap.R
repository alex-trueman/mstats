
#' Calculate mean and bootstrapped confidence limits using the bias corrected and
#' accelarated (BCa) method, which is more robust with low sample numbers and
#' non-normal distributions of bootstrapped means.
#'
#' Based on https://www.painblogr.org/2017-10-18-purrring-through-bootstraps
#' by Peter Kamerman (@painblogR)
#' @author Alex M Trueman
#'
#' @param df Dataframe containing grouping and value fields.
#' @param value Value field for statistics.
#' @param reps Number of bootstraping repetitions. May need to increase if
#' BCa error occurs.
#' @param conf Confidence limits probability (e.g., 0.95).
#' @param ... One or more grouping columns.
#'
#' @return Dataframe with mean and confidence limits.
#' @export
#' @import dplyr
#' @importFrom rlang quos enquo quo_name
#' @importFrom boot boot boot.ci
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#'
#' @examples
#' df <- data.frame(
#' x = rlnorm(n = 100, meanlog = 1.5, sdlog = 1.1),
#' d = sample(c("a", "b"), 100, replace = TRUE)
#' )
#'
#' boot_mean_ci(df, x, reps = 100, conf = 0.9, d)
#'
boot_mean_ci <- function(df, value, reps, conf, ...) {

    group_quo <- quos(...)
    value_quo <- enquo(value)
    value_str <- quo_name(value_quo)

    # Weighted mean function to pass to boot.
    sample_mean <- function(d, i) {

        return(mean(d[i]))

    }

    # Calculate and tidy the confidence intervals by group.
    df_boot <- df %>%
        select(!!! group_quo, !! value_quo) %>%
        group_by(!!! group_quo) %>%
        nest() %>%
        mutate(
            booted = map(.x = data, ~ boot(
                data = `$`(.x, !!value_str), # Odd approach maybe, but works.
                statistic = sample_mean,
                R = reps,
                stype = "i",
                parallel = "snow")), # For windows multithreading, but boot.ci is slower.
            booted_ci = map(.x = booted, ~ boot.ci(
                .x,
                conf = conf,
                type = "bca"))
        ) %>%
        mutate(statistic = map(.x = booted_ci, ~ .x$t0),
            lower_ci = map(.x = booted_ci, ~ .x$bca[[4]]),
            upper_ci = map(.x = booted_ci, ~ .x$bca[[5]])) %>%
        select(-c(data, booted, booted_ci)) %>%
        unnest()

    return(df_boot)

}
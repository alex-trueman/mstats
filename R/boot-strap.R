#' Calculate mean and bootstrapped confidence limits using the bias corrected and
#' accelerated (BCa) method, which is more robust with low sample numbers and
#' non-normal distributions of bootstrapped means.
#'
#' Based on https://www.painblogr.org/2017-10-18-purrring-through-bootstraps
#' by Peter Kamerman (@painblogR)
#' @author Alex M Trueman
#'
#' @param df Data frame containing grouping and value fields.
#' @param value Value field for statistics.
#' @param reps Number of bootstrapping repetitions. May need to increase if
#' BCa error occurs.
#' @param conf Confidence limits probability (e.g., 0.95).
#' @param ... One or more grouping columns.
#'
#' @return Data frame with mean and confidence limits.
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
                parallel = "snow")), # For windows multi-threading, but boot.ci is slower.
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

#' Calculate weighted mean and bootstrapped confidence limits using the bias
#' corrected and accelerated (BCa) method, which is more robust with low sample
#' numbers and non-normal distributions of bootstrapped means.
#'
#' Based on https://www.painblogr.org/2017-10-18-purrring-through-bootstraps
#' by Peter Kamerman (@painblogR)
#' @author Alex M Trueman
#'
#' @param df Data frame containing grouping, value, and weighting fields.
#' @param value Value field for statistics.
#' @param weight Weighting field.
#' @param reps Number of bootstrapping repetitions. May need to increase if
#' BCa error occurs.
#' @param conf Confidence limits probability (e.g., 0.95).
#' @param ... One or more grouping columns.
#'
#' @return Data frame with weighted mean and confidence limits.
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
#' w = runif(n = 100, min = 0.1, max = 5),
#' d = sample(c("a", "b"), 100, replace = TRUE)
#' )
#'
#' boot_wmean_ci(df, x, w, reps = 100, conf = 0.9, d)
#'
boot_wmean_ci <- function(df, value, weight, reps, conf, ...) {

  # Set up passed arguments.
  group_quo <- quos(...)
  value_quo <- enquo(value)
  value_str <- quo_name(value_quo)
  weight_quo <- enquo(weight)
  weight_str <- quo_name(weight_quo)

  # Weighted mean function to pass to boot.
  sample_mean <- function(d, i, j) {

    return(weighted.mean(d[i], j[i]))

  }

  # Calculate and tidy the confidence intervals by group.
  df_boot <- df %>%
    select(!!! group_quo, !! value_quo, !! weight_quo) %>%
    group_by(!!! group_quo) %>%
    nest() %>%
    mutate(
      booted = map(.x = data, ~ boot::boot(
        data = `$`(.x, !!value_str), # Odd approach maybe, but works.
        statistic = sample_mean,
        R = reps,
        stype = "i",
        j = `$`(.x, !!weight_str), # Odd approach maybe, but works.
        parallel = "snow")), # For windows multi-threading, but boot.ci is slower.
      booted_ci = map(.x = booted, ~ boot::boot.ci(
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
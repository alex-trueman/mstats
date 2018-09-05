#' Calculate means for model and sample data for tabulation and plotting.
#'
#' Get means by domain group for various variables from block model and sample
#' files. Combine the statistics in wide and tall (tidy) formats for tabulation
#' and plotting respectively.
#'
#' @author Alex M Trueman
#'
#' @param bm Block model as data frame.
#' @param bm_columns Character vector of column names in block model for calculation of means.
#' @param samp Sample data as data frame.
#' @param samp_value Name of field in sample data for mean calculation.
#' @param samp_weight Decluster weight field in sample file.
#' @param group Grouping field present in block model and sample data.
#'
#' @return List of data frames with wide (for report tables) and tall (tidy) layout.
#' @export
#' @importFrom dplyr funs group_by left_join summarise_at summarise
#' @importFrom magrittr %>%
#' @importFrom rlang enquo quo_name
#' @importFrom stats weighted.mean
#' @importFrom tidyr gather
means_by_group <- function(bm, bm_columns, samp, samp_value, samp_weight, group) {

  samp_value <- enquo(samp_value)
  samp_weight <- enquo(samp_weight)
  group <- enquo(group)
  group_str <- quo_name(group)

  # Block model means.
  bm_means <- bm %>%
    group_by(!! group) %>%
    summarise_at(bm_columns, funs(mean(., na.rm = TRUE)))

  # Sample means.
  samp_means <- samp %>%
    group_by(!! group) %>%
    summarise(
      naive = mean(!! samp_value, na.rm = TRUE),
      declustered = weighted.mean(!! samp_value, !! samp_weight, na.rm = TRUE)
    )

  # Join block and sample mean data in wide format.
  all_means_wide <- bm_means %>%
    left_join(samp_means, by = group_str)

  # Create tidy format and make type an ordered factor so that plot order is controlled.
  all_means_tall <- all_means_wide %>%
    gather(type, mean, -c(!! group)) %>%
    mutate(
      type = factor(type, levels = c(bm_columns, "naive", "declustered"), ordered = TRUE))

  return(list(wide = all_means_wide, tall = all_means_tall))

}

#' Tabulation of summary statistics by group field.
#'
#' Markdown formatted tabulation of summary statistics by grouping field.
#'
#' @author Alex M Trueman
#'
#' @param df Data frame for generation of statistics
#' @param group Grouping field
#' @param value Numeric field to have statistics calculated
#' @param digits Round to this many digits
#'
#' @return Markdown format table as character string
#' @export
#' @importFrom dplyr filter group_by rename select summarise
#' @importFrom magrittr %>%
#' @importFrom rlang enquo UQ quo_name
#' @importFrom stats median sd var
#' @importFrom pander pander
sumstats <- function(df, group, value, digits) {
  group <- enquo(group)
  value <- enquo(value)
  group_str <- quo_name(group)
  value_str <- quo_name(value)

  data <- df %>%
    select(!! group, !! value) %>%
    filter(!is.na(UQ(value))) %>%
    group_by(!! group) %>%
    summarise(
      n = n(),
      min = min(!! value),
      max = max(!! value),
      median = median(!! value),
      mean = mean(!! value),
      sd = sd(!! value),
      var = var(!! value),
      cv = sd / mean
    ) %>%
    rename(group = !! group)

  pander(
    x = data,
    style = "rmarkdown",
    caption = paste0(
      "Descriptive statistics for ", value_str,
      " grouped by ", group_str
    ),
    justify = c("center", rep("right", 8)),
    round = c(0, 0, rep(digits, 7)),
    keep.trailing.zeros = TRUE,
    split.table = Inf
  )
}

#' Tabulation of extended summary statistics by group field.
#'
#' Markdown formatted tabulation of summary statistics by grouping field.
#'
#' @author Alex M Trueman
#'
#' @param df Data frame for generation of statistics
#' @param group Grouping field
#' @param value Numeric field to have statistics calculated
#' @param digits Round to this many digits
#'
#' @return Markdown format table as character string
#' @export
#' @importFrom dplyr filter group_by rename select summarise
#' @importFrom magrittr %>%
#' @importFrom rlang enquo UQ quo_name
#' @importFrom stats median quantile sd var
#' @importFrom pander pander
extended_sumstats <- function(df, group, value, digits) {
  group <- enquo(group)
  value <- enquo(value)
  group_str <- quo_name(group)
  value_str <- quo_name(value)

  data <- df %>%
    select(!! group, !! value) %>%
    filter(!is.na(UQ(value))) %>%
    group_by(!! group) %>%
    summarise(
      n = n(),
      min = min(!! value),
      max = max(!! value),
      median = median(!! value),
      mean = mean(!! value),
      irq = quantile(!! value, probs = 0.75) - quantile(!! value, probs = 0.25),
      sd = sd(!! value),
      var = var(!! value),
      cv = sd / mean,
      cl90 = 1.96 * sd
    ) %>%
    rename(group = !! group)

  pander(
    x = data,
    style = "rmarkdown",
    caption = paste0(
      "Descriptive statistics for ", value_str,
      " grouped by ", group_str
    ),
    justify = c("center", rep("right", 10)),
    #big.mark = ",",
    round = c(0, 0, rep(digits, 9)),
    keep.trailing.zeros = TRUE,
    split.table = Inf
  )
}

#' Tabulation of weighted summary statistics by group field.
#'
#' Markdown formatted tabulation of weighted summary statistics by grouping field.
#'
#' @author Alex M Trueman
#'
#' @param df Data frame for generation of statistics
#' @param group Grouping field
#' @param value Numeric field to have statistics calculated
#' @param weight Weight field for statistics
#' @param digits Round to this many digits
#'
#' @return Markdown format table as character string
#' @export
#' @importFrom dplyr filter group_by rename select summarise
#' @importFrom Hmisc wtd.var
#' @importFrom magrittr %>%
#' @importFrom rlang enquo UQ quo_name
#' @importFrom stats median weighted.mean
#' @importFrom pander pander
weighted_sumstats <- function(df, group, value, weight, digits) {
  group <- enquo(group)
  value <- enquo(value)
  weight <- enquo(weight)
  group_str <- quo_name(group)
  value_str <- quo_name(value)

  data <- df %>%
    select(!! group, !! value, !! weight) %>%
    filter(!is.na(UQ(value))) %>%
    group_by(!! group) %>%
    summarise(
      n = n(),
      min = min(!! value),
      max = max(!! value),
      median = median(!! value),
      mean = weighted.mean(!! value, !! weight),
      var = wtd.var(x = !! value, weights = !! weight),
      sd = sqrt(var),
      cv = sd / mean
    ) %>%
    rename(group = !! group)

  pander(
    x = data,
    style = "rmarkdown",
    caption = paste0(
      "Weighted descriptive statistics for ", value_str,
      " grouped by ", group_str
    ),
    justify = c("center", rep("right", 8)),
    #big.mark = ",",
    round = c(0, 0, rep(digits, 7)),
    keep.trailing.zeros = TRUE,
    split.table = Inf
  )
}

#' Tabulation of top cutting statistics by group field.
#'
#' Markdown formatted tabulation of weighted summary statistics by grouping field.
#'
#' @author Alex M Trueman
#'
#' @param df Data frame for generation of statistics
#' @param group Grouping field
#' @param value Numeric field to have statistics calculated
#' @param value_tc Numeric top cut field for statistics
#' @param weight Weight field for statistics
#' @param digits Round to this many digits
#'
#' @return Markdown format table as character string
#' @export
#' @importFrom dplyr filter group_by rename select summarise
#' @importFrom magrittr %>%
#' @importFrom rlang enquo UQ quo_name
#' @importFrom stats sd weighted.mean
#' @importFrom pander pander
topcut_sumstats <- function(df, group, value, value_tc, weight, digits) {
  group <- enquo(group)
  value <- enquo(value)
  value_tc <- enquo(value_tc)
  weight <- enquo(weight)
  group_str <- quo_name(group)
  value_str <- quo_name(value)

  data <- df %>%
    filter(!is.na(UQ(value))) %>%
    group_by(!! group) %>%
    summarise(
      n = n(),
      min = min(!! value, na.rm = TRUE),
      max = max(!! value, na.rm = TRUE),
      max_tc = max(!! value_tc, na.rm = TRUE),
      topcut = ifelse(max > max_tc, max_tc, NA),
      n_tc = sum(UQ(value) > topcut),
      p_tc = n_tc / n * 100,
      mean = weighted.mean(!! value, !! weight, na.rm = TRUE),
      mean_tc = weighted.mean(!! value_tc, !! weight, na.rm = TRUE),
      cv = sd(!! value, na.rm = TRUE) / mean,
      cv_tc = sd(!! value_tc, na.rm = TRUE) / mean_tc
    ) %>%
    rename(group = !! group) %>%
    select(group, n, n_tc, p_tc, min, max, topcut, mean, mean_tc, cv, cv_tc)

  pander(
    x = data,
    style = "rmarkdown",
    caption = paste0(
      "Weighted top cutting statistics for ", value_str,
      " grouped by ", group_str
    ),
    justify = c("center", rep("right", 10)),
    # big.mark = ",",
    round = c(0, 0, 0, rep(digits, 8)),
    keep.trailing.zeros = TRUE,
    split.table = Inf
  )
}

#' Detailed descriptive statistics for one or more columns columns.
#'
#' @author Alex M Trueman
#'
#' @param df Dataframe
#' @param ... One or more numeric column names to be evaluated.
#' @param sep Character string separating original column name and statistic.
#'    By default, this is a regex that finds the last underscore character in
#'    the column name. Normally this will work and doesn't need to be changed.
#'
#' @return Dataframe of statistics for each column input.
#' @export
#' @importFrom dplyr funs n select summarise_all
#' @importFrom magrittr %>%
#' @importFrom moments skewness
#' @importFrom rlang quos !!!
#' @importFrom stats median na.omit quantile sd var
#' @importFrom tidyr gather separate spread
#'
#' @examples
#' d <- data.frame(a = rnorm(10, 100, 20), b = rnorm(10, 1, 2))
#' descstat(d, a, b)
descstat <- function(df, ..., sep = "_(?=[^_]+$)") {

    vars <- quos(...)

    x <- df %>%
        select(!!!vars) %>%
        na.omit() %>%
        summarise_all(funs(
            n = n(),
            min = min,
            q25 = quantile(., 0.25),
            median = median,
            q75 = quantile(., 0.75),
            max = max,
            mean = mean,
            sd = sd,
            var = var,
            skew = skewness)) %>%
        gather(statistic, value) %>%
        separate(statistic, into = c("variable", "statistic"), sep = sep) %>%
        spread(statistic, value) %>%
        mutate(cv = sd / mean) %>%
        select(variable, n, min, q25, median, q75, max, mean, sd, var, cv, skew)

    return(x)

}

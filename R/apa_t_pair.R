#' APA text for Paired-Samples T-Test
#'
#' @description
#' Create APA-formatted text for the results of an independent-samples t-test in the following format:
#'
#' A paired-samples t-test was conducted to compare \{dv\} between \{level1\} (M = \{mean1\}, SD = \{sd1\}) and \{level2\} (M = \{mean2\}, SD = \{sd2\}). There was a \{non\}significant difference; t(\{df\}) = \{t_value\}, p = \{p_value\}.
#'
#' @param x A vector of the values for level 1.
#' @param y A vector of the values for level 2.
#' @param dv The text describing the DV in the output statement.
#' @param level1 The text describing level 1 in the output statement.
#' @param level2 The text describing level 2 in the output statement.
#'
#' @return A character string.
#' @export
apa_t_pair <- function(x, y,
                       dv = "the DV",
                       level1 = "level 1",
                       level2 = "level 2") {
  t_results <- t.test(x, y, paired = TRUE)

  template <- "A paired-samples t-test was conducted to compare {dv} between {level1} (M = {mean1}, SD = {sd1}) and {level2} (M = {mean2}, SD = {sd2}). There was a {non}significant difference; t({df}) = {t_value}, p = {p_value}."

  glue::glue(
    template,
    mean1   = round0(mean(x), 1),
    sd1     = round0(sd(x), 1),
    mean2   = round0(mean(y), 1),
    sd2     = round0(sd(y), 1),
    non     = ifelse(t_results$p.value < .05, "", "non-"),
    df      = round0(t_results$parameter, 0),
    t_value = round0(t_results$statistic,2),
    p_value = round0(t_results$p.value, 3)
  )
}

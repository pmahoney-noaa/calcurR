#' Title
#'
#' @param df
#' @param merge_tim_delta
#' @param seq_tim_delta
#' @param seq_dist_delta
#'
#' @return
#' @export
#'
#' @examples

check_aerial_merge <- function(df, merge_tim_delta = 2, seq_tim_delta = 2, seq_dist_delta = 200) {
  if (any(df$merge_diff_sec > merge_tim_delta)) {
    nM <- sum(df$merge_diff_sec > merge_tim_delta)
    nMr <- df %>% slice(which(df$merge_diff_sec > merge_tim_delta))
    warning(
      paste(
        c(sprintf("There are %i records when merged have a longer than acceptable time mismatch.", nM),
          capture.output(print(nMr, row.names = FALSE))),
        collapse = "\n")
    )
  }

  dfo <- df %>%
    mutate(
      survey_break = ifelse(
        seq_diff_sec > seq_tim_delta * 60 & seq_dist_m > seq_dist_delta,
        1, 0),
      survey_break_time = ifelse(
        seq_diff_sec > seq_tim_delta * 60,
        1, 0),
      survey_break_space = ifelse(
        seq_dist_m > seq_dist_delta,
        1, 0),
    )

  return(dfo)
}

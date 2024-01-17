#' Calculate diff from best
#'
#' @param d
#'
#' @return Columns with raced differences from the best
#' @export
#'
#' @examples
#' calc_diff(d)

calc_diff <- function(x) {
  rates <- dplyr::select(x, geoid, best, ends_with("_rate"), -starts_with("total_"), -ends_with("_no_rate"))  #get geoid, raced rate and best columns
  rates <- unique(rates) # 2/21/23 added due to prior step resulting in dupes for overcrowding
  diff_long <- pivot_longer(rates, 3:ncol(rates), names_to="measure_rate", values_to="rate") %>%   #pivot wide table to long on geoid & best cols
    mutate(diff=abs(best-rate)) %>%                                                     #calc diff from best
    mutate(measure_diff=sub("_rate", "_diff", measure_rate))                            #create new column names for diffs from best
  diff_wide <- diff_long %>% dplyr::select(geoid, measure_diff, diff) %>%      #pivot long table back to wide
    pivot_wider(names_from=measure_diff, values_from=diff)
  x <- x %>% left_join(diff_wide, by="geoid")                           #join new diff from best columns back to original table

  return(x)
}

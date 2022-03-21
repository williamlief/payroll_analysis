#' @name missing_summary
#' @description reports percent of missing data for all variables in data frame
#' @usage missing_summary(diamonds)
#'
#' @param df : the data frame to analyze
#' @param group : optional single grouping variable
#' 
#' @return a tibble
#' @export
#'
#' @examples
missing_summary <- function(df, group = NULL) {
  
  if(rlang::quo_is_null(enquo(group))) {
    miss <- df %>% 
      summarize_all(list(~mean(is.na(.)))) %>% 
      gather()
    
    return(miss)
  }
  
  group <- enquo(group)
  var <- rlang::as_name(group)
  if (!(var %in% colnames(df))) {
    stop(paste0("variable ", var, " not found"))
  }
  
  miss <- df %>% 
    group_by(year) %>% 
    summarize_all(list(~mean(is.na(.)))) %>% 
    gather(key = "var", value = "pct_na", -!!group) %>% 
    spread(key = !!group, value = pct_na)
  
  return(miss)
}
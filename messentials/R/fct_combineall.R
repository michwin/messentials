#' Factor Combine
#'
#' Combining factor levels easily
#' @param df A data set
#' @param .f A factor
#' @param level The name of the combination level
#' @param ... The variables to sum
#' @importFrom magrittr %>%
#' @export

fct_combineall <-
function(df, .f, level, ...){
  .f_quo <- rlang::enquo(.f)
  level_quo<-rlang::quo_name(rlang::enquo(level))
  vars<-rlang::quos(...)
  
  df %>%
    dplyr::mutate(lvl = as.character(!!.f_quo),
                  !!.f_quo := forcats::fct_collapse(!!.f_quo, !!level_quo := lvl),
                  !!.f_quo := as.character(!!.f_quo)) %>%
    dplyr::bind_rows(dplyr::mutate(df, !! .f_quo := as.character(!!.f_quo)))%>%
    dplyr::mutate(!!.f_quo := factor(!!.f_quo, levels = c(lvl, !!level_quo)))%>%
    dplyr::group_by(!!.f_quo)%>%
    dplyr::summarize_at(vars, function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE))}

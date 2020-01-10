#' Sum expression for variables of a Census table within a range and by a certain interval
#'
#' Creates a sum expression to be used with the "mutate" function
#'
#' @param table Census table ID, including "e" or zeros
#' @param start first row in range to be summed
#' @param end last row in range to be summed
#' @param int intervals at which the rows occur
#' 
#' @return expression that can be used in tandem with "mutate" to create new variable
#' 
#' @example
#' mutate(df, Employment_by_disability = !! census_sum("B18130e", 5, 40, 7))
#'
#' @export 

census_sum <- function(table, start, end, int) {
  
  rlang::parse_expr(paste(rep(table), seq(from = start, to = end, by = int), sep = "", collapse = " + "))
  
}

#' Sum expression for variables of a Census table
#'
#' Creates a sum expression to be used with the "mutate" function
#'
#' @param table Census table ID, including "e" or zeros
#' @param indices int Census table rows to be summed; multiple numbers should be concatenated
#' 
#' @return expression that can be used in tandem with "mutate" to create new variable
#' 
#' @example
#' mutate(df, Employment_by_disability = !! census_sum("B18130e", 5, 6, 10, 11, 14, 15))
#'
#' @export 
census_sum_select <- function(table, indices) {
  
  rlang::parse_expr(paste(rep(table),  indices, sep = "", collapse = " + "))
  
}
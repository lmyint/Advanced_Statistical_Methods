

# Test that the user has entered an expression equaivalent to that
# given as the first argument.
# @param correct_expression the correct or expected expression as a string
# @return TRUE or FALSE
# @examples
# \dontrun{
#   # Test that a user has entered a particular command
#   #
#   expr_equivalent_to('myVar <- c(3, 5, 7)')
# }
expr_equivalent_to <- function(correct_expression){
  e <- get("e", parent.frame())
  expr <- e$expr
  if(is.expression(expr))expr <- expr[[1]]
  correct <- parse(text=correct_expression)[[1]]
  results <- expectThat(expr, 
                        is_equivalent_to(correct, label=correct_expression),
                        label=deparse(expr))
  if( is(e, "dev") && !results$passed)swirl_out(results$message) 
  return(results$passed)
}

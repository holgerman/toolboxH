## scatterplot mit tooltip via ggvis (nicht interactive in knitr, aber in RStudio / R)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mtc PARAM_DESCRIPTION
#' @param xcol PARAM_DESCRIPTION
#' @param ycol PARAM_DESCRIPTION
#' @param tooltipcols PARAM_DESCRIPTION
#' @param on_click PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggvis_tooltip
#' @export 
ggvis_tooltip <- function (mtc,xcol, ycol, tooltipcols, on_click = T) {
  library(ggvis)
  ## example
  # mtc <- mtcars
  # ggvis_tooltip(mtc, 'mpg', 'wt', tooltipcols= c("am", "gear"), on_click = F)

  stopifnot("id225847815" %in% names(mtc)==F)
  mtc$id225847815 <- 1:nrow(mtc)

  if("data.table" %in% class(mtc)) {
    library(data.table)
    setDF(mtc)
  }


  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- mtc[mtc$id225847815 == x$id225847815,c(tooltipcols) , drop = F]
    paste0(names(row), ": ", format(row), collapse = "<br />")
  }

  if(on_click) modus = "click" else modus = "hover"

  mtc %>% ggvis(x = ~get(xcol), y = ~get(ycol), key := ~id225847815) %>%
    layer_points() %>%    add_tooltip(all_values,modus)

}

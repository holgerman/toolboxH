## An T F index for alle duplicated entries, not only the duplicated one
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vektor PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname allDuplicatedEntries
#' @export
allDuplicatedEntries <- function (vektor) {
  ## 150303 umgestellt auf datatable
  if(length(vektor)==0) return(0)


  vektab = data.table::data.table(myvektor = vektor, num = 1:length(vektor))
  duplicated_vals = vektab[duplicated(myvektor),myvektor]
  duplicated_entries = vektab[ myvektor %in% duplicated_vals]
  data.table::setkey(duplicated_entries, myvektor)
  duplicated_entries$num

}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fn PARAM_DESCRIPTION
#' @param sheet PARAM_DESCRIPTION, Default: 1
#' @param skip PARAM_DESCRIPTION, Default: 0
#' @param na PARAM_DESCRIPTION, Default: ''
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'
#' @rdname read_excel2
#' @export
#' @import readxl
#' @import stringr
#' @import data.table
#'
#'

read_excel2 = function(fn, sheet = 1, skip =0, na = "", ...) {
  ## inspired by the idea of data.table::fread to automatically switch to text if a column is not as expected to be numeric, logical etc.

  sheetnames = readxl:::xlsx_sheets(fn)
  if(is.numeric(sheet)) sheetnum=sheet else sheetnum =  which(sheetnames==sheet)
  if(is.numeric(sheetnum)==F) stop("not numeric sheetnum created in function read_excel2, please debug")

  # myfile_list = tryCatch(read_excel(fn,sheet = sheet, ...),warning=function(w) return(list(read_excel(fn,sheet = sheet, ...),w))) ## leider nur eine warning, bruache aber alle
  ## better this: https://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning

  withWarnings <- function(fn,sheet = sheet) {
    myWarnings <- NULL
    wHandler <- function(w) {
      myWarnings <<- c(myWarnings, list(w))
      invokeRestart("muffleWarning")
    }
    val <- withCallingHandlers(readxl::read_excel(fn,sheet , skip =skip, na = na,...), warning = wHandler)
    list(value = val, warnings = myWarnings)
  }

  myfile_list = withWarnings(fn, sheet)

  # str(myfile_list)
  newwarning  = sapply(myfile_list$warnings, "[[", 1)
  # print(newwarning)
  if(any(grepl("expecting", newwarning))) {
    newwarning = grep("expecting", newwarning, value = T)
    cols2text = sort(as.numeric(unique(sapply(stringr::str_split(newwarning, " |\\]"),"[",2))))
    # print(last.warning)
    message("Reimporting \n", fn, "\n with column(s) ", paste(cols2text, collapse = ", "), " as type text, so you can ignore the warnings from read_excel import..."  )
    col_types <- readxl:::xlsx_col_types(path = fn,sheet = sheetnum-1, n = 1000,nskip = 1)
    stopifnot(length(col_types) == ncol(myfile_list$value))
    # print(col_types)
    # print(names(myfile))
    col_types[cols2text] <- 'text'
    # print(col_types)
    myfile = readxl::read_excel(fn,sheet = sheet, col_types=col_types,skip =skip, na = na, ...)
  } else myfile = myfile_list$value
  data.table::setDT(myfile)

  myfile

}

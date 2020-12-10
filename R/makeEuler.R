#' @title Create a Proportional Euler Diagram
#' @description Create a 2-way Proportional Euler Diagram
#' @param mytab1 First table with objects and effect sizes to compare
#' @param mytab2 Secib table with objects and effect sizes to compare
#' @param titletext Plot Title, Default: ''
#' @param legendtext Plot legend, default is the names of the input tables, if set must be acharacter vector of length 2, Default: NULL
#' @param plotteIntermediates for debug only, plot intermediate venn plots, Default: F
#' @param createSimplifiedLabels show only Number of objects with identical direction in label of the intersect , Default: F
#' @param object_colname Column name in table mytab1 with the objects to compare, Default: 'object'
#' @param direction_colname Column name in table mytab2 with the objects to compare, Default: 'direction'
#' @param increasingCutoff cutoff of same size, typical 0 for standard metric effect sizes or 1 for Foldchanges, Default: 0
#' @return Returns a plot and the code to generate the plot
#' @details DETAILS
#' @examples
#'  #EXAMPLE1
#'  library(eulerr)
#'library(toolboxH)
#'
#'tab1 = data.table(object = letters[1:10], direction = rnorm(10))
#'tab1
#'tab2 = data.table(object = letters[5:25], direction = rnorm(21))
#'tab2
#'
#'plottext = makeEuler(tab1 , tab2 , titletext = "Yes, we can Make a Title great again")
#' # savePlotInMultiFormats("exampleplot", 4, 3, plotecode = plottext)
#' @rdname makeEuler
#' @export
#'
#'

makeEuler  = function(mytab1,
                      mytab2,
                      titletext = "",
                      legendtext = NULL,
                      plotteIntermediates = F,
                      createSimplifiedLabels=F,
                      object_colname = 'object',
                      direction_colname = 'direction',
                      increasingCutoff = 0) {

  if(is.null(legendtext)) legendtext =c(deparse(substitute(mytab1)), deparse(substitute(mytab2)))

  mytab1 = copy(mytab1)
  mytab2 = copy(mytab2)

  mytab1[, object := get(object_colname)]
  mytab1[, direction := get(direction_colname)]

  mytab2[, object := get(object_colname)]
  mytab2[, direction := get(direction_colname)]



  tab1_all = mytab1[,object]

  tab1_inc = mytab1[direction >increasingCutoff ,object]


  tab1_dec = mytab1[direction <increasingCutoff ,object]



  qlist1 = venn3(tab1_all, tab1_dec, tab1_inc, plotte = plotteIntermediates)

  tab2_all = mytab2[,object]

  tab2_inc = mytab2[direction >increasingCutoff ,object]


  tab2_dec = mytab2[direction <increasingCutoff ,object]


  qlist2 = venn3(tab2_all, tab2_dec, tab2_inc, plotte = plotteIntermediates)

  qall = venn2(tab1_all,tab2_all, plotte = plotteIntermediates)

  if(createSimplifiedLabels==T) {


    qinc = venn2(tab1_inc,tab2_inc, plotte = plotteIntermediates)
    qdec = venn2(tab1_dec,tab2_dec, plotte = plotteIntermediates)


    label1 = paste0(intToUtf8(9650), qinc$q2 %>% length(), "  \n", intToUtf8(9660),qdec$q2 %>% length(), "  ")
    label2 = paste0(intToUtf8(9650),qinc$q3 %>% length(), "\n",intToUtf8(9660), qdec$q3 %>% length())
    label3 = paste0(intToUtf8(9650),qinc$q1 %>% length(), "\n",intToUtf8(9660), qdec$q1 %>% length())

  } else {

    # str(qall)
    only1_inc = sum(qall$q2 %in% tab1_inc)
    only1_dec = sum(qall$q2 %in% tab1_dec)


    both_inc1_inc2 = sum(qall$q1 %in% tab1_inc & qall$q1 %in% tab2_inc)
    both_dec1_dec2 = sum(qall$q1 %in% tab1_dec & qall$q1 %in% tab2_dec)

    both_inc1_dec2 = sum(qall$q1 %in% tab1_inc & qall$q1 %in% tab2_dec)
    both_dec1_inc2 = sum(qall$q1 %in% tab1_dec & qall$q1 %in% tab2_inc)


    only2_inc = sum(qall$q3 %in% tab2_inc)
    only2_dec = sum(qall$q3 %in% tab2_dec)


    # qcompl = venn4(tab1_inc,tab2_inc, tab1_dec,tab2_dec, plotte = plotte)

    label1 = paste0(intToUtf8(9650), only1_inc, "       \n", intToUtf8(9660),only1_dec, "       ")
    label3 = paste0("   ",intToUtf8(9650),intToUtf8(9650),both_inc1_inc2, " (",intToUtf8(9650),intToUtf8(9660),both_inc1_dec2, ")", "\n   ",intToUtf8(9660),intToUtf8(9660), both_dec1_dec2, " (", intToUtf8(9660),intToUtf8(9650), both_dec1_inc2, ")")
    label2 = paste0(intToUtf8(9650),only2_inc, "\n",intToUtf8(9660), only2_dec)

  }



  plotcolors = c(alpha("darkslateblue",0.8), alpha("lightblue", 0.8))

  plottext = paste0('print(eulerr:::plot.euler(eulerr::euler(list(with = tab1_all,  without = tab2_all), shape = "circle"), quantities = c(label1,label2,label3), fills = c(alpha("darkslateblue",0.8), alpha("lightblue", 0.8)), main = titletext,  labels = F, legend=list(labels = legendtext)))')  # legendGrob fuer legend
  eval(parse(text = plottext))
  plottext
}

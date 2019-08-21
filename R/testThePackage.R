testThePackage = function() {
  message("Testing match_hk")
  {test1 = data.table(a = 5:1, b = letters[5:1])
    test1
    test2 = data.table(a = c(1,1:2, NA), b = letters[c(1,1:3)])
    test2
    test3 = data.table(a = c(1,2:1, NA), b = letters[c(1,1:3)])
    test3

    toannot = data.table(x = c(1,NA, 1:2))
    goodresult = c('a',NA, 'a', "b")
    naivresult = c('a',NA, 'a', "a")
    testthat::expect_equal(toannot[, y:= test1[match_hk(toannot$x, test1$a), b]][,y], goodresult)

    testthat::expect_error(toannot[, y:= test2[match_hk(toannot$x, test2$a, showMessages = F), b]])

    testthat::expect_error(toannot[, y:= test2[match_hk(toannot$x, test2$a, makeunique = T,importcol = 'b'), b]])
    testthat::expect_equal(toannot[, y:= test2[match_hk(toannot$x, test2$a, makeunique = T,importcol = test2$b), b]][,y], goodresult)
    testthat::expect_equal(toannot[, y:= test2[match_hk(toannot$x, test2$a, makeunique = T,importcol = test2$b), b]][,y], goodresult)
    testthat::expect_equal(toannot[, y:= test2[match_hk(toannot$x, test2$a, testunique = F), b]][,y], goodresult)

    testthat::expect_error(toannot[, y:= test3[match_hk(toannot$x, test3$a, showMessages = F), b]])
    testthat::expect_equal(toannot[, y:= test3[match_hk(toannot$x, test3$a, testunique = F, showMessages = F), b]][,y], naivresult)

    testthat::expect_error(toannot[, y:= test3[match_hk(toannot$x, test3$a, makeunique = T,importcol = test3$b, showMessages = F), b]])
  }
  message("DONE witout errors\n---------------------")
}

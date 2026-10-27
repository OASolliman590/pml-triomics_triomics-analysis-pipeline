exprs <- parse('tests/testthat/test-D-T01.R')
selected <- Filter(function(x) is.call(x) && identical(x[[1]], quote(testthat::test_that)) && length(x)>=3 && is.character(x[[2]]) && identical(x[[2]],'D-T01 refuses stale paired signs and preserves the old pointer'), as.list(exprs))
stopifnot(length(selected)==1)
e <- new.env(parent=globalenv())
e$file.exists <- function(...) { paths <- c(...); result <- base::file.exists(paths); result[paths=='/tmp/triomics-m7-017-luna/native-roots.txt'] <- FALSE; result }
tryCatch(eval(selected[[1]],e),error=function(err) cat('CLEAN_ROOT_SIMULATION:',class(err)[1],conditionMessage(err),'\n'))

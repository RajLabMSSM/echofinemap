test_that("exists_not_empty returns TRUE for non-empty files", {

    tmp <- tempfile(fileext = ".txt")
    on.exit(unlink(tmp), add = TRUE)
    writeLines("hello", tmp)

    res <- echofinemap:::exists_not_empty(tmp)
    testthat::expect_true(res)
    testthat::expect_named(res, tmp)
})

test_that("exists_not_empty returns FALSE for empty files", {

    tmp <- tempfile(fileext = ".txt")
    on.exit(unlink(tmp), add = TRUE)
    file.create(tmp)

    res <- echofinemap:::exists_not_empty(tmp)
    testthat::expect_false(res)
})

test_that("exists_not_empty returns FALSE for nonexistent files", {

    res <- echofinemap:::exists_not_empty("/no/such/file.txt")
    testthat::expect_false(res)
})

test_that("exists_not_empty handles multiple files", {

    tmp1 <- tempfile(fileext = ".txt")
    tmp2 <- tempfile(fileext = ".txt")
    tmp3 <- tempfile(fileext = ".txt")
    on.exit(unlink(c(tmp1, tmp2, tmp3)), add = TRUE)
    writeLines("content", tmp1)
    file.create(tmp2)
    ## tmp3 does not exist

    res <- echofinemap:::exists_not_empty(c(tmp1, tmp2, tmp3))
    testthat::expect_length(res, 3)
    testthat::expect_true(res[[1]])
    testthat::expect_false(res[[2]])
    testthat::expect_false(res[[3]])
})

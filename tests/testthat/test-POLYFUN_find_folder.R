test_that("POLYFUN_find_folder returns system.file path by default", {

    res <- echofinemap:::POLYFUN_find_folder()
    ## Should return a non-empty string (the submodule path within the package)
    testthat::expect_true(is.character(res))
    testthat::expect_true(nchar(res) > 0)
})

test_that("POLYFUN_find_folder uses user-supplied path", {

    custom_path <- file.path(tempdir(), "custom_polyfun")
    res <- echofinemap:::POLYFUN_find_folder(polyfun_path = custom_path)
    testthat::expect_equal(res, custom_path)
})

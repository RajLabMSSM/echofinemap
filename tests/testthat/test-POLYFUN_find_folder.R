test_that("POLYFUN_find_folder returns system.file path by default", {

    polyfun_path <- system.file("tools", "polyfun", package = "echofinemap")
    if (polyfun_path == "") {
        testthat::skip("polyfun submodule not available")
    }
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

test_that("POLYFUN_find_folder errors when submodule not found", {

    polyfun_path <- system.file("tools", "polyfun", package = "echofinemap")
    if (polyfun_path != "") {
        testthat::skip("polyfun submodule is available; cannot test missing path")
    }
    testthat::expect_error(
        echofinemap:::POLYFUN_find_folder(),
        "Cannot find polyfun_path"
    )
})

test_that("FINEMAP_check_version_results keeps .cred for >= 1.3.1", {

    res <- echofinemap:::FINEMAP_check_version_results(
        finemap_version = "1.4.1",
        results_file = ".cred"
    )
    testthat::expect_equal(res, ".cred")
})

test_that("FINEMAP_check_version_results changes .cred to .snp for < 1.3.1", {

    res <- echofinemap:::FINEMAP_check_version_results(
        finemap_version = "1.2.0",
        results_file = ".cred"
    )
    testthat::expect_equal(res, ".snp")
})

test_that("FINEMAP_check_version_results keeps .snp regardless of version", {

    res <- echofinemap:::FINEMAP_check_version_results(
        finemap_version = "1.2.0",
        results_file = ".snp"
    )
    testthat::expect_equal(res, ".snp")
})

test_that("PAINTOR_check_superpopulation returns NULL when LD_ls provided", {

    res <- echofinemap:::PAINTOR_check_superpopulation(
        dat_ls = list(data.frame(x = 1)),
        LD_ls = list(matrix(1)),
        superpopulation = "EUR",
        verbose = FALSE
    )
    testthat::expect_null(res)
})

test_that("PAINTOR_check_superpopulation replicates single pop", {

    dat_ls <- list(data.frame(x = 1), data.frame(x = 2))
    res <- echofinemap:::PAINTOR_check_superpopulation(
        dat_ls = dat_ls,
        LD_ls = NULL,
        superpopulation = "EUR",
        verbose = FALSE
    )
    testthat::expect_equal(res, c("EUR", "EUR"))
})

test_that("PAINTOR_check_superpopulation errors on empty pop", {

    testthat::expect_error(
        echofinemap:::PAINTOR_check_superpopulation(
            dat_ls = list(data.frame(x = 1)),
            LD_ls = NULL,
            superpopulation = character(0),
            verbose = FALSE
        ),
        regexp = "Must provide"
    )
})

test_that("PAINTOR_check_superpopulation errors on length mismatch", {

    testthat::expect_error(
        echofinemap:::PAINTOR_check_superpopulation(
            dat_ls = list(data.frame(x = 1), data.frame(x = 2)),
            LD_ls = NULL,
            superpopulation = c("EUR", "AFR", "EAS"),
            verbose = FALSE
        ),
        regexp = "same length"
    )
})

test_that("PAINTOR_check_superpopulation passes matching-length pop", {

    dat_ls <- list(data.frame(x = 1), data.frame(x = 2))
    res <- echofinemap:::PAINTOR_check_superpopulation(
        dat_ls = dat_ls,
        LD_ls = NULL,
        superpopulation = c("EUR", "AFR"),
        verbose = FALSE
    )
    testthat::expect_equal(res, c("EUR", "AFR"))
})

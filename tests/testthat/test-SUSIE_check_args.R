test_that("SUSIE_check_args returns NULL when prior_weights is NULL", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        Effect = c(0.1, 0.2, 0.3)
    )

    res <- echofinemap:::SUSIE_check_args(
        prior_weights = NULL,
        dat = dat,
        keep_i = c(TRUE, TRUE, TRUE),
        max_causal = 5,
        rescale_priors = FALSE,
        verbose = FALSE
    )
    testthat::expect_null(res)
})

test_that("SUSIE_check_args subsets prior_weights by keep_i", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        Effect = c(0.1, 0.2)
    )
    prior_weights <- c(0.3, 0.5, 0.2)
    keep_i <- c(TRUE, FALSE, TRUE)

    res <- echofinemap:::SUSIE_check_args(
        prior_weights = prior_weights,
        dat = dat,
        keep_i = keep_i,
        max_causal = 5,
        rescale_priors = FALSE,
        verbose = FALSE
    )
    testthat::expect_equal(length(res), nrow(dat))
    testthat::expect_equal(res, c(0.3, 0.2))
})

test_that("SUSIE_check_args rescales priors when requested", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        Effect = c(0.1, 0.2)
    )
    prior_weights <- c(2, 3)
    keep_i <- c(TRUE, TRUE)

    res <- echofinemap:::SUSIE_check_args(
        prior_weights = prior_weights,
        dat = dat,
        keep_i = keep_i,
        max_causal = 5,
        rescale_priors = TRUE,
        verbose = FALSE
    )
    testthat::expect_equal(sum(res), 1)
})

test_that("SUSIE_check_args errors on length mismatch", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        Effect = c(0.1, 0.2, 0.3)
    )
    prior_weights <- c(0.3, 0.5)
    keep_i <- c(TRUE, TRUE)

    testthat::expect_error(
        echofinemap:::SUSIE_check_args(
            prior_weights = prior_weights,
            dat = dat,
            keep_i = keep_i,
            max_causal = 5,
            rescale_priors = FALSE,
            verbose = FALSE
        ),
        regexp = "same length"
    )
})

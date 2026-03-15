test_that("PAINTOR_check_required_cols returns NULL for NULL input", {

    res <- echofinemap:::PAINTOR_check_required_cols(
        dat = NULL,
        zscore_col = "ZSCORE",
        tstat_col = "tstat"
    )
    testthat::expect_null(res)
})

test_that("PAINTOR_check_required_cols renames zscore and tstat cols", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        CHR = c(4L, 4L),
        POS = c(100L, 200L),
        my_z = c(2.5, -1.0),
        my_t = c(3.0, -0.5),
        Effect = c(0.5, -0.2),
        StdErr = c(0.1, 0.3)
    )

    res <- echofinemap:::PAINTOR_check_required_cols(
        dat = dat,
        zscore_col = "my_z",
        tstat_col = "my_t"
    )
    testthat::expect_type(res, "list")
    testthat::expect_length(res, 1)
    testthat::expect_true("ZSCORE" %in% colnames(res[[1]]))
    testthat::expect_true("tstat" %in% colnames(res[[1]]))
})

test_that("PAINTOR_check_required_cols errors on missing zscore_col", {

    dat <- data.table::data.table(
        SNP = "rs1",
        tstat = 1.5
    )

    testthat::expect_error(
        echofinemap:::PAINTOR_check_required_cols(
            dat = dat,
            zscore_col = "ZSCORE",
            tstat_col = "tstat"
        ),
        "zscore_col.*must be present"
    )
})

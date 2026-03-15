test_that("PAINTOR_merge_datasets merges on CHR, POS, SNP", {

    d1 <- data.table::data.table(
        CHR = c(4L, 4L),
        POS = c(100L, 200L),
        SNP = c("rs1", "rs2"),
        ZSCORE = c(2.5, -1.0)
    )
    d2 <- data.table::data.table(
        CHR = c(4L, 4L),
        POS = c(100L, 200L),
        SNP = c("rs1", "rs2"),
        ZSCORE = c(1.0, -0.5)
    )
    dat_ls <- list(pop1 = d1, pop2 = d2)

    res <- echofinemap:::PAINTOR_merge_datasets(
        dat_ls = dat_ls,
        verbose = FALSE
    )
    testthat::expect_s3_class(res, "data.table")
    testthat::expect_equal(nrow(res), 2)
    ## ZSCORE columns should have suffixes
    zscore_cols <- grep("^ZSCORE", colnames(res), value = TRUE)
    testthat::expect_length(zscore_cols, 2)
})

test_that("PAINTOR_merge_datasets errors on missing required columns", {

    d1 <- data.table::data.table(
        CHR = 4L,
        POS = 100L,
        SNP = "rs1",
        ZSCORE = 2.5
    )
    d2 <- data.table::data.table(
        CHR = 4L,
        POS = 100L,
        SNP = "rs1"
        ## Missing ZSCORE
    )
    dat_ls <- list(pop1 = d1, pop2 = d2)

    testthat::expect_error(
        echofinemap:::PAINTOR_merge_datasets(
            dat_ls = dat_ls,
            verbose = FALSE
        ),
        "missing the following required column"
    )
})

test_that("PAINTOR_merge_datasets handles single dataset", {

    d1 <- data.table::data.table(
        CHR = 4L,
        POS = 100L,
        SNP = "rs1",
        ZSCORE = 2.5
    )
    dat_ls <- list(pop1 = d1)

    res <- echofinemap:::PAINTOR_merge_datasets(
        dat_ls = dat_ls,
        verbose = FALSE
    )
    testthat::expect_equal(nrow(res), 1)
    testthat::expect_true("ZSCORE" %in% colnames(res))
})

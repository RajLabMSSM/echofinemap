test_that("PAINTOR_get_suffixes extracts suffixes from merged columns", {

    dat_merged <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        ZSCORE.1 = c(2.5, -1.0),
        ZSCORE.2 = c(1.0, -0.5),
        tstat.1 = c(3.0, -0.5),
        tstat.2 = c(2.0, -1.0)
    )

    res <- echofinemap:::PAINTOR_get_suffixes(dat_merged)
    testthat::expect_equal(res, c("1", "2"))
})

test_that("PAINTOR_get_suffixes works with single ZSCORE column", {

    dat_merged <- data.table::data.table(
        SNP = "rs1",
        ZSCORE.1 = 2.5
    )

    res <- echofinemap:::PAINTOR_get_suffixes(dat_merged)
    testthat::expect_equal(res, "1")
})

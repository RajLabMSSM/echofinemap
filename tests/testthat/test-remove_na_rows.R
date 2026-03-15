test_that("remove_na_rows removes rows with NAs in specified columns", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3", "rs4"),
        Effect = c(0.5, NA, 0.3, 0.1),
        P = c(0.01, 0.02, NA, 0.04),
        StdErr = c(0.1, 0.2, 0.3, 0.4)
    )

    res <- echofinemap:::remove_na_rows(
        dat = dat,
        cols = c("Effect", "P"),
        verbose = FALSE
    )
    testthat::expect_equal(nrow(res), 2)
    testthat::expect_true(all(c("rs1", "rs4") %in% res$SNP))
})

test_that("remove_na_rows ignores non-existent columns", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        Effect = c(0.5, 0.3)
    )

    res <- echofinemap:::remove_na_rows(
        dat = dat,
        cols = c("Effect", "NonExistent"),
        verbose = FALSE
    )
    testthat::expect_equal(nrow(res), 2)
})

test_that("remove_na_rows returns all rows when no NAs", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        Effect = c(0.5, 0.3, 0.1),
        P = c(0.01, 0.02, 0.03)
    )

    res <- echofinemap:::remove_na_rows(
        dat = dat,
        cols = c("Effect", "P"),
        verbose = FALSE
    )
    testthat::expect_equal(nrow(res), 3)
})

test_that("remove_na_rows uses default columns", {

    dat <- data.table::data.table(
        SNP = c("rs1", NA, "rs3"),
        Effect = c(0.5, 0.3, 0.1),
        P = c(0.01, 0.02, 0.03),
        StdErr = c(0.1, 0.2, 0.3),
        MAF = c(0.1, 0.2, 0.3)
    )

    res <- echofinemap:::remove_na_rows(dat = dat, verbose = FALSE)
    ## One row removed because SNP is NA
    testthat::expect_equal(nrow(res), 2)
})

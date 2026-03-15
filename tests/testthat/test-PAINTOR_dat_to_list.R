test_that("PAINTOR_dat_to_list wraps data.frame in list", {

    df <- data.frame(SNP = c("rs1", "rs2"), ZSCORE = c(1.5, -0.5))
    res <- echofinemap:::PAINTOR_dat_to_list(df)

    testthat::expect_type(res, "list")
    testthat::expect_length(res, 1)
    testthat::expect_equal(names(res), "dataset1")
    testthat::expect_equal(res[[1]], df)
})

test_that("PAINTOR_dat_to_list wraps data.table in list", {

    dt <- data.table::data.table(SNP = "rs1", ZSCORE = 2.0)
    res <- echofinemap:::PAINTOR_dat_to_list(dt)

    testthat::expect_type(res, "list")
    testthat::expect_length(res, 1)
})

test_that("PAINTOR_dat_to_list wraps matrix in list", {

    m <- matrix(1:4, nrow = 2)
    res <- echofinemap:::PAINTOR_dat_to_list(m)

    testthat::expect_type(res, "list")
    testthat::expect_length(res, 1)
})

test_that("PAINTOR_dat_to_list passes through list", {

    lst <- list(
        pop1 = data.frame(SNP = "rs1", ZSCORE = 1.0),
        pop2 = data.frame(SNP = "rs1", ZSCORE = 0.5)
    )
    res <- echofinemap:::PAINTOR_dat_to_list(lst)

    testthat::expect_type(res, "list")
    testthat::expect_length(res, 2)
    testthat::expect_equal(names(res), c("pop1", "pop2"))
})

test_that("PAINTOR_dat_to_list auto-names unnamed list", {

    lst <- list(
        data.frame(SNP = "rs1", ZSCORE = 1.0),
        data.frame(SNP = "rs2", ZSCORE = 0.5)
    )
    res <- echofinemap:::PAINTOR_dat_to_list(lst)

    testthat::expect_equal(names(res), c("dataset1", "dataset2"))
})

test_that("PAINTOR_dat_to_list uses custom prefix", {

    df <- data.frame(SNP = "rs1", val = 1)
    res <- echofinemap:::PAINTOR_dat_to_list(df, prefix = "pop")

    testthat::expect_equal(names(res), "pop1")
})

test_that("PAINTOR_dat_to_list errors on invalid input", {

    testthat::expect_error(
        echofinemap:::PAINTOR_dat_to_list("not_a_dataset"),
        "must be one of"
    )
})

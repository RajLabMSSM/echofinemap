test_that("COJO_args constructs basic command string", {

    res <- echofinemap:::COJO_args(
        cojo_file = "sumstats.ma",
        bfile = "test_plink",
        out = "results"
    )
    testthat::expect_true(is.character(res))
    testthat::expect_true(grepl("--bfile test_plink", res))
    testthat::expect_true(grepl("--cojo-file sumstats.ma", res))
    testthat::expect_true(grepl("--out results", res))
})

test_that("COJO_args includes optional flags when set", {

    res <- echofinemap:::COJO_args(
        cojo_file = "sumstats.ma",
        bfile = "test_plink",
        out = "results",
        cojo_slct = TRUE,
        maf = 0.01,
        cojo_p = 5e-8,
        cojo_wind = 500
    )
    testthat::expect_true(grepl("--cojo-slct", res))
    testthat::expect_true(grepl("--maf 0.01", res))
    testthat::expect_true(grepl("--cojo-p 5e-08", res))
    testthat::expect_true(grepl("--cojo-wind 500", res))
})

test_that("COJO_args omits NULL optional arguments", {

    res <- echofinemap:::COJO_args(
        cojo_file = "sumstats.ma",
        bfile = "test_plink",
        out = "results",
        cojo_slct = NULL,
        cojo_joint = NULL
    )
    testthat::expect_false(grepl("--cojo-slct", res))
    testthat::expect_false(grepl("--cojo-joint", res))
})

test_that("COJO_args includes conditional analysis flag", {

    res <- echofinemap:::COJO_args(
        cojo_file = "sumstats.ma",
        bfile = "test_plink",
        out = "results",
        cojo_cond = "cond_snps.txt"
    )
    testthat::expect_true(grepl("--cojo-cond cond_snps.txt", res))
})

test_that("COJO_args includes cojo_gc flag", {

    res <- echofinemap:::COJO_args(
        cojo_file = "sumstats.ma",
        bfile = "test_plink",
        out = "results",
        cojo_gc = TRUE
    )
    testthat::expect_true(grepl("--cojo-gc", res))
})

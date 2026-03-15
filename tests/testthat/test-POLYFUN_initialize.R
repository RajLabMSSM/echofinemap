test_that("POLYFUN_initialize creates directory and returns path", {

    locus_dir <- file.path(tempdir(), "test_polyfun_init")
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::POLYFUN_initialize(locus_dir = locus_dir)
    testthat::expect_equal(res, file.path(locus_dir, "PolyFun"))
    testthat::expect_true(dir.exists(res))
})

test_that("POLYFUN_initialize is idempotent", {

    locus_dir <- file.path(tempdir(), "test_polyfun_init2")
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    res1 <- echofinemap:::POLYFUN_initialize(locus_dir = locus_dir)
    res2 <- echofinemap:::POLYFUN_initialize(locus_dir = locus_dir)
    testthat::expect_equal(res1, res2)
    testthat::expect_true(dir.exists(res2))
})

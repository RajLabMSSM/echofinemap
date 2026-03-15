test_that("FINEMAP_construct_datak returns NULL when prior_k is NULL", {

    res <- echofinemap:::FINEMAP_construct_datak(
        prior_k = NULL,
        locus_dir = tempdir(),
        n_causal = 5,
        verbose = FALSE
    )
    testthat::expect_null(res)
})

test_that("FINEMAP_construct_datak writes file and returns path", {

    locus_dir <- file.path(tempdir(), "test_datak")
    dir.create(file.path(locus_dir, "FINEMAP"),
               showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::FINEMAP_construct_datak(
        prior_k = 0.5,
        locus_dir = locus_dir,
        n_causal = 3,
        verbose = FALSE
    )
    testthat::expect_true(is.character(res))
    testthat::expect_true(file.exists(res))
    ## Read back and check: 3 columns, all 0.5
    content <- data.table::fread(res, header = FALSE)
    testthat::expect_equal(ncol(content), 3)
    testthat::expect_true(all(unlist(content) == 0.5))
})

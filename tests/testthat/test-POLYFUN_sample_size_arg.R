test_that("POLYFUN_sample_size_arg returns --n when N column absent", {

    testthat::skip_if_not_installed("echodata")

    ## Write a small sumstats file without N/N_cases/N_controls
    tmp_path <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp_path), add = TRUE)

    dat <- data.table::data.table(
        SNP = paste0("rs", 1:10),
        CHR = rep(4L, 10),
        POS = seq(100, 1000, 100),
        Effect = runif(10, -1, 1),
        StdErr = runif(10, 0.01, 0.5),
        P = runif(10, 0, 1)
    )
    data.table::fwrite(dat, tmp_path, sep = "\t")

    res <- echofinemap:::POLYFUN_sample_size_arg(
        fullSS_path = tmp_path,
        sample_size = 50000,
        verbose = FALSE
    )
    testthat::expect_equal(res, "--n 50000")
})

test_that("POLYFUN_sample_size_arg returns NULL when N col present", {

    testthat::skip_if_not_installed("echodata")

    tmp_path <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp_path), add = TRUE)

    dat <- data.table::data.table(
        SNP = paste0("rs", 1:10),
        N = rep(50000L, 10),
        Effect = runif(10),
        P = runif(10)
    )
    data.table::fwrite(dat, tmp_path, sep = "\t")

    testthat::expect_warning(
        res <- echofinemap:::POLYFUN_sample_size_arg(
            fullSS_path = tmp_path,
            sample_size = 30000,
            nrows = 10,
            verbose = FALSE
        ),
        "Cannot both specify"
    )
    testthat::expect_null(res)
})

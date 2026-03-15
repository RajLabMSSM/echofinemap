test_that("COJO_get_stepwise_results returns NULL for NULL path", {

    res <- echofinemap:::COJO_get_stepwise_results(
        jma_cojo_path = NULL,
        verbose = FALSE
    )
    testthat::expect_null(res)
})

test_that("COJO_get_stepwise_results returns NULL for non-existent path", {

    res <- echofinemap:::COJO_get_stepwise_results(
        jma_cojo_path = file.path(tempdir(), "nonexistent.jma.cojo"),
        verbose = FALSE
    )
    testthat::expect_null(res)
})

test_that("COJO_get_stepwise_results reads valid file", {

    tmp <- tempfile(fileext = ".jma.cojo")
    on.exit(unlink(tmp), add = TRUE)

    mock_data <- data.table::data.table(
        Chr = 4L,
        SNP = c("rs1", "rs2"),
        bp = c(100L, 200L),
        refA = c("A", "G"),
        freq = c(0.3, 0.5),
        freq_geno = c(0.31, 0.49),
        b = c(0.1, 0.2),
        se = c(0.01, 0.02),
        p = c(1e-8, 1e-6),
        n = c(1000L, 1000L),
        bJ = c(0.09, 0.19),
        bJ_se = c(0.011, 0.021),
        pJ = c(1e-7, 1e-5),
        LD_r = c(0.1, 0.2)
    )
    data.table::fwrite(mock_data, tmp, sep = "\t")

    res <- echofinemap:::COJO_get_stepwise_results(
        jma_cojo_path = tmp,
        verbose = FALSE
    )
    testthat::expect_true(data.table::is.data.table(res))
    testthat::expect_equal(nrow(res), 2)
    ## LD_r2 should be computed
    testthat::expect_true("LD_r2" %in% colnames(res))
    testthat::expect_equal(res$LD_r2, res$LD_r^2)
})

test_that("COJO_get_conditional_results returns empty list for NULL paths", {

    res <- echofinemap:::COJO_get_conditional_results(
        cma_cojo_path = NULL,
        cond_path = NULL,
        verbose = FALSE
    )
    testthat::expect_true(is.list(res))
    testthat::expect_length(res, 0)
})

test_that("COJO_get_conditional_results reads valid cma file", {

    tmp_cma <- tempfile(fileext = ".cma.cojo")
    on.exit(unlink(tmp_cma), add = TRUE)

    mock_cma <- data.table::data.table(
        Chr = 4L,
        SNP = c("rs1", "rs2", "rs3"),
        bp = c(100L, 200L, 300L),
        refA = c("A", "G", "T"),
        freq = c(0.3, 0.5, 0.4),
        freq_geno = c(0.31, 0.49, 0.41),
        b = c(0.1, 0.2, 0.15),
        se = c(0.01, 0.02, 0.015),
        p = c(1e-8, 1e-6, 1e-4),
        n = c(1000L, 1000L, 1000L),
        bC = c(0.09, 0.19, 0.14),
        bC_se = c(0.011, 0.021, 0.016),
        pC = c(0.001, 0.04, 0.1)
    )
    data.table::fwrite(mock_cma, tmp_cma, sep = "\t")

    res <- echofinemap:::COJO_get_conditional_results(
        cma_cojo_path = tmp_cma,
        cond_path = NULL,
        pC_max = 0.05,
        verbose = FALSE
    )
    testthat::expect_true("cma.cojo" %in% names(res))
    ## Only SNPs with pC <= 0.05 should remain
    testthat::expect_true(all(res[["cma.cojo"]]$pC <= 0.05))
    testthat::expect_equal(nrow(res[["cma.cojo"]]), 2)
})

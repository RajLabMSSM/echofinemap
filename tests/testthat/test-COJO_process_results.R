test_that("COJO_process_results merges stepwise results into dat", {

    tmp_dir <- file.path(tempdir(), "test_cojo_process")
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## Create a mock jma.cojo file
    jma_path <- file.path(tmp_dir, "cojo.jma.cojo")
    mock_jma <- data.table::data.table(
        Chr = c(4L, 4L),
        SNP = c("rs1", "rs3"),
        bp = c(100L, 300L),
        refA = c("A", "G"),
        freq = c(0.3, 0.4),
        freq_geno = c(0.31, 0.41),
        b = c(0.1, 0.15),
        se = c(0.01, 0.015),
        p = c(1e-8, 1e-5),
        n = c(1000L, 1000L),
        bJ = c(0.09, 0.14),
        bJ_se = c(0.011, 0.016),
        pJ = c(0.001, 0.1),
        LD_r = c(0.1, 0.05)
    )
    data.table::fwrite(mock_jma, jma_path, sep = "\t")

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        Effect = c(0.1, 0.2, 0.15)
    )

    paths <- list("jma.cojo" = jma_path)
    res <- echofinemap:::COJO_process_results(
        dat = dat,
        paths = paths,
        credset_thresh = 0.95,
        freq_cutoff = 0.1,
        verbose = FALSE
    )
    testthat::expect_true(data.table::is.data.table(res))
    testthat::expect_equal(nrow(res), 3)
    testthat::expect_true("bJ" %in% colnames(res))
    testthat::expect_true("CS" %in% colnames(res))
    ## rs1 has pJ=0.001 < 0.05, so CS should be 1
    testthat::expect_equal(res[res$SNP == "rs1"]$CS, 1)
    ## rs2 was not in jma results, so bJ should be NA
    testthat::expect_true(is.na(res[res$SNP == "rs2"]$bJ))
})

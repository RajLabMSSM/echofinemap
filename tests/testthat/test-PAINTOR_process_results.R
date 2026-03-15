test_that("PAINTOR_process_results processes single-dataset results", {

    tmp_dir <- file.path(tempdir(), "test_pt_results_1")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## Create mock PAINTOR results file
    res_file <- file.path(tmp_dir, "results.txt")
    res_data <- data.table::data.table(
        RSID = c("rs1", "rs2", "rs3"),
        Posterior_Prob = c(0.98, 0.01, 0.01)
    )
    data.table::fwrite(res_data, res_file, sep = "\t")

    dat_merged <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        CHR = c(4L, 4L, 4L),
        POS = c(100L, 200L, 300L)
    )

    res_paths <- list(
        RESname = list(pop1 = res_file)
    )

    res <- echofinemap:::PAINTOR_process_results(
        dat_merged = dat_merged,
        res_paths = res_paths,
        credset_thresh = 0.95,
        verbose = FALSE
    )
    testthat::expect_s3_class(res, "data.table")
    testthat::expect_true("PP" %in% colnames(res))
    testthat::expect_true("CS" %in% colnames(res))
    ## rs1 should be in credible set (PP >= 0.95)
    testthat::expect_equal(res[res$SNP == "rs1", ]$CS, 1)
    testthat::expect_equal(res[res$SNP == "rs2", ]$CS, 0)
})

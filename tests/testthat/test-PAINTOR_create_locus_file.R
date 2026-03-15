test_that("PAINTOR_create_locus_file writes locus file", {

    tmp_dir <- file.path(tempdir(), "test_pt_locus")
    pt_dir <- file.path(tmp_dir, "PAINTOR")
    locus_dir <- file.path(tmp_dir, "BST1")
    dir.create(pt_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    dat_merged <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        CHR = c(4L, 4L, 4L),
        POS = c(100L, 200L, 300L),
        ZSCORE = c(2.5, -1.0, 0.5)
    )

    path <- echofinemap:::PAINTOR_create_locus_file(
        dat_merged = dat_merged,
        locus_dir = locus_dir,
        PT_results_path = pt_dir,
        verbose = FALSE
    )
    testthat::expect_true(file.exists(path))
    ## File should be named after the locus
    testthat::expect_equal(basename(path), "BST1")

    result <- data.table::fread(path)
    ## SNP column should have been renamed to RSID
    testthat::expect_true("RSID" %in% colnames(result))
    testthat::expect_false("SNP" %in% colnames(result))
    testthat::expect_equal(nrow(result), 3)
})

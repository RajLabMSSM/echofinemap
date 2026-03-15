test_that("FINEMAP_import_data_config parses config file", {

    tmp_dir <- file.path(tempdir(), "test_fm_config_1")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## Create a mock data.config file
    config_lines <- c(
        "rank config log10bf prob k",
        "1 rs1,rs2 10.5 0.97 2",
        "2 rs3 5.2 0.80 1",
        "3 rs4,rs5 2.1 0.50 2"
    )
    writeLines(config_lines, file.path(fm_dir, "data.config"))

    res <- echofinemap:::FINEMAP_import_data_config(
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        pvalue_thresh = NULL,
        verbose = FALSE
    )
    testthat::expect_s3_class(res, "data.table")
    ## Only config with prob >= 0.95 should remain
    testthat::expect_true(all(res$prob >= 0.95))
    ## The config "rs1,rs2" should be melted into individual SNP rows
    testthat::expect_true("rs1" %in% res$SNP)
    testthat::expect_true("rs2" %in% res$SNP)
})

test_that("FINEMAP_import_data_config returns empty table when nothing passes threshold", {

    tmp_dir <- file.path(tempdir(), "test_fm_config_2")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    config_lines <- c(
        "rank config log10bf prob k",
        "1 rs1 5.2 0.50 1"
    )
    writeLines(config_lines, file.path(fm_dir, "data.config"))

    res <- echofinemap:::FINEMAP_import_data_config(
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        pvalue_thresh = NULL,
        verbose = FALSE
    )
    testthat::expect_equal(nrow(res), 0)
    testthat::expect_true("SNP" %in% colnames(res))
    testthat::expect_true("prob" %in% colnames(res))
})

test_that("FINEMAP_import_data_config returns max_causal with flag", {

    tmp_dir <- file.path(tempdir(), "test_fm_config_3")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    config_lines <- c(
        "rank config log10bf prob k",
        "1 rs1,rs2,rs3 15.0 0.99 3",
        "2 rs4 5.0 0.96 1"
    )
    writeLines(config_lines, file.path(fm_dir, "data.config"))

    res <- echofinemap:::FINEMAP_import_data_config(
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        pvalue_thresh = NULL,
        return_max_causal = TRUE,
        verbose = FALSE
    )
    testthat::expect_type(res, "list")
    testthat::expect_true("config_dat" %in% names(res))
    testthat::expect_true("max_causal" %in% names(res))
    testthat::expect_equal(res$max_causal, 3)
})

test_that("FINEMAP_import_data_config top_config_only returns one row", {

    tmp_dir <- file.path(tempdir(), "test_fm_config_top")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    config_lines <- c(
        "rank config log10bf prob k",
        "1 rs1,rs2 15.0 0.99 2",
        "2 rs3 10.0 0.97 1"
    )
    writeLines(config_lines, file.path(fm_dir, "data.config"))

    res <- echofinemap:::FINEMAP_import_data_config(
        locus_dir = tmp_dir,
        credset_thresh = 0.0,
        pvalue_thresh = NULL,
        top_config_only = TRUE,
        verbose = FALSE
    )
    ## Top config has 2 SNPs, so melted result should have 2 rows
    testthat::expect_equal(nrow(res), 2)
    testthat::expect_setequal(res$SNP, c("rs1", "rs2"))
})

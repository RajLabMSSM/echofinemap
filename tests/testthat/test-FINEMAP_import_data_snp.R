test_that("FINEMAP_import_data_snp reads and filters by prob", {

    tmp_dir <- file.path(tempdir(), "test_fm_snp_1")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    snp_lines <- c(
        "index rsid chromosome position allele1 allele2 maf beta se z prob",
        "1 rs1 4 15723925 A T 0.1 0.5 0.1 5.0 0.98",
        "2 rs2 4 15723950 C G 0.2 0.3 0.15 2.0 0.40",
        "3 rs3 4 15724000 A G 0.05 0.2 0.2 1.0 0.96"
    )
    writeLines(snp_lines, file.path(fm_dir, "data.snp"))

    res <- echofinemap:::FINEMAP_import_data_snp(
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        verbose = FALSE
    )
    testthat::expect_s3_class(res, "data.table")
    ## Only SNPs with prob > 0.95 should remain
    testthat::expect_equal(nrow(res), 2)
    testthat::expect_true(all(res$prob > 0.95))
})

test_that("FINEMAP_import_data_snp falls back to prob_group", {

    tmp_dir <- file.path(tempdir(), "test_fm_snp_2")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## Simulate file with prob_group instead of prob
    snp_lines <- c(
        "index rsid chromosome position allele1 allele2 maf beta se z prob_group",
        "1 rs1 4 15723925 A T 0.1 0.5 0.1 5.0 0.99",
        "2 rs2 4 15723950 C G 0.2 0.3 0.15 2.0 0.10"
    )
    writeLines(snp_lines, file.path(fm_dir, "data.snp"))

    res <- echofinemap:::FINEMAP_import_data_snp(
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        verbose = FALSE
    )
    ## prob column should have been created from prob_group
    testthat::expect_true("prob" %in% colnames(res))
    testthat::expect_equal(nrow(res), 1)
})

test_that("FINEMAP_import_data_snp returns 0 rows if none pass threshold", {

    tmp_dir <- file.path(tempdir(), "test_fm_snp_3")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    snp_lines <- c(
        "index rsid chromosome position allele1 allele2 maf beta se z prob",
        "1 rs1 4 15723925 A T 0.1 0.5 0.1 5.0 0.50",
        "2 rs2 4 15723950 C G 0.2 0.3 0.15 2.0 0.10"
    )
    writeLines(snp_lines, file.path(fm_dir, "data.snp"))

    res <- echofinemap:::FINEMAP_import_data_snp(
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        verbose = FALSE
    )
    testthat::expect_equal(nrow(res), 0)
})

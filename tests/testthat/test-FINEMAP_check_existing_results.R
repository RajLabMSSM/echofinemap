test_that("FINEMAP_check_existing_results returns NULL when no files exist", {

    tmp_dir <- file.path(tempdir(), "test_fm_existing_1")
    dir.create(file.path(tmp_dir, "FINEMAP"),
               recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        Effect = c(0.5, 0.3),
        StdErr = c(0.1, 0.2)
    )

    res <- echofinemap:::FINEMAP_check_existing_results(
        dat = dat,
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        finemap_version = package_version("1.4.1"),
        force_new = FALSE,
        verbose = FALSE
    )
    testthat::expect_null(res)
})

test_that("FINEMAP_check_existing_results returns NULL when force_new=TRUE", {

    tmp_dir <- file.path(tempdir(), "test_fm_existing_2")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## Create a dummy file that would normally be found
    writeLines("rsid prob", file.path(fm_dir, "data.snp"))

    dat <- data.table::data.table(
        SNP = c("rs1"),
        Effect = c(0.5),
        StdErr = c(0.1)
    )

    res <- echofinemap:::FINEMAP_check_existing_results(
        dat = dat,
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        finemap_version = package_version("1.4.1"),
        force_new = TRUE,
        verbose = FALSE
    )
    testthat::expect_null(res)
    ## Directory should be recreated (old contents deleted)
    testthat::expect_true(dir.exists(fm_dir))
})

test_that("FINEMAP_check_existing_results cleans up old dir on force_new", {

    tmp_dir <- file.path(tempdir(), "test_fm_existing_3")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## Create files that should be removed
    writeLines("old data", file.path(fm_dir, "data.snp"))

    dat <- data.table::data.table(SNP = "rs1", Effect = 0.5, StdErr = 0.1)

    echofinemap:::FINEMAP_check_existing_results(
        dat = dat,
        locus_dir = tmp_dir,
        credset_thresh = 0.95,
        finemap_version = package_version("1.4.1"),
        force_new = TRUE,
        verbose = FALSE
    )

    ## Old data.snp should have been deleted
    testthat::expect_false(file.exists(file.path(fm_dir, "data.snp")))
})

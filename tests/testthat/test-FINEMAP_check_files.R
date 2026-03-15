test_that("FINEMAP_check_files returns empty when no files exist", {

    tmp_dir <- file.path(tempdir(), "test_fm_check_empty")
    dir.create(file.path(tmp_dir, "FINEMAP"),
               recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::FINEMAP_check_files(locus_dir = tmp_dir)
    testthat::expect_length(res, 0)
})

test_that("FINEMAP_check_files detects .snp file", {

    tmp_dir <- file.path(tempdir(), "test_fm_check_snp")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## Create a non-empty data.snp file
    writeLines("rsid prob", file.path(fm_dir, "data.snp"))

    res <- echofinemap:::FINEMAP_check_files(locus_dir = tmp_dir)
    testthat::expect_true(".snp" %in% res)
})

test_that("FINEMAP_check_files detects .config file", {

    tmp_dir <- file.path(tempdir(), "test_fm_check_config")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    writeLines("config prob", file.path(fm_dir, "data.config"))

    res <- echofinemap:::FINEMAP_check_files(locus_dir = tmp_dir)
    testthat::expect_true(".config" %in% res)
})

test_that("FINEMAP_check_files detects .cred files", {

    tmp_dir <- file.path(tempdir(), "test_fm_check_cred")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## FINEMAP v1.4 produces data.cred<N> files
    writeLines("index cred1 prob1", file.path(fm_dir, "data.cred2"))

    res <- echofinemap:::FINEMAP_check_files(locus_dir = tmp_dir)
    testthat::expect_true(".cred" %in% res)
})

test_that("FINEMAP_check_files detects multiple file types", {

    tmp_dir <- file.path(tempdir(), "test_fm_check_multi")
    fm_dir <- file.path(tmp_dir, "FINEMAP")
    dir.create(fm_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    writeLines("index cred1 prob1", file.path(fm_dir, "data.cred1"))
    writeLines("rsid prob", file.path(fm_dir, "data.snp"))
    writeLines("config prob", file.path(fm_dir, "data.config"))

    res <- echofinemap:::FINEMAP_check_files(locus_dir = tmp_dir)
    testthat::expect_length(res, 3)
    testthat::expect_setequal(res, c(".cred", ".snp", ".config"))
})

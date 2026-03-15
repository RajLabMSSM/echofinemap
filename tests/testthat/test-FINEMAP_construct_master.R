test_that("FINEMAP_construct_master creates master file", {

    tmp_dir <- file.path(tempdir(), "test_fm_master_1")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    master_path <- echofinemap:::FINEMAP_construct_master(
        locus_dir = tmp_dir,
        n_samples = 25000,
        verbose = FALSE
    )
    testthat::expect_true(file.exists(master_path))
    testthat::expect_equal(basename(master_path), "master")

    lines <- readLines(master_path)
    testthat::expect_length(lines, 2)
    ## Header should contain required fields
    testthat::expect_true(grepl("z;ld;snp;config;cred;log;n_samples",
                                lines[1]))
    ## Data line should contain sample size
    testthat::expect_true(grepl("25000", lines[2]))
    ## Should reference FINEMAP directory files
    testthat::expect_true(grepl("FINEMAP/data\\.z", lines[2]))
    testthat::expect_true(grepl("FINEMAP/data\\.ld", lines[2]))
})

test_that("FINEMAP_construct_master adds k column when data.k_path provided", {

    tmp_dir <- file.path(tempdir(), "test_fm_master_2")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    data_k_path <- file.path(tmp_dir, "FINEMAP", "data.k")

    master_path <- echofinemap:::FINEMAP_construct_master(
        locus_dir = tmp_dir,
        n_samples = 10000,
        data.k_path = data_k_path,
        verbose = FALSE
    )

    lines <- readLines(master_path)
    ## Header should include k
    testthat::expect_true(grepl(";k$", lines[1]))
    ## Data line should reference data.k
    testthat::expect_true(grepl("data\\.k", lines[2]))
})

test_that("FINEMAP_construct_master creates directory", {

    tmp_dir <- file.path(tempdir(), "test_fm_master_3")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    testthat::expect_false(dir.exists(file.path(tmp_dir, "FINEMAP")))

    echofinemap:::FINEMAP_construct_master(
        locus_dir = tmp_dir,
        n_samples = 5000,
        verbose = FALSE
    )
    testthat::expect_true(dir.exists(file.path(tmp_dir, "FINEMAP")))
})

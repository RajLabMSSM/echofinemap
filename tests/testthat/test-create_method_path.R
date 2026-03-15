test_that("create_method_path builds correct path without LD_reference", {

    tmp_dir <- file.path(tempdir(), "test_method_path_1")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    path <- echofinemap::create_method_path(
        locus_dir = tmp_dir,
        finemap_method = "SUSIE"
    )
    testthat::expect_true(grepl("SUSIE", path))
    testthat::expect_true(grepl("SUSIE\\.tsv", path))
    ## Directory should be created
    testthat::expect_true(dir.exists(file.path(tmp_dir, "SUSIE")))
    ## By default, astrices are removed
    testthat::expect_false(grepl("\\*", path))
})

test_that("create_method_path includes astrices when requested", {

    tmp_dir <- file.path(tempdir(), "test_method_path_2")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    path <- echofinemap::create_method_path(
        locus_dir = tmp_dir,
        finemap_method = "FINEMAP",
        include_astrices = TRUE
    )
    testthat::expect_true(grepl("\\*", path))
})

test_that("create_method_path adds .gz when compress=TRUE", {

    tmp_dir <- file.path(tempdir(), "test_method_path_3")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    path <- echofinemap::create_method_path(
        locus_dir = tmp_dir,
        finemap_method = "ABF",
        compress = TRUE
    )
    testthat::expect_true(endsWith(path, ".gz"))
})

test_that("create_method_path incorporates LD_reference", {

    tmp_dir <- file.path(tempdir(), "test_method_path_4")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    path <- echofinemap::create_method_path(
        locus_dir = tmp_dir,
        finemap_method = "SUSIE",
        LD_reference = "1KGphase3"
    )
    testthat::expect_true(grepl("1KGphase3_LD", path))
    testthat::expect_true(grepl("SUSIE\\.tsv", path))
})

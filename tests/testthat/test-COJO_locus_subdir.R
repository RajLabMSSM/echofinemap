test_that("COJO_locus_subdir creates directory and returns path", {

    locus_dir <- file.path(tempdir(), "test_COJO_subdir")
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    cojo_dir <- echofinemap:::COJO_locus_subdir(locus_dir = locus_dir)
    testthat::expect_true(dir.exists(cojo_dir))
    testthat::expect_equal(cojo_dir, file.path(locus_dir, "COJO"))
})

test_that("COJO_locus_subdir is idempotent", {

    locus_dir <- file.path(tempdir(), "test_COJO_subdir2")
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    cojo_dir1 <- echofinemap:::COJO_locus_subdir(locus_dir = locus_dir)
    cojo_dir2 <- echofinemap:::COJO_locus_subdir(locus_dir = locus_dir)
    testthat::expect_equal(cojo_dir1, cojo_dir2)
    testthat::expect_true(dir.exists(cojo_dir2))
})

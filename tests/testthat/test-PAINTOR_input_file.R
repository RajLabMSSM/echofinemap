test_that("PAINTOR_input_file writes input.files", {

    tmp_dir <- file.path(tempdir(), "test_pt_input")
    pt_dir <- file.path(tmp_dir, "PAINTOR")
    dir.create(pt_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    locus_dir <- file.path(tmp_dir, "BST1")

    path <- echofinemap:::PAINTOR_input_file(
        locus_dir = locus_dir,
        PT_results_path = pt_dir,
        verbose = FALSE
    )
    testthat::expect_true(file.exists(path))
    testthat::expect_equal(basename(path), "input.files")

    content <- readLines(path)
    testthat::expect_equal(content[1], "BST1")
})

test_that("PAINTOR_list_annotations finds BED files", {

    tmp_dir <- file.path(tempdir(), "test_paintor_list_annot")
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ## Create mock BED files
    writeLines("chr1\t100\t200", file.path(tmp_dir, "FANTOM5_enh.bed"))
    writeLines("chr1\t300\t400", file.path(tmp_dir, "DHS_signal.bed"))
    writeLines("chr1\t500\t600", file.path(tmp_dir, "ChromHMM_state.bed.gz"))

    res <- echofinemap:::PAINTOR_list_annotations(
        annot_dir = tmp_dir,
        categories = NULL,
        verbose = FALSE
    )
    testthat::expect_equal(length(res), 3)
})

test_that("PAINTOR_list_annotations filters by category", {

    tmp_dir <- file.path(tempdir(), "test_paintor_list_annot2")
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    writeLines("chr1\t100\t200", file.path(tmp_dir, "FANTOM5_enh.bed"))
    writeLines("chr1\t300\t400", file.path(tmp_dir, "DHS_signal.bed"))
    writeLines("chr1\t500\t600", file.path(tmp_dir, "ChromHMM_state.bed"))

    res <- echofinemap:::PAINTOR_list_annotations(
        annot_dir = tmp_dir,
        categories = "FANTOM5",
        verbose = FALSE
    )
    testthat::expect_equal(length(res), 1)
    testthat::expect_true(grepl("FANTOM5", res))
})

test_that("PAINTOR_list_annotations returns empty for no matches", {

    tmp_dir <- file.path(tempdir(), "test_paintor_list_annot3")
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    writeLines("chr1\t100\t200", file.path(tmp_dir, "FANTOM5_enh.bed"))

    res <- echofinemap:::PAINTOR_list_annotations(
        annot_dir = tmp_dir,
        categories = "NONEXISTENT",
        verbose = FALSE
    )
    testthat::expect_length(res, 0)
})

test_that("PAINTOR_download_annotations returns uniform priors when disabled", {

    dat_merged <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        CHR = c(4L, 4L, 4L),
        POS = c(100L, 200L, 300L)
    )
    locus_dir <- file.path(tempdir(), "test_paintor_annot_dl")
    PT_results_path <- file.path(locus_dir, "PAINTOR")
    dir.create(PT_results_path, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::PAINTOR_download_annotations(
        dat_merged = dat_merged,
        locus_dir = locus_dir,
        PT_results_path = PT_results_path,
        use_annotations = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(is.character(res))
    testthat::expect_true(file.exists(res))

    ## Read the file: should have all 1s, one per SNP
    content <- data.table::fread(res)
    testthat::expect_equal(nrow(content), 3)
    testthat::expect_true(all(content[[1]] == 1))
})

test_that("PAINTOR_download_annotations returns list when annotations enabled", {

    dat_merged <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        CHR = c(4L, 4L),
        POS = c(100L, 200L)
    )
    locus_dir <- file.path(tempdir(), "test_paintor_annot_dl2")
    PT_results_path <- file.path(locus_dir, "PAINTOR")
    dir.create(PT_results_path, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    ## With use_annotations=TRUE but no sources specified,
    ## should return an empty list (no XGR, no PAINTOR, no ROADMAP)
    res <- echofinemap:::PAINTOR_download_annotations(
        dat_merged = dat_merged,
        locus_dir = locus_dir,
        PT_results_path = PT_results_path,
        use_annotations = TRUE,
        annot_paintor = NULL,
        annot_xgr = NULL,
        annot_roadmap = NULL,
        verbose = FALSE
    )
    testthat::expect_true(is.list(res))
    testthat::expect_length(res, 0)
})

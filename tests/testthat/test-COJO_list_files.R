test_that("COJO_list_files returns only existing files", {

    cojo_dir <- file.path(tempdir(), "test_cojo_list")
    dir.create(cojo_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(cojo_dir, recursive = TRUE), add = TRUE)

    prefix <- "cojo"
    ## Create only some of the expected files
    writeLines("test", file.path(cojo_dir, paste0(prefix, ".jma.cojo")))
    writeLines("test", file.path(cojo_dir, paste0(prefix, ".ldr.cojo")))

    res <- echofinemap:::COJO_list_files(
        cojo_dir = cojo_dir,
        prefix = prefix
    )
    testthat::expect_true(is.list(res))
    testthat::expect_true(all(file.exists(unlist(res))))
    testthat::expect_true("jma.cojo" %in% names(res))
    testthat::expect_true("ldr.cojo" %in% names(res))
    ## file.ma was not created, so should not appear
    testthat::expect_false("file.ma" %in% names(res))
})

test_that("COJO_list_files returns empty list when no files exist", {

    cojo_dir <- file.path(tempdir(), "test_cojo_list_empty")
    dir.create(cojo_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(cojo_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::COJO_list_files(
        cojo_dir = cojo_dir,
        prefix = "cojo"
    )
    testthat::expect_true(is.list(res))
    testthat::expect_length(res, 0)
})

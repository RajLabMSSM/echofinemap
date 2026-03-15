test_that("PAINTOR_datatype_handler creates path for GWAS data only", {

    tmp_dir <- file.path(tempdir(), "test_pdt_gwas")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::PAINTOR_datatype_handler(
        locus_dir = tmp_dir,
        dat_prefixes = "study1",
        annot_prefixes = NULL,
        verbose = FALSE
    )
    testthat::expect_true(grepl("GWAS", res))
    testthat::expect_true(grepl("PAINTOR", res))
    testthat::expect_true(dir.exists(res))
})

test_that("PAINTOR_datatype_handler creates path for annotations only", {

    tmp_dir <- file.path(tempdir(), "test_pdt_annot")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::PAINTOR_datatype_handler(
        locus_dir = tmp_dir,
        dat_prefixes = NULL,
        annot_prefixes = "roadmap",
        verbose = FALSE
    )
    testthat::expect_true(grepl("annotations", res))
    testthat::expect_true(grepl("PAINTOR", res))
})

test_that("PAINTOR_datatype_handler creates path for both types", {

    tmp_dir <- file.path(tempdir(), "test_pdt_both")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::PAINTOR_datatype_handler(
        locus_dir = tmp_dir,
        dat_prefixes = "study1",
        annot_prefixes = "roadmap",
        verbose = FALSE
    )
    testthat::expect_true(grepl("annotations", res))
    testthat::expect_true(grepl("study1--roadmap", res))
    testthat::expect_true(grepl("PAINTOR", res))
})

test_that("PAINTOR_datatype_handler errors when no data at all", {

    testthat::expect_error(
        echofinemap:::PAINTOR_datatype_handler(
            locus_dir = tempdir(),
            dat_prefixes = NULL,
            annot_prefixes = NULL,
            verbose = FALSE
        ),
        "Neither primary data nor annotations detected"
    )
})

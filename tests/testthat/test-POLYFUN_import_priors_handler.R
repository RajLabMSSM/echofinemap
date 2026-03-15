test_that("POLYFUN_import_priors_handler errors on multiple chromosomes", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        CHR = c(1L, 2L),
        POS = c(100L, 200L)
    )

    testthat::expect_error(
        echofinemap:::POLYFUN_import_priors_handler(
            dat = dat,
            mode = "precomputed",
            out.path = tempdir(),
            locus_dir = tempdir(),
            conda_env = "echoR_mini"
        ),
        "Only one unique chromosome"
    )
})

test_that("POLYFUN_import_priors_handler errors on empty priors files", {

    ## parametric mode requires snpvar files which won't exist
    tmp_dir <- file.path(tempdir(), "test_polyfun_priors_empty")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        CHR = c(4L, 4L),
        POS = c(100L, 200L)
    )

    testthat::expect_error(
        echofinemap:::POLYFUN_import_priors_handler(
            dat = dat,
            mode = "parametric",
            out.path = tmp_dir,
            locus_dir = tmp_dir,
            conda_env = "echoR_mini"
        ),
        "Could not identify files with priors"
    )
})

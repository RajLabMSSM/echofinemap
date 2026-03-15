test_that("POLYFUN_read_priors handles empty file", {

    tmp <- tempfile(fileext = ".tsv.gz")
    on.exit(unlink(tmp), add = TRUE)

    ## Write an empty file with header only
    data.table::fwrite(
        data.table::data.table(SNP = character(), SNPVAR = numeric()),
        tmp, sep = "\t"
    )

    res <- echofinemap:::POLYFUN_read_priors(
        snp_w_priors.file = tmp,
        verbose = FALSE
    )
    testthat::expect_true(data.table::is.data.table(res))
    testthat::expect_equal(nrow(res), 0)
})

test_that("POLYFUN_read_priors renames SNP_x column", {

    tmp <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp), add = TRUE)

    mock_priors <- data.table::data.table(
        SNP_x = c("rs1", "rs2"),
        SNP_y = c("rs1", "rs2"),
        SNPVAR = c(0.01, 0.02)
    )
    data.table::fwrite(mock_priors, tmp, sep = "\t")

    res <- echofinemap:::POLYFUN_read_priors(
        snp_w_priors.file = tmp,
        verbose = FALSE
    )
    ## SNP_x should be renamed to SNP
    testthat::expect_true("SNP" %in% colnames(res))
    testthat::expect_false("SNP_x" %in% colnames(res))
    ## SNP_y should be removed
    testthat::expect_false("SNP_y" %in% colnames(res))
})

test_that("POLYFUN_read_priors reads normal format", {

    tmp <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp), add = TRUE)

    mock_priors <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        SNPVAR = c(0.01, 0.02, 0.03)
    )
    data.table::fwrite(mock_priors, tmp, sep = "\t")

    res <- echofinemap:::POLYFUN_read_priors(
        snp_w_priors.file = tmp,
        verbose = FALSE
    )
    testthat::expect_equal(nrow(res), 3)
    testthat::expect_true("SNP" %in% colnames(res))
    testthat::expect_equal(res$SNP, c("rs1", "rs2", "rs3"))
})

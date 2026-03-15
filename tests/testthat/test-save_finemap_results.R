test_that("save_finemap_results writes a tab-separated file", {

    tmp_path <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp_path), add = TRUE)

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        PP = c(0.9, 0.1, 0.0),
        CS = c(1L, 0L, 0L)
    )

    echofinemap:::save_finemap_results(
        dat = dat,
        path = tmp_path,
        verbose = FALSE
    )
    testthat::expect_true(file.exists(tmp_path))

    result <- data.table::fread(tmp_path)
    testthat::expect_equal(nrow(result), 3)
    testthat::expect_equal(colnames(result), c("SNP", "PP", "CS"))
    testthat::expect_equal(result$SNP, c("rs1", "rs2", "rs3"))
})

test_that("save_finemap_results preserves NAs", {

    tmp_path <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp_path), add = TRUE)

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        PP = c(0.9, NA)
    )

    echofinemap:::save_finemap_results(
        dat = dat,
        path = tmp_path,
        verbose = FALSE
    )

    result <- data.table::fread(tmp_path)
    testthat::expect_true(is.na(result$PP[2]))
})

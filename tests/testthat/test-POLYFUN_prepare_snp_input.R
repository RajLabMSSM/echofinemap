test_that("POLYFUN_prepare_snp_input creates expected file", {

    dat <- echodata::BST1
    locus_dir <- file.path(tempdir(), "test_polyfun_snp_input")
    dir.create(locus_dir, showWarnings = FALSE, recursive = TRUE)
    PF.output.path <- file.path(locus_dir, "PolyFun")
    dir.create(PF.output.path, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    snp_path <- echofinemap:::POLYFUN_prepare_snp_input(
        PF.output.path = PF.output.path,
        locus_dir = locus_dir,
        dat = dat
    )

    testthat::expect_true(is.character(snp_path))
    testthat::expect_true(file.exists(snp_path))
    testthat::expect_true(grepl("snps_to_finemap", snp_path))

    ## Read back and verify columns
    snp_data <- data.table::fread(snp_path)
    testthat::expect_true(all(
        c("SNP", "CHR", "BP") %in% colnames(snp_data)
    ))
    testthat::expect_equal(nrow(snp_data), nrow(dat))
})

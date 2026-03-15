test_that("FINEMAP_construct_data creates data.z and data.ld files", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), "test_FINEMAP_construct")
    dir.create(file.path(locus_dir, "FINEMAP"),
               showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    out <- echoLD::subset_common_snps(LD_matrix = LD_matrix, dat = dat)
    LD_matrix <- out$LD
    dat <- out$DT

    res <- echofinemap:::FINEMAP_construct_data(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        verbose = FALSE
    )

    testthat::expect_true(is.list(res))
    testthat::expect_true(all(c("data.z", "data.z_path",
                                "data.ld", "data.ld_path") %in% names(res)))
    ## Files should exist on disk
    testthat::expect_true(file.exists(res$data.z_path))
    testthat::expect_true(file.exists(res$data.ld_path))
    ## data.z should have expected columns
    testthat::expect_true(all(
        c("rsid", "chromosome", "position", "allele1", "allele2",
          "maf", "beta", "se", "flip") %in% colnames(res$data.z)
    ))
    ## Dimensions should match
    testthat::expect_equal(nrow(res$data.z), nrow(res$data.ld))
    testthat::expect_equal(nrow(res$data.z), ncol(res$data.ld))
})

test_that("FINEMAP_construct_data fills missing optional columns", {

    ## Create minimal data without A1, A2, MAF
    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        CHR = c(4L, 4L, 4L),
        POS = c(100L, 200L, 300L),
        Effect = c(0.1, 0.2, 0.3),
        StdErr = c(0.01, 0.02, 0.03)
    )
    LD_matrix <- matrix(
        c(1, 0.5, 0.3, 0.5, 1, 0.4, 0.3, 0.4, 1),
        nrow = 3, dimnames = list(c("rs1", "rs2", "rs3"),
                                  c("rs1", "rs2", "rs3"))
    )
    locus_dir <- file.path(tempdir(), "test_FINEMAP_construct_fill")
    dir.create(file.path(locus_dir, "FINEMAP"),
               showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    res <- echofinemap:::FINEMAP_construct_data(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        verbose = FALSE
    )
    ## Default fills: A1="A", A2="T", MAF=0.1
    testthat::expect_true(all(res$data.z$allele1 == "A"))
    testthat::expect_true(all(res$data.z$allele2 == "T"))
    testthat::expect_true(all(res$data.z$maf == 0.1))
})

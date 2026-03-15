test_that("FINEMAP_construct_data creates z and ld files", {

    testthat::skip_if_not_installed("echodata")
    testthat::skip_if_not_installed("echoLD")

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix

    tmp_dir <- file.path(tempdir(), "test_fm_data_1")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    dir.create(file.path(tmp_dir, "FINEMAP"),
               recursive = TRUE, showWarnings = FALSE)

    sub_out <- echoLD::subset_common_snps(LD_matrix = LD_matrix,
                                          dat = dat,
                                          verbose = FALSE)

    result <- echofinemap:::FINEMAP_construct_data(
        dat = sub_out$DT,
        locus_dir = tmp_dir,
        LD_matrix = sub_out$LD,
        verbose = FALSE
    )
    ## Should return a list with expected components
    testthat::expect_type(result, "list")
    testthat::expect_true("data.z" %in% names(result))
    testthat::expect_true("data.ld" %in% names(result))
    testthat::expect_true("data.z_path" %in% names(result))
    testthat::expect_true("data.ld_path" %in% names(result))

    ## Files should exist
    testthat::expect_true(file.exists(result$data.z_path))
    testthat::expect_true(file.exists(result$data.ld_path))

    ## data.z should have required columns
    z <- result$data.z
    testthat::expect_true(all(c("rsid", "chromosome", "position",
                                "allele1", "allele2", "maf",
                                "beta", "se", "flip") %in% colnames(z)))
    ## No NAs in rsid
    testthat::expect_false(any(is.na(z$rsid)))
})

test_that("FINEMAP_construct_data fills missing optional columns", {

    testthat::skip_if_not_installed("echodata")
    testthat::skip_if_not_installed("echoLD")

    ## Create data without A1, A2, MAF
    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        CHR = c(4L, 4L),
        POS = c(100L, 200L),
        Effect = c(0.5, 0.3),
        StdErr = c(0.1, 0.2)
    )
    LD_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2,
                        dimnames = list(c("rs1", "rs2"), c("rs1", "rs2")))

    tmp_dir <- file.path(tempdir(), "test_fm_data_2")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    dir.create(file.path(tmp_dir, "FINEMAP"),
               recursive = TRUE, showWarnings = FALSE)

    result <- echofinemap:::FINEMAP_construct_data(
        dat = dat,
        locus_dir = tmp_dir,
        LD_matrix = LD_matrix,
        verbose = FALSE
    )
    z <- result$data.z
    ## Missing A1, A2, MAF should have been filled with defaults
    testthat::expect_true(all(z$allele1 == "A"))
    testthat::expect_true(all(z$allele2 == "T"))
    testthat::expect_true(all(z$maf == 0.1))
})

test_that("FINEMAP_construct_data handles sparse matrices", {

    testthat::skip_if_not_installed("Matrix")

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        CHR = c(4L, 4L),
        POS = c(100L, 200L),
        Effect = c(0.5, 0.3),
        StdErr = c(0.1, 0.2),
        A1 = c("A", "C"),
        A2 = c("G", "T"),
        MAF = c(0.1, 0.2)
    )
    ## Create a sparse LD matrix
    LD_sparse <- Matrix::Matrix(
        c(1, 0.5, 0.5, 1),
        nrow = 2, sparse = TRUE,
        dimnames = list(c("rs1", "rs2"), c("rs1", "rs2"))
    )

    tmp_dir <- file.path(tempdir(), "test_fm_data_sparse")
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    dir.create(file.path(tmp_dir, "FINEMAP"),
               recursive = TRUE, showWarnings = FALSE)

    result <- echofinemap:::FINEMAP_construct_data(
        dat = dat,
        locus_dir = tmp_dir,
        LD_matrix = LD_sparse,
        verbose = FALSE
    )
    ## Should write successfully (sparse converted to dense)
    testthat::expect_true(file.exists(result$data.ld_path))
    ## Returned LD should be a dense matrix
    testthat::expect_true(is.matrix(result$data.ld))
})

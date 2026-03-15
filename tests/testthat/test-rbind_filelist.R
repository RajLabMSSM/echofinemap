test_that("rbind_filelist reads and binds multiple files", {

    tmp_dir <- file.path(tempdir(), "test_rbind_filelist")
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    f1 <- file.path(tmp_dir, "file1.tsv")
    f2 <- file.path(tmp_dir, "file2.tsv")
    data.table::fwrite(
        data.table::data.table(SNP = c("rs1", "rs2"), val = c(1, 2)),
        f1, sep = "\t"
    )
    data.table::fwrite(
        data.table::data.table(SNP = c("rs3", "rs4"), val = c(3, 4)),
        f2, sep = "\t"
    )

    res <- echofinemap:::rbind_filelist(
        file.list = c(f1, f2),
        verbose = FALSE
    )
    testthat::expect_true(data.table::is.data.table(res))
    testthat::expect_equal(nrow(res), 4)
    testthat::expect_equal(res$SNP, c("rs1", "rs2", "rs3", "rs4"))
})

test_that("rbind_filelist uses fill=TRUE for mismatched columns", {

    tmp_dir <- file.path(tempdir(), "test_rbind_filelist2")
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    f1 <- file.path(tmp_dir, "file1.tsv")
    f2 <- file.path(tmp_dir, "file2.tsv")
    data.table::fwrite(
        data.table::data.table(SNP = "rs1", val = 1),
        f1, sep = "\t"
    )
    data.table::fwrite(
        data.table::data.table(SNP = "rs2", extra = "a"),
        f2, sep = "\t"
    )

    res <- echofinemap:::rbind_filelist(
        file.list = c(f1, f2),
        verbose = FALSE
    )
    testthat::expect_equal(nrow(res), 2)
    testthat::expect_true(all(c("SNP", "val", "extra") %in% colnames(res)))
})

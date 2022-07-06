test_that("SUSIE works", {
  
    set.seed(1234)
    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    
    run_tests <- function(dat, dat2, credset=5){
        testthat::expect_equal(nrow(LD_matrix), nrow(dat2))
        testthat::expect_gte(nrow(dat), 
                             nrow(dat2))
        testthat::expect_equal(sum(dat2$CS>0), credset)
        testthat::expect_equal(sum(dat2$PP>0.95), credset)
    }
    
    #### max_causal=1 #### 
    dat1 <- echofinemap::SUSIE(dat=dat, 
                               LD_matrix=LD_matrix,
                               max_causal = 1)
    run_tests(dat = dat, dat2 = dat1, credset = 1)
    #### max_causal=5 ####
    dat2 <- echofinemap::SUSIE(dat=dat, 
                               LD_matrix=LD_matrix,
                               max_causal = 5)
    run_tests(dat = dat, dat2 = dat2, credset = 5)
    #### max_causal=10 #### 
    dat3 <- echofinemap::SUSIE(dat=dat, 
                               LD_matrix=LD_matrix,
                               max_causal = 10)
    run_tests(dat = dat, dat2 = dat3, credset = 6)
    #### max_causal=20 #### 
    dat4 <- echofinemap::SUSIE(dat=dat, 
                               LD_matrix=LD_matrix,
                               max_causal = 20)
    run_tests(dat = dat, dat2 = dat4, credset = 14)
    
    #### window-size=1000 #### 
    dat5 <- echofinemap::SUSIE(dat=dat[seq_len(1000),], 
                               LD_matrix=LD_matrix,
                               max_causal = 5)
    run_tests(dat = dat, dat2 = dat5, credset = 5)
    
    #### prior_weights #### 
    dat$pw <- rep(seq_len(4),nrow(dat)/4) # Made up random weights
    dat6 <- echofinemap::SUSIE(dat=dat, 
                               LD_matrix=LD_matrix,
                               priors_col = "pw",
                               max_causal = 5)
    run_tests(dat = dat, dat2 = dat6, credset = 5)
})

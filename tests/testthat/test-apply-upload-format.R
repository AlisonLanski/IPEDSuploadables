test_that("apply_upload_format collapses data appropriately", {

  expect_equal(data.frame(apply_upload_format(data.frame(A=1,
                                              B=2,
                                              C='asdf'))),
               data.frame(allunited = "A=1,B=2,C=asdf"))

  expect_equal(data.frame(apply_upload_format(data.frame(A=c(1,15),
                                              B=c(2,25),
                                              C=c('asdf','xyz')))),
               data.frame(allunited = c("A=1,B=2,C=asdf",
                                            "A=15,B=25,C=xyz")))
})

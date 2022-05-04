test_that("get_ipeds_unitid returns a character, regardless of initial data type",{
  expect_type(get_ipeds_unitid(data.frame(UNITID = 123456)), 'character')
  expect_type(get_ipeds_unitid(data.frame(UNITID = '123456')), 'character')
})

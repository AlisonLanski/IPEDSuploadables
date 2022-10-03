####################################
#fake data for testing below
#data is correctly set up (no errors)

com_df <- data.frame(unitid = 123456,
                     majorcip = c(2.34, 12.34, 2.3400, 2, 12),
                     degreelevel = 17,
                     studentid = 900)

com_df2 <- data.frame(unitid = 123456,
                      majorcip = c("000000", "111111", "040000"),
                      degreelevel = 7,
                      studentid = 999)

ef1_df <- data.frame(unitid = 123456,
                     studentid = 44444)

hr_df <- data.frame(unitid = 123456,
                    empid = c(1:16),
                    gender = rep(1:2, 8),
                    raceethnicity = rep(1:4, 4),
                    occcategory3 = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 18, 20, 22, 24),
                    ftpt = c('P', rep('F', 15)),
                    months = c(99, rep(9, 15)),
                    currentemployee = 1)

om_df <- data.frame(unitid = 123456,
                    cohortstatus = c("Include", "Exclude"))


####
#######################################

### TESTS BEGIN

test_that("columns are sent to uppercase by prep functions", {
  expect_equal(sum(grepl(colnames(prep_com_data_frame(com_df)), pattern = "[[:lower:]]")), 0)
  expect_equal(sum(grepl(colnames(prep_ef1_data_frame(ef1_df)), pattern = "[[:lower:]]")), 0)
  expect_equal(sum(grepl(colnames(prep_hr_data_frame(hr_df)), pattern = "[[:lower:]]")), 0)
  expect_equal(sum(grepl(colnames(prep_om_data_frame(om_df)), pattern = "[[:lower:]]")), 0)
})


test_that("prep script type conversions are applied", {

  #COM
  expect_type(prep_com_data_frame(com_df)$UNITID, 'character')
  expect_type(prep_com_data_frame(com_df)$STUDENTID, 'character')
  expect_type(prep_com_data_frame(com_df)$DEGREELEVEL, 'character')
  expect_type(prep_com_data_frame(com_df)$MAJORCIP, 'character')

  #EF1
  expect_type(prep_ef1_data_frame(ef1_df)$UNITID, 'character')
  expect_type(prep_ef1_data_frame(ef1_df)$STUDENTID, 'character')

  #HR
  expect_type(prep_hr_data_frame(hr_df)$UNITID, 'character')
  expect_type(prep_hr_data_frame(hr_df)$EMPID, 'character')

})


test_that("Funky data used for recoding will throw error messages", {
  #COM cips

 #not 1/2/6digits, no period
  expect_error(prep_com_data_frame(data.frame(unitid = 123456,
                                              degreelevel = 5,
                                              student_id = 100,
                                              majorcip = 345)))

    #mixing types
    expect_error(prep_com_data_frame(data.frame(unitid = 123456,
                                 degreelevel = 5,
                                 student_id = 100,
                                 majorcip = c("34.", "34.01", "340000"))))
})

test_that("Funky data used for recoding will throw warning messages", {

  #HR gender
  expect_warning(prep_hr_data_frame(data.frame(unitid = 123456,
                                               empid = 'A',
                                               gender = 'M',
                                               raceethnicity = 1,
                                               occcategory3 = 1)), "Check Gender")

  #HR RE
  expect_warning(prep_hr_data_frame(data.frame(unitid = 123456,
                                               empid = 'A',
                                               gender = 1,
                                               raceethnicity = 12,
                                               occcategory3 = 1)), "Check RaceEthnicity")
  #HR OccCat3
  expect_warning(prep_hr_data_frame(data.frame(unitid = 123456,
                                               empid = 'A',
                                               gender = 1,
                                               raceethnicity = 1,
                                               occcategory3 = 21)), "Check OccCategory3")

  #HR Months
  expect_warning(prep_hr_data_frame(data.frame(unitid = 123456,
                                               empid = 'A',
                                               gender = 1,
                                               raceethnicity = 1,
                                               occcategory3 = 1,
                                               currentemployee = 1,
                                               ftpt = 'F',
                                               months = 4)), "Check Months")

  #OM Exclusions
  expect_warning(prep_om_data_frame(data.frame(unitid = 123456,
                                               studentid = c('AAA', 'BBB', 'CCC'),
                                               cohortstatus = c("Amy", "Bob", "Include"))), "Check CohortStatus")
})


# #saved file doesn't exist because I don't want to make a new list/data object
# #also, this prep test can be ultimately tested by looking at the final output (at this point)
# #can add specific tests for rows/columns if we need to update something down the line
# test_that("HR prep calculations come out as expected", {
#    expect_equal(prep_hr_data_frame(hr_df), saved_hr_prep)
# })

test_that("OM prep recoding comes out as expected", {
  expect_equal(prep_om_data_frame(om_df)$EXCLUSION, c(FALSE, TRUE))
})


test_that("prep for COM handles CIPs appropriately", {
  expect_equal(toString(prep_com_data_frame(com_df)$MAJORCIP), "02.3400, 12.3400, 02.3400, 02.0000, 12.0000")
  expect_equal(toString(prep_com_data_frame(com_df2)$MAJORCIP), "00.0000, 11.1111, 04.0000")
})


########
### COM

test_that("COM parts produce expected dfs", {
  #prep data
  datcom <- prep_com_data_frame(com_students)
  datcip <- prep_com_data_frame(com_cips)

  #tests
  expect_equal(make_com_part_A(datcom, datcip), part_outputs$com_partA)
  expect_equal(make_com_part_B(datcom, datcip), part_outputs$com_partB)
  expect_equal(make_com_part_C(datcom), part_outputs$com_partC)
  expect_equal(make_com_part_D(datcom, datcip), part_outputs$com_partD)
  #expect_equal(make_com_part_E(datcom, ugender = TRUE, ggender = TRUE), part_outputs$com_partE)
  expect_equal(make_com_part_E(datcom), part_outputs$com_partE)
})



########
### E1D

#adding for new gender testing starting in 2023
e1d_partD_df <- data.frame(UNITID = 999999,
                           STUDENTID = c(1:15),
                           STUDENTLEVEL = c(rep('Undergraduate', 6), rep('Graduate', 9)),
                           GENDERDETAIL = c(rep(4, 10), rep(3, 5)))

e1d_partD_df_noGrad <- e1d_partD_df[e1d_partD_df$STUDENTLEVEL == 'Undergraduate',]

# e1d_partD_TrueTrue <- data.frame(UNITID = 999999,
#                                  SURVSECT = "E1D",
#                                  PART = "D",
#                                  FYGU01 = 1,
#                                  FYGU011 = 0,
#                                  FYGU012 = 6,
#                                  FYGU02 = 3,
#                                  FYGU021 = 5,
#                                  FYGU022 = -2)


## adding new hs credit testing for 2024-2025
e1d_df_noHS <- data.frame(UNITID = c("111", "111", "111"),
                 STUDENTID = c(20, 21, 22),
                 ISHIGHSCHOOL = c(0, 0, 0),
                 ISDUAL = c(0, 0, 0))

e1d_partF_noHS <- data.frame(UNITID = "111",
                             SURVSECT = "E1D",
                             PART = "F",
                             ENROLLHS = 2,
                             ENROLLHS_WITHIN = -2,
                             ENROLLHS_OUTSIDE = -2)

e1d_df_HSnotDual <- e1d_df_noHS
e1d_df_HSnotDual[, 3] <- 1
e1d_partF_HSnotDual <- data.frame(UNITID = "111",
                             SURVSECT = "E1D",
                             PART = "F",
                             ENROLLHS = 1,
                             ENROLLHS_WITHIN = 2,
                             ENROLLHS_OUTSIDE = 1)

test_that("E1D parts produce expected dfs", {

  #tests
  expect_equal(make_e1d_part_A(e1d_students), part_outputs$e1d_partA)
  expect_equal(make_e1d_part_B(e1d_instr), part_outputs$e1d_partB)
  expect_equal(make_e1d_part_C(e1d_students), part_outputs$e1d_partC)
  #updated part
  expect_equal(make_e1d_part_D(e1d_students), part_outputs$e1d_partD)
  expect_equal(make_e1d_part_D(e1d_partD_df_noGrad)$FYSEXG, 0)
  #  expect_equal(make_e1d_part_D(e1d_students, ugender = TRUE, ggender = TRUE), part_outputs$e1d_partD)
  #  expect_equal(make_e1d_part_D(e1d_partD_df, ugender = TRUE, ggender = TRUE), e1d_partD_TrueTrue)
  expect_equal(make_e1d_part_E(e1d_students), part_outputs$e1d_partE)
  expect_equal(make_e1d_part_F(e1d_students), part_outputs$e1d_partF)
  expect_equal(make_e1d_part_F(e1d_df_noHS), e1d_partF_noHS)
  expect_equal(make_e1d_part_F(e1d_df_HSnotDual), e1d_partF_HSnotDual)
})

########
### EF1

# for testing UNK gender (and duplicate elimination)
ef1_df_unk <- data.frame(UNITID = '999999',
                         STUDENTID = c('A', 'B', 'C', 'D', 'D'),
                         STUDENTLEVEL = c('Undergraduate',
                                          'Undergraduate',
                                          'Graduate',
                                          'Graduate',
                                          'Graduate'),
                         GENDERDETAIL = c(1, 3,   3,   4,   4))

ef1_unk <- dplyr::tibble(UNITID = "999999",
                      SURVSECT = "EF1",
                      PART = 'H',
                      EFSEXUG = 1,
                      EFSEXG = 2)

test_that("EF1 parts produce expected dfs", {
  #prep data
  datef1 <- prep_ef1_data_frame(ef1_students)
  datret <- prep_ef1_data_frame(ef1_retention)

  #tests
  expect_equal(make_ef1_part_A(datef1, cips = TRUE), part_outputs$ef1_partA_TRUE)
  expect_equal(make_ef1_part_A(datef1, cips = FALSE), part_outputs$ef1_partA_FALSE)
  expect_equal(make_ef1_part_B(datef1), part_outputs$ef1_partB)
  expect_equal(make_ef1_part_C(datef1), part_outputs$ef1_partC)
  expect_equal(make_ef1_part_D(datef1), part_outputs$ef1_partD)
  expect_equal(make_ef1_part_E(datret), part_outputs$ef1_partE)
  #can't run F -- s/f ratio collected via popup
  #expect_equal(make_ef1_part_F(datef1), part_outputs$ef1_partF)
  expect_equal(make_ef1_part_G(datef1), part_outputs$ef1_partG)
#  expect_equal(make_ef1_part_H(datef1, ugender = T, ggender = T), part_outputs$ef1_partH)
  expect_equal(make_ef1_part_H(datef1), part_outputs$ef1_partH)
  expect_equal(make_ef1_part_H(ef1_df_unk), ef1_unk)
})

########
### GR200

test_that("GR200 parts produce expected dfs", {
  #tests
  expect_equal(make_gr200(gr200_students), part_outputs$gr200)
  #expect_snapshot_value(make_gr200(gr200_students), variant = "gr200")
})

########
### GR

test_that("GR parts produce expected dfs", {
  #tests
  expect_equal(make_gr_part_B(gr_students), part_outputs$gr_partB)
  expect_equal(make_gr_part_C(gr_students), part_outputs$gr_partC)
})

########
### HR

test_that("HR parts produce expected dfs", {
  dathr <- prep_hr_data_frame(hr_staff)

  #tests
  expect_equal(make_hr_part_A1(dathr), part_outputs$hr_partA1)
  expect_equal(make_hr_part_A2(dathr), part_outputs$hr_partA2)
  expect_equal(make_hr_part_B1(dathr), part_outputs$hr_partB1)
  expect_equal(make_hr_part_B2(dathr), part_outputs$hr_partB2)
  expect_equal(make_hr_part_B3(dathr), part_outputs$hr_partB3)
  expect_equal(make_hr_part_D1(dathr), part_outputs$hr_partD1)
  expect_equal(make_hr_part_D2(dathr), part_outputs$hr_partD2)
  expect_equal(make_hr_part_D3(dathr), part_outputs$hr_partD3)
  expect_equal(make_hr_part_D4(dathr), part_outputs$hr_partD4)
  expect_equal(make_hr_part_G1(dathr), part_outputs$hr_partG1)
  expect_equal(make_hr_part_G2(dathr), part_outputs$hr_partG2)
  expect_equal(make_hr_part_H1(dathr), part_outputs$hr_partH1)
  expect_equal(make_hr_part_H2(dathr), part_outputs$hr_partH2)
})


########
### OM

test_that("OM parts produce expected dfs", {
  datom <- prep_om_data_frame(om_students)

  #tests
  expect_equal(make_om_part_A(datom), part_outputs$om_partA)
  expect_equal(make_om_part_B(datom), part_outputs$om_partB)
  expect_equal(make_om_part_C(datom), part_outputs$om_partC)
  expect_equal(make_om_part_D(datom), part_outputs$om_partD)
})

# ##Code to set up the list (name on the left, replace the right side with a rerun of the relevant script)
# ##Note that you will need to run prep scripts before the make functions for COM, HR, EF1 and OM
# ## something like hr_partA1 = make_hr_part_A1(prep_hr_data_frame(hr_staff))
# ## not adding all of that now because I'm lazy :}
#
# part_outputs <- list(
#   com_partA = make_com_part_A(prep_com_data_frame(com_students),
#                               prep_com_data_frame(com_cips)),
#   com_partB = make_com_part_B(prep_com_data_frame(com_students),
#                               prep_com_data_frame(com_cips)),
#   com_partC = make_com_part_C(prep_com_data_frame(com_students)),
#   com_partD = make_com_part_D(prep_com_data_frame(com_students),
#                               prep_com_data_frame(com_cips)),
#   com_partE = make_com_part_E(prep_com_data_frame(com_students)),
#   e1d_partA = e1d_partA,
#   e1d_partB = e1d_partB,
#   e1d_partC = e1d_partC,
#   e1d_partD = make_e1d_part_D(e1d_students, TRUE, TRUE),
#   e1d_partE = make_e1d_part_E(e1d_students),
#   e1d_partF = make_e1d_part_F(e1d_students),
#   ef1_partA_FALSE = `ef1_partA-FALSE`,
#   ef1_partA_TRUE = `ef1_partA-TRUE`,
#   ef1_partB = ef1_partB,
#   ef1_partC = ef1_partC,
#   ef1_partD = ef1_partD,
#   ef1_partE = ef1_partE,
#   #no F: F is st/fac ratio
#   ef1_partG = ef1_partG,
#   gr_partB = gr_partB,
#   gr_partC = gr_partC,
#   gr200 = gr200,
#   hr_partA1 = hr_partA1,
#   hr_partA2 = hr_partA2,
#   hr_partB1 = hr_partB1,
#   hr_partB2 = hr_partB2,
#   hr_partB3 = hr_partB3,
#   hr_partD1 = hr_partD1,
#   hr_partD2 = hr_partD2,
#   hr_partD3 = hr_partD3,
#   hr_partD4 = hr_partD4,
#   hr_partG1 = hr_partG1,
#   hr_partG2 = hr_partG2,
#   hr_partH1 = hr_partH1,
#   hr_partH2 = hr_partH2,
#   om_partA = om_partA,
#   om_partB = om_partB,
#   om_partC = om_partC,
#   om_partD = om_partD
# )
#
# use_data(part_outputs, overwrite = TRUE)


# ## Keeping an example of the exepect_equal_to_reference method
# ## might be better to replace with expect_snapshot, but currently too hard to figure out
# ########
# ### GR200
#
# test_that("GR200 parts produce expected dfs", {
#   #tests
#   expect_equal_to_reference(make_gr200(gr200_students), file = "gr200.rds")
#
# })
#

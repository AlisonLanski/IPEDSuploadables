### Reminder bits of code when the package needs serious structural updates

## ALSO: the scripts to check
# Dummy data update?  also save the data and update the documentation for the data
# After dummy data update: fix the vignette requirements or any instructions; knit to check results
# Update make scripts or create new make scripts
# Update produce scripts to include any changes for the make scripts
# Produce correct parts, save to part_outputs
# Rerun test scripts and/or add additional test scripts
# Test additional data configurations for any control flow/filtering etc
# Final package check
# Final documentation updates including zzz.R file
# After merge, make CRAN submission/change log updates; update pkgdown etc (see other txt file)



### Rewrite dummy data and stored part outputs

####################
##Completions
######

# #Data
# com_students <- create_dummy_data_com("student")
# use_data(com_students, overwrite = T)
#
# com_cips <- create_dummy_data_com("cip")
# use_data(com_cips, overwrite = T)
#
# #Vignettes: see vignette scripts
#
# #Output
# part_outputs$com_partA <- make_com_part_A(prep_com_data_frame(com_students),
#                                           prep_com_data_frame(com_cips))
# part_outputs$com_partB <- make_com_part_B(prep_com_data_frame(com_students),
#                                           prep_com_data_frame(com_cips))
# part_outputs$com_partC <- make_com_part_C(prep_com_data_frame(com_students))
# part_outputs$com_partD <- make_com_part_D(prep_com_data_frame(com_students),
#                                           prep_com_data_frame(com_cips))
# part_outputs$com_partE <- make_com_part_E(prep_com_data_frame(com_students))
# use_data(part_outputs, overwrite = T)

####################
##12 month enrollment
######

# #Data
# set.seed(1892)
# e1d_students <- create_dummy_data_e1d("student")
# use_data(e1d_students, overwrite = T)
#
# set.seed(1892)
# e1d_instr <- create_dummy_data_e1d("instr")
# use_data(e1d_instr, overwrite = T)
#
# #Vignettes: see vignette scripts
#
# #Output
# part_outputs$e1d_partA <- make_e1d_part_A(e1d_students)
# part_outputs$e1d_partB <- make_e1d_part_B(e1d_instr)
# part_outputs$e1d_partC <- make_e1d_part_C(e1d_students)
# part_outputs$e1d_partD <- make_e1d_part_D(e1d_students, TRUE, TRUE)
# part_outputs$e1d_partE <- make_e1d_part_E(e1d_students)
# use_data(part_outputs, overwrite = T)

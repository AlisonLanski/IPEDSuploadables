# Outcome measures part A
# Establishing cohorts
#

#Is all of this related to grad rates?
#end up with wide data (not long)
#ask for wide data
#maybe later do a prep script that will pivot long to wide and fix stuff

#' Make Outcome Measures Part A
#'
#' @description Establishing the Outcome Measures cohorts
#'
#' @param df A dataframe of student statuses
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @return A text file ready for IPEDS upload
#' @export
#'
#' @examples
#'
make_om_part_A <- function(df, output = "part", format = "both") {

  partA <- df %>%

    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$CohortType, .data$Recipient)%>%
    dplyr::summarize(CohortCount = dplyr::n(),
                     ExclusionCount = sum(.data$Exclusion)) %>%
    dplyr::ungroup() %>%

    #sort for easy viewing
    dplyr::arrange(.data$CohortType, .data$Recipient) %>%

    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=OM1",
                     PART = "PART=A",
                     LINE = paste0("LINE=", .data$CohortType),
                     RECIPIENT_TYPE = paste0("RECIPIENT_TYPE=", .data$Recipient),
                     COHORT = paste0("COHORT=", .data$CohortCount),
                     EXCLUSION = paste0("EXCLUSION=", .data$ExclusionCount)
    )

  #create the txt file
  write_report(df = partA,
               component = "OutcomeMeasures",
               part = "PartA",
               output = output,
               format = format)

}

# make_om_part_A(om_dat)
#
#
# UNITID
# SURVSECT 'OM1'
# PART #code
# LINE #code
# RECIPIENT_TYPE #code
# COHORT #count
# EXCLUSION #count
# #(ADJ_Cohort is auto calced)
#

# #Award status at 4 years after entry
# #part B
# UNITID
# SURVSECT
# PART
# TIME
# RECIPIENT_TYPE
# AWARD_CERTIFICATES #COUNT
# AWARD_ASSOCIATES #COUNT
# AWARD_BACHELORS #COUNT
#
#
# UNITID=nnnnnn,SURVSECT=OM1,PART=A,LINE=4,RECIPIENT_TYPE=1,COHORT=500,EXCLUSION=10
# UNITID=nnnnnn,SURVSECT=OM1,PART=A,LINE=4,RECIPIENT_TYPE=2,COHORT=700,EXCLUSION=7
# UNITID=nnnnnn,SURVSECT=OM1,PART=B,LINE=1,RECIPIENT_TYPE=1,AWARD_CERTIFICATES=400,
# AWARD_ASSOCIATES=450,AWARD_BACHELORS=550
# UNITID=nnnnnn,SURVSECT=OM1,PART=B,LINE=1,RECIPIENT_TYPE=2,AWARD_CERTIFICATES=600,
# AWARD_ASSOCIATES=700,AWARD_BACHELORS=1150
#
#
#
# #Award status at 6 years after entry
# #part C
# UNITID
# SURVSECT
# PART
# TIME
# RECIPIENT_TYPE
# AWARD_CERTIFICATES #COUNT
# AWARD_ASSOCIATES #COUNT
# AWARD_BACHELORS #COUNT
#
#
# UNITID=nnnnnn,SURVSECT=OM1,PART=C,LINE=1,RECIPIENT_TYPE=1,AWARD_CERTIFICATES=400,
# AWARD_ASSOCIATES=450,AWARD_BACHELORS=550
# UNITID=nnnnnn,SURVSECT=OM1,PART=C,LINE=1,RECIPIENT_TYPE=2,AWARD_CERTIFICATES=600,
# AWARD_ASSOCIATES=700,AWARD_BACHELORS=1150
#
# UNITID=nnnnnn,SURVSECT=OM1,PART=D,LINE=1,RECIPIENT_TYPE=1,AWARD_CERTIFICATES=450,
# AWARD_ASSOCIATES=450,AWARD_BACHELORS=600,STILL_ENROLLED=25,ENROLLED_ANOTHER=20
# UNITID=nnnnnn,SURVSECT=OM1,PART=D,LINE=1,RECIPIENT_TYPE=2,AWARD_CERTIFICATES=650,
# AWARD_ASSOCIATES=750,AWARD_BACHELORS=1200,STILL_ENROLLED=50,ENROLLED_ANOTHER=40
#
#
# UNITID=nnnnnn,SURVSECT=OM1,PART=A,LINE=n,RECIPIENT_TYPE=n,COHORT=nnnnnn,EXCLUSION=nnnnnn
# UNITID=nnnnnn,SURVSECT=OM1,PART=B,LINE=n,RECIPIENT_TYPE=n,AWARD_CERTIFICATES=nnnnnn,
# AWARD_ASSOCIATES=nnnnnn,AWARD_BACHELORS=nnnnnn
# UNITID=nnnnnn,SURVSECT=OM1,PART=C,LINE=n,RECIPIENT_TYPE=n,AWARD_CERTIFICATES=nnnnnn,
# AWARD_ASSOCIATES=nnnnnn,AWARD_BACHELORS=nnnnnn
# UNITID=nnnnnn,SURVSECT=OM1,PART=D,LINE=n,RECIPIENT_TYPE=n,AWARD_CERTIFICATES=nnnnnn,
# AWARD_ASSOCIATES=nnnnnn,AWARD_BACHELORS=nnnnnn,STILL_ENROLLED=nnnnnn,
# ENROLLED_ANOTHER=nnnnnn
#
# #Award status at 8 years after entry
# #part d
# UNITID
# SURVSECT
# PART
# TIME
# RECIPIENT_TYPE
# AWARD_CERTIFICATES #COUNT
# AWARD_ASSOCIATES #COUNT
# AWARD_BACHELORS #COUNT
# STILL_ENROLLED #COUNT
# ENROLLED_ANOTHER #COUNT

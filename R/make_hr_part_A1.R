##
#' Produce IPEDS HR Part A1
#'
#' @description  Part A1 --- Count of FT Instructional staff by tenure status, academic rank, and race/ethnicity/gender
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select group_by ungroup arrange transmute
#'
#'
make_hr_part_A1 <- function(df, output = "part") {

  ipeds_unitid <- set_ipeds_unitid()

  #set up the grid of options
  combos_A1 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           Rank = c(1:6),
                           REG = c(1:18),
                           Count = 0) %>%
               dplyr::bind_rows(expand.grid(Unitid = ipeds_unitid,
                                     Tenure = 6,
                                     Rank = 7,
                                     REG = 1:18,
                                     Count = 0)
                         )

  #produce the uploadable format
  partA1 <- df %>%
            dplyr::filter(df$CurrentEmployee == 1,
                   df$Instructional == 1,
                   df$FtPt == 'F') %>%
            dplyr::select(df$Unitid,
                          df$Tenure,
                          df$Rank,
                          df$REG,
                          df$Count) %>%
            #add extra combinations
            dplyr::bind_rows(combos_A1) %>%
            #aggregate the full data
            dplyr::group_by(df$Unitid,
                            df$Tenure,
                            df$Rank,
                            df$REG) %>%
            dplyr::summarize(Count = sum(df$Count)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(df$Tenure, df$Rank, df$REG) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", df$Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=A1",
                      TENURE = paste0("TENURE=", df$Tenure),
                      RANK = paste0("RANK=", df$Rank),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", df$REG),
                      COUNT = paste0("COUNT=", df$Count))

      write_report(df = partA1,
                   component = 'HumanResources',
                   part = "PartA1",
                   output = output,
                   append = FALSE)



  write_report <- function(df, component, part, output, append) {

    if(tolower(output) == 'part' | output == 'both') {
        write.table(x = df, sep = ",",
                file = paste0(path, component, "_", part, "_", Sys.Date(), ".txt"),
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    if(tolower(output) == 'full' | output == 'both'){
      write.table(x = df, sep = ",",
                  file = paste0(path, component, "_AllParts_", Sys.Date(), ".txt"),
                  quote = FALSE, row.names = FALSE, col.names = FALSE, append = append)
    }
  }





}

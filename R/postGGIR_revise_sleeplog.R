#' Title
#'
#' @param outputfolder
#' @param criterror
#' @param outputfile
#' @param outputplotname
#'
#' @return
#' @export
#'
#' @examples
postGGIR_revise_sleeplog = function(outputfolder, criterror = 1, outputfile, outputplotname) {
  # load full nightsummary report
  p4_path = grep("part4_nightsummary_sleep_full", dir(outputfolder, recursive = T, full.names = T), value = T)
  NS = data.table::fread(p4_path, data.table = FALSE)

  # Identify nights with high criterrors
  error_onset = which(abs(NS$error_onset) >= criterror)
  error_wake = which(abs(NS$error_wake) >= criterror)
  error = sort(unique(c(error_onset, error_wake)))

  # subset nightsummary
  guider_onset = c("guider_onset_ts","guider_inbedStart_ts")[c("guider_onset_ts","guider_inbedStart_ts") %in% colnames(NS)]
  guider_wake = c("guider_wakeup_ts","guider_inbedEnd_ts")[c("guider_wakeup_ts","guider_inbedEnd_ts") %in% colnames(NS)]
  NSconflicts = NS[error, c("ID", "filename","calendar_date", "night",
                   "cleaningcode", "fraction_night_invalid",
                   "sleeponset_ts", guider_onset,
                   "wakeup_ts", guider_wake)]

  # Create visualization of conflicting nights
  visualize_conflicting_nights(outputfolder, NSconflicts,
                               outputplotname = outputplotname)

  # save to excel file
  openxlsx::write.xlsx(x = NSconflicts, file = file.path(dirname(outputfolder),outputfile),
                       asTable = T, overwrite = TRUE,
                       creator = "Jairo Hidalgo Migueles", sheetName = "Nights to revise",
                       firstRow = TRUE, firstCol = TRUE, colWidths = "auto", na.string = " ")

  # output data frame
  return(NS)
}

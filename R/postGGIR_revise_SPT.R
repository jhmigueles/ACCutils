#' Title
#'
#' @param outputfolder
#' @param criterror
#' @param outputfile
#' @param outputplotname
#' @param desiredtz
#' @param SPTlowerLimit
#' @param SPTupperLimit
#'
#' @return
#' @export
#'
#' @examples
postGGIR_revise_SPT = function(outputfolder, desiredtz, criterror = 4,
                               SPTlowerLimit = 4, SPTupperLimit = 10,
                               excludefirstlast = FALSE,
                               outputfile, outputplotname) {
  # load full nightsummary report
  p4path = grep("part4_nightsummary_sleep_full",
                dir(outputfolder, recursive = T, full.names = T), value = T)
  NS = data.table::fread(p4path, data.table = FALSE)

  # Identify nights below and above limits
  nights2rev = which(NS$SptDuration <= SPTlowerLimit | NS$SptDuration >= SPTupperLimit)

  # Identify nights with high criterrors
  error_onset = which(abs(NS$error_onset) >= criterror)
  error_wake = which(abs(NS$error_wake) >= criterror)
  nights2rev = sort(unique(c(nights2rev, error_onset, error_wake)))

  # guiders for those nights
  NSconflicts = NS[nights2rev, ]

  # exclude first last?
  if (excludefirstlast) {
    exclude = c()
    for (i in 1:nrow(NSconflicts)) {
      is_first_last = NSconflicts$night[i] %in% c(range(NS$night[which(NS$ID == NSconflicts$ID[i])]))
      if (is_first_last) exclude = c(exclude,i)
    }
    NSconflicts = NSconflicts[-exclude,]
  }

  if (nrow(NSconflicts) > 0) {

    # identify auto and guider onset and wakeups
    tsVars = grep("_ts", colnames(NSconflicts), value = T)
    onset_0 = tsVars[1]; wake_0 = tsVars[2]
    onset_1 = tsVars[3]; wake_1 = tsVars[4]

    # visualize nights
    visualize_conflicting_nights(outputfolder = outputfolder,
                                 NS = NS,
                                 NSconflicts = NSconflicts,
                                 desiredtz = desiredtz,
                                 onset_0 = onset_0, onset_1 = onset_1,
                                 wake_0 = wake_0, wake_1 = wake_1,
                                 outputplotname = outputplotname)

    # save to excel file
    openxlsx::write.xlsx(x = NSconflicts, file = file.path(dirname(outputfolder),outputfile),
                         asTable = T, overwrite = TRUE,
                         creator = "Jairo Hidalgo Migueles", sheetName = "Nights to revise",
                         firstRow = TRUE, firstCol = TRUE, colWidths = "auto", na.string = " ")
  } else {
    cat("\nNo short, long, or incoherent nights according to sleeplog")
  }

}

#' Title
#'
#' @param outputfolder
#' @param criterror
#' @param outputfile
#' @param outputplotname
#' @param desiredtz
#' @param SPTlowerLimit
#' @param SPTupperLimit
#' @param excludefirstlast
#' @param method
#'
#' @return
#' @export
#'
#' @examples
postGGIR_revise_SPT = function(outputfolder, desiredtz, criterror = 4,
                               SPTlowerLimit = 4, SPTupperLimit = 10,
                               excludefirstlast = FALSE,
                               method = c("all", "last")[1],
                               outputfile, outputplotname) {
  # load full nightsummary report
  p4path = grep("part4_nightsummary_sleep_full",
                dir(outputfolder, recursive = T, full.names = T), value = T)
  NS = data.table::fread(p4path, data.table = FALSE)

  if (method == "all") {
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
  } else if (method == "last") {
    revise = c()
    for (i in 1:nrow(NSconflicts)) {
      is_last = NSconflicts$night[i] %in% max(NS$night[which(NS$ID == NSconflicts$ID[i])])
      if (is_last) revise = c(revise,i)
    }
    NSconflicts = NSconflicts[revise,]
  }

  # revise nights
  if (nrow(NSconflicts) > 0) {

    # identify auto and guider onset and wakeups
    tsVars = grep("_ts", colnames(NSconflicts), value = T)
    onset_0 = tsVars[1]; wake_0 = tsVars[2]
    onset_1 = tsVars[3]; wake_1 = tsVars[4]

    # visualize nights
    if (outputplotname == "" | length(outputplotname) == 0) outputplotname = "nights2revise.pdf"
    if (tools::file_ext(outputplotname) != "pdf") {
      ext = tools::file_ext(outputplotname)
      if (ext == "") outputplotname = paste0(outputplotname, ".pdf") else outputplotname = gsub(ext, "pdf", outputfile)
    }
    visualize_conflicting_nights(outputfolder = outputfolder,
                                 NS = NS,
                                 NSconflicts = NSconflicts,
                                 desiredtz = desiredtz,
                                 onset_0 = onset_0, onset_1 = onset_1,
                                 wake_0 = wake_0, wake_1 = wake_1,
                                 outputplotname = outputplotname)

    # data cleaning file template
    data_cleaning_file = NSconflicts[, c("ID", "night", "night", "night")]
    colnames(data_cleaning_file) = c("ID", "day_part5", "relyonguider_part4", "night_part4")
    data_cleaning_file$day_part5 = NA

    # save data cleaning file
    if (outputfile == "" | length(outputfile) == 0) outputfile = "data_cleaning_part4.csv"
    if (tools::file_ext(outputfile) != "csv") {
      ext = tools::file_ext(outputfile)
      if (ext == "") outputfile = paste0(outputfile, ".csv") else outputfile = gsub(ext, "csv", outputfile)
    }
    write.csv(x = data_cleaning_file, file = file.path(dirname(outputfolder),outputfile),
              row.names = FALSE, na = "")
  } else {
    cat("\nNo short, long, or incoherent nights according to sleeplog")
  }

}

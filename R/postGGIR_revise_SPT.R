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

  # identify auto and guider onset and wakeups
  tsVars = grep("_ts", colnames(NS), value = T)
  onset_0 = tsVars[1]; wake_0 = tsVars[2]
  onset_1 = tsVars[3]; wake_1 = tsVars[4]

  if (method == "all") {
    # Identify nights below and above limits
    nights2rev = which(NS$SptDuration <= SPTlowerLimit | NS$SptDuration >= SPTupperLimit)

    IDs = unique(NS$ID)
    for (i in 1:length(IDs)) {
      expected_nights = 1:(max(NS$night[which(NS$ID == IDs[i])]))
      available = NS$night[which(NS$ID == IDs[i])]
      missing = which(!(expected_nights %in% available))
      if (length(missing) > 0) {
        for (missing_i in 1:length(missing)) {
          id = IDs[i]
          filename = unique(NS$filename[which(NS$ID == id)])
          night = missing[missing_i]
          if (night == 1) {
            NSnext_row = which(NS$ID == id)[night] + 1
            calendar_date = trunc(strptime(NS$calendar_date[NSnext_row], format = "%d/%m/%Y") - 22*60*60, "days")
          } else {
            if (missing_i == 1) NSprev_row = which(NS$ID == id)[night] - 1
            calendar_date = trunc(strptime(NS$calendar_date[NSprev_row], format = "%d/%m/%Y") + 26*60*60, "days")
          }
          calendar_missing = GGIR::POSIXtime2iso8601(calendar_date, tz = desiredtz)
          calendar_missing = substr(calendar_missing, 1, 10)
          y_m_d = as.numeric(unlist(strsplit(calendar_missing, "-", fixed = T)))
          calendar_missing_format = paste(y_m_d[3], y_m_d[2], y_m_d[1], sep = "/")
          onset_auto = "22:00:00"; wake_auto = "04:00:00"
          onset_guider = "22:00:00"; wake_guider = "04:00:00"

          NS[nrow(NS) + 1,] = NA
          NS[nrow(NS), c("ID", "filename")] = c(id, filename)
          NS[nrow(NS), c("night")] = night
          NS[nrow(NS), c("calendar_date")] = calendar_missing_format
          NS[nrow(NS), c(onset_0, wake_0, onset_1, wake_1)] = c("22:00:00", "04:00:00")
          NS[nrow(NS), c("guider")] = "madeup"
          nights2rev = c(nights2rev, nrow(NS))

          # redefine prev calendar date for next iteration
          NSprev_row = nrow(NS)
        }
      }
    }

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

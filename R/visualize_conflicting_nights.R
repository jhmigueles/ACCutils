#' Title
#'
#' @param NSconflicts
#' @param outputfolder
#' @param desiredtz
#' @param outputplotname
#'
#' @return
#' @export
#'
#' @examples
visualize_conflicting_nights = function(outputfolder, NSconflicts, desiredtz = "Europe/Madrid",outputplotname) {
  # create pdf
  pdf(file.path(dirname(outputfolder),outputplotname), paper = "a4",
      height = 24, width = 18)
  par(las = 1, mar = c(3, 4, 3, 1), mfrow = c(4, 1))
  for (i in 1:nrow(NSconflicts)) {
    # load IMP data from ms2
    ms2path = file.path(outputfolder, "meta/ms2.out/", NSconflicts$filename[i])
    load(ms2path)
    ms = IMP$metashort

    # identify starting and endpoint
    guider_onset = c("guider_onset_ts","guider_inbedStart_ts")[c("guider_onset_ts","guider_inbedStart_ts") %in% colnames(NSconflicts)]
    guider_wake = c("guider_wakeup_ts","guider_inbedEnd_ts")[c("guider_wakeup_ts","guider_inbedEnd_ts") %in% colnames(NSconflicts)]

    fromTime = c(NSconflicts[i, c("sleeponset_ts")], NSconflicts[i, guider_onset])
    fromDate = as.POSIXct(NSconflicts$calendar_date[i], tz = desiredtz, format = "%d/%m/%Y")
    fromDate = as.Date(fromDate, tz = desiredtz) + 1 # to match day classification in visual reports
    from_tmp = as.POSIXct(paste(fromDate, fromTime))

    toTime = c(NSconflicts[i, c("wakeup_ts")], NSconflicts[i, guider_wake])
    toDate = as.POSIXct(NSconflicts$calendar_date[i], tz = desiredtz, format = "%d/%m/%Y")
    toDate = as.Date(toDate, tz = desiredtz) + 1 # to match day classification in visual reports
    to_tmp = as.POSIXct(paste(toDate, toTime))

    # substract 24 hours if slept before midnight
    prevDay_onset_acc = prevDay_onset_log = FALSE
    if (from_tmp[1] >= to_tmp[1]) {
      prevDay_onset_acc = TRUE
      from_tmp[1] = from_tmp[1] - 1440*60
    }
    if (from_tmp[2] >= to_tmp[2]) {
      prevDay_onset_log = TRUE
      from_tmp[2] = from_tmp[2] - 1440*60
    }

    # identify in dataset
    from_tmp2 = from_tmp - (2*60*60)
    from_iso = GGIR::POSIXtime2iso8601(min(from_tmp2), tz = desiredtz)
    if (from_iso %in% ms$timestamp) from = which(ms$timestamp == from_iso) else from = 1

    to_tmp2 = max(to_tmp) + (2*60*60)
    to_iso = GGIR::POSIXtime2iso8601(to_tmp2, tz = desiredtz)
    if (to_iso %in% ms$timestamp) to = which(ms$timestamp == to_iso) else to = nrow(ms)

    # subset ms
    ms = ms[from:to,]

    # plot angle and acceleration
    ylim = max(abs(range(ms$anglez))) + 20
    main = paste(paste0("ID: ", NSconflicts$ID[i]),
                 paste0("Wake-up date: ", fromDate), sep = "; ")
    plot(ms$anglez, type = "l", lwd = 2, ylim = c(-ylim, ylim),
         xlab = "", ylab = "Z-axis angle (º)", xaxt = "n",
         main = main)

    # x axis
    at = grep(":00:00", ms$timestamp)
    labels = ms$timestamp[at]
    labels = substr(labels, 12, 13)

    axis(side = 1, at = at, labels = labels, tick = F, line = -1)

    # grid
    abline(v = at, lty = 3, col = "grey")

    # identify sleeponset and wakeup (acc and sleeplog)
    onset_tmp = GGIR::POSIXtime2iso8601(from_tmp, tz = desiredtz)
    onset_acc = which(ms$timestamp == onset_tmp[1])
    onset_log = which(ms$timestamp == onset_tmp[2])
    if (length(onset_acc) == 0) onset_acc = 1
    if (length(onset_log) == 0) onset_log = 1
    error_onset_min = abs(onset_log - onset_acc) / 12

    wakeup_tmp = GGIR::POSIXtime2iso8601(to_tmp, tz = desiredtz)
    wakeup_acc = which(ms$timestamp == wakeup_tmp[1])
    wakeup_log = which(ms$timestamp == wakeup_tmp[2])
    if (length(wakeup_acc) == 0) wakeup_acc = nrow(ms)
    if (length(wakeup_log) == 0) wakeup_log = nrow(ms)
    error_wake_min = abs(wakeup_log - wakeup_acc) / 12

    # mark sleeplog reported times in plot
    abline(v = c(onset_acc, wakeup_acc), lwd = 2, col = "#ff6361")
    rect(xleft = onset_acc, xright = wakeup_acc, ybottom = ylim - 16, ytop = ylim - 2,
         border = NA, col = "#ff6361")
    text(x = mean(c(onset_acc, wakeup_acc)), y = mean(c(ylim - 16, ylim - 2)),
         labels = "Accelerometer", col = "#003e5a", font = 2, cex = 0.8)

    abline(v = c(onset_log, wakeup_log), lwd = 2, col = "#ffa600")
    rect(xleft = onset_log, xright = wakeup_log, ybottom = ylim - 30, ytop = ylim - 16,
         border = NA, col = "#ffa600")
    text(x = mean(c(onset_log, wakeup_log)), y = mean(c(ylim - 30, ylim - 16)),
         labels = "Sleeplog", col = "#003e5a", font = 2, cex = 0.8)

    # plot specific times
    text(x = min(onset_log, onset_acc), y = mean(c(ylim - 16, ylim - 2)),
         labels = fromTime[1], pos = 2, col = "#ff6361", cex = 0.6, xpd = T)
    text(x = min(onset_log, onset_acc), y = mean(c(ylim - 30, ylim - 16)),
         labels = fromTime[2], pos = 2, col = "#ffa600", cex = 0.6, xpd = T)

    text(x = max(wakeup_log, wakeup_acc), y = mean(c(ylim - 16, ylim - 2)),
         labels = toTime[1], pos = 4, col = "#ff6361", cex = 0.6, font = 2, xpd = T)
    text(x = max(wakeup_log, wakeup_acc), y = mean(c(ylim - 30, ylim - 16)),
         labels = toTime[2], pos = 4, col = "#ffa600", cex = 0.6, font = 2, xpd = T)
  }
  dev.off()
}

#' Title
#'
#' @param NSconflicts
#' @param outputfolder
#' @param desiredtz
#' @param outputplotname
#' @param onset_0
#' @param onset_1
#' @param wake_0
#' @param wake_1
#'
#' @return
#' @export
#' @import GGIR
#' @importFrom lubridate round_date
#'
#' @examples
visualize_conflicting_nights = function(outputfolder, NS, NSconflicts, desiredtz = "Europe/Madrid",
                                        onset_0 = c("sleeponset_ts", "sleeponset_ts_auto")[2],
                                        onset_1 = c("guider_onset_ts", "guider_inbedStart_ts", "sleeponset_ts_sleeplog")[3],
                                        wake_0 = c("wakeup_ts", "wakeup_ts_auto")[2],
                                        wake_1 = c("guider_wakeup_ts", "guider_inbedEnd_ts", "wakeup_ts_sleeplog")[3],
                                        outputplotname) {

  # add non wear (plot and %) - when r5long available
  # add if it is first or last night (from NS)

  # create pdf
  pdf(file.path(dirname(outputfolder),outputplotname), paper = "a4",
      height = 24, width = 18)
  par(las = 1, mar = c(3, 4, 3, 1), mfrow = c(4, 1))
  for (i in 1:nrow(NSconflicts)) {
    # load M data from basic
    path = file.path(outputfolder, paste0("meta/basic/meta_", NSconflicts$filename[i]))
    path2 = file.path(outputfolder, paste0("meta/ms2.out/", NSconflicts$filename[i]))
    M = IMP = NULL
    load(path)
    load(path2)
    ms = M$metashort
    ms$nonwear = IMP$r5long

    # identify starting and endpoint
    fromTime = c(NSconflicts[i, onset_0], NSconflicts[i, onset_1])
    fromDate = as.POSIXct(NSconflicts$calendar_date[i], tz = desiredtz, format = "%d/%m/%Y")
    hour = lubridate::hour(strptime(fromTime, format("%H:%M:%S")))
    if (min(hour) >= 0 & min(hour) < 16) fromDate = as.Date(fromDate, tz = desiredtz) + 1 # to match day classification in visual reports
    from_tmp = as.POSIXct(paste(fromDate, fromTime))

    toTime = c(NSconflicts[i, wake_0], NSconflicts[i, wake_1])
    toDate = trunc(fromDate + 60*60*26, "days")
    if (min(hour) >= 0 & min(hour) < 16) toDate = fromDate # to match day classification in visual reports
    to_tmp = as.POSIXct(paste(toDate, toTime))

    # identify in dataset
    midsleep = as.POSIXct((as.numeric(min(from_tmp)) + as.numeric(max(to_tmp))) / 2, origin = '1970-01-01')
    midsleep = lubridate::round_date(midsleep, unit = "5 seconds")
    from_tmp2 = midsleep - (12*60*60)
    to_tmp2 = midsleep + (12*60*60)
    from_iso = GGIR::POSIXtime2iso8601(from_tmp2, tz = desiredtz)
    to_iso = GGIR::POSIXtime2iso8601(to_tmp2, tz = desiredtz)

    if (from_iso %in% ms$timestamp) {
      from = which(ms$timestamp == from_iso)
    } else { # fill data wiht NA
      epoch = GGIR::iso8601chartime2POSIX(ms$timestamp[2], tz = desiredtz) - GGIR::iso8601chartime2POSIX(ms$timestamp[1], tz = desiredtz)
      epoch = as.numeric(epoch)
      rows2add = seq(from_tmp2, GGIR::iso8601chartime2POSIX(ms$timestamp[1], tz = desiredtz), by = epoch)
      rows2add = rows2add[-length(rows2add)]
      dummyMS = ms[1:length(rows2add),]
      dummyMS$timestamp = GGIR::POSIXtime2iso8601(rows2add, tz = desiredtz)
      dummyMS[, 2:ncol(dummyMS)] = NA
      ms = rbind(dummyMS, ms)
      from = 1
    }

    if (to_iso %in% ms$timestamp) {
      to = which(ms$timestamp == to_iso)
    } else { # fill data wiht NA
      epoch = GGIR::iso8601chartime2POSIX(ms$timestamp[2], tz = desiredtz) - GGIR::iso8601chartime2POSIX(ms$timestamp[1], tz = desiredtz)
      epoch = as.numeric(epoch)
      rows2add = seq(GGIR::iso8601chartime2POSIX(ms$timestamp[nrow(ms)], tz = desiredtz), to_tmp2,  by = epoch)
      rows2add = rows2add[-1]
      dummyMS = ms[1:length(rows2add),]
      dummyMS$timestamp = GGIR::POSIXtime2iso8601(rows2add, tz = desiredtz)
      dummyMS[, 2:ncol(dummyMS)] = NA
      ms = rbind(ms, dummyMS)
      to = nrow(ms)
    }

    # subset ms
    ms = ms[from:to,]

    # add nonwear percentage
    nonwear = NSconflicts$fraction_night_invalid[i]*100

    # add nonwear percentage
    night_number = NSconflicts$night[i]

    # plot angle and acceleration
    ylim = max(abs(range(ms$anglez, na.rm = T)), na.rm = T) + 20
    main = paste(paste0("ID: ", NSconflicts$ID[i]))
    sub = paste0("Night ", night_number,"; Non-wear: ", nonwear, "%")
    # plot(ms$anglez, type = "l", ylim = c(-ylim, ylim),
    #      xlab = "", ylab = "Z-axis angle (º)", xaxt = "n",
    #      main = main)
    plot(ms$anglez, type = "l", ylim = c(-ylim, ylim),
         xlab = "", ylab = "Z-axis angle (º)", xaxt = "n")

    # add title
    mtext(side = 3, line = 1.8, at = nrow(ms)/2, adj = 0.5,
          cex = 1, text = main, font = 2)
    mtext(side = 3, line = 0.6, at = nrow(ms)/2, adj = 0.5,
          cex = 0.8, text = sub)

    # x axis
    at = grep(":00:00", ms$timestamp)
    labels = ms$timestamp[at]
    labels = substr(labels, 12, 13)

    axis(side = 1, at = at, labels = labels, tick = F, line = -1)

    # grid
    abline(v = at, lty = 3, col = "grey")

    # midnight
    midnight = grep("00:00:00", ms$timestamp)
    axis(side = 1, at = midnight, labels = "00", col.axis = "indianred3",
         tick = F, line = -1, font = 2)

    # dates
    atDate1 = mean(c(1, midnight))
    date1 = substr(ms$timestamp[atDate1], 1, 10)
    axis(side = 1, at = atDate1, labels = date1,
         tick = F, line = 0.4, font = 2)
    atDate2 = ceiling(mean(c(midnight, nrow(ms))))
    date2 = substr(ms$timestamp[atDate2], 1, 10)
    axis(side = 1, at = atDate2, labels = date2,
         tick = F, line = 0.4, font = 2)

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
         labels = paste0("Guider: ", NSconflicts$guider[i]),
         col = "#003e5a", font = 2, cex = 0.8)

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

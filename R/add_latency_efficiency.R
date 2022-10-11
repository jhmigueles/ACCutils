#' Adds sleep latency and efficiency to GGIR reports
#' @description Adds sleep latency and efficiency to GGIR reports when they are not calculated (i.e., when sleeptypeWindow != "TimeInBed")
#' @param outputfolder
#'
#' @return Does not retunr anything. Instead, sleep latency and efficiency will be added to the part 4 and part 5 reports from GGIR (both full and clean reports)
#' @export
#' @details Caution! Sleep latency and efficiency depend in an accurate definition of the time in bed, which is not possible unless participants report the time to go to bed and get out of the bed in their sleep diaries.
#' @import data.table
add_latency_efficiency = function(outputfolder) {

  # identify reports ----
  reports = dir(outputfolder, full.names = T, recursive = T)
  p4_paths = grep("part4", reports, value = T)
  p5_paths = grep("part5", reports, value = T)

  # load reports ----
  p4 = p5 = list()
  for (i in 1:length(p4_paths)) p4[[i]] = data.table::fread(p4_paths[i], data.table = FALSE)
  for (i in 1:length(p5_paths)) p5[[i]] = data.table::fread(p5_paths[i], data.table = FALSE)
  names(p4) = basename(p4_paths); names(p5) = basename(p5_paths)

  # identify variables of interest in reports (inbed/spt onset and wakeups) ----
  p4onset_cn = p4wakeup_cn = list()
  p5onset_cn = p5wakeup_cn = list()
  for (i in 1:length(p4)) {
    p4onset_cn[[i]] = grep("onset|inbedStart", colnames(p4[[i]]), value = T)
    p4wakeup_cn[[i]] = grep("wakeup|inbedEnd", colnames(p4[[i]]), value = T)
  }
  for (i in 1:length(p5)) {
    p5onset_cn[[i]] = grep("onset|inbedStart", colnames(p5[[i]]), value = T)
    p5wakeup_cn[[i]] = grep("wakeup|inbedEnd", colnames(p5[[i]]), value = T)
  }
  names(p4onset_cn) = names(p4wakeup_cn) = names(p4)
  names(p5onset_cn) = names(p5wakeup_cn) = names(p5)

  # add guider onset and wakeup in summary_sleep_clean and all p5 reports ----
  # part 4
  Nfull = grep("nightsummary_sleep_full", names(p4), value = T)
  p4[[Nfull]]$sleeplatency = p4[[Nfull]]$sleeponset - p4[[Nfull]]$guider_onset
  p4[[Nfull]]$sleeplatency[which(p4[[Nfull]]$sleeplatency < 0)] = 0
  p4[[Nfull]]$guider_inbedDuration = p4[[Nfull]]$SptDuration + p4[[Nfull]]$sleeplatency
  p4[[Nfull]]$sleepefficiency = p4[[Nfull]]$SptDuration / p4[[Nfull]]$guider_inbedDuration
}

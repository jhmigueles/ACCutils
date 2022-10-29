#' Title
#'
#' @param outputfolder
#' @param criterror
#' @param outputfile
#' @param outputplotname
#' @param f0
#' @param f1
#' @param loglocation
#' @param coln1
#' @param colid
#' @param nnights
#' @param sleeplogidnum
#' @param sleeplogsep
#' @param desiredtz
#' @param sleepwindowType
#'
#' @return
#' @import GGIR
#' @export
#'
#' @examples
GGIR_automatic_vs_sleeplog = function(outputfolder, f0 = 1, f1 = 0,
                                      loglocation, coln1 = c(), colid = c(), nnights = c(),
                                      sleeplogidnum = TRUE, sleeplogsep = ",",
                                      desiredtz = "", sleepwindowType = "SPT",
                                      criterror = 2,
                                      outputfile = c(), outputplotname = c()) {
  # Run GGIR part 4 with and without sleeplog
  outputdir = unlist(strsplit(outputfolder, "output_"))[1]
  studyname = unlist(strsplit(outputfolder, "output_"))[2]
  studyname = gsub("/", "", studyname)

  # sleeplog
  GGIR::GGIR(mode = 4,
             datadir = "files/file.gt3x",  #fake datadir, will not be used
             outputdir = outputdir,
             studyname = studyname,
             desiredtz = desiredtz,
             f0 = f0, f1 = f1,
             overwrite = TRUE,
             # with sleeplog
             loglocation = loglocation,
             colid = colid, coln1 = coln1, nnights = nnights,
             sleeplogidnum = sleeplogidnum, sleeplogsep = sleeplogsep,
             sleepwindowType = sleepwindowType,
             # reports
             do.report = 4,
             do.visual = FALSE,
             visualreport = FALSE)

  # load sleeplog full niths and clean nights datasets
  p4full_path = grep("part4_nightsummary_sleep_full", dir(outputfolder, recursive = T, full.names = T), value = T)
  p4clean_path = grep("part4_nightsummary_sleep_clean", dir(outputfolder, recursive = T, full.names = T), value = T)
  NS_full_sleeplog = data.table::fread(p4full_path, data.table = FALSE)
  NS_clean_sleeplog = data.table::fread(p4clean_path, data.table = FALSE)

  # now automatic detection
  GGIR::GGIR(mode = 4,
             datadir = "files/file.gt3x",  #fake datadir, will not be used
             outputdir = outputdir,
             studyname = studyname,
             desiredtz = desiredtz,
             f0 = f0, f1 = f1,
             overwrite = TRUE,
             # with sleeplog
             loglocation = c(),
             HASPT.algo = "HDCZA",
             # reports
             do.report = 4,
             do.visual = FALSE,
             visualreport = FALSE)

  # load sleeplog full niths and clean nights datasets
  p4full_path = grep("part4_nightsummary_sleep_full", dir(outputfolder, recursive = T, full.names = T), value = T)
  p4clean_path = grep("part4_nightsummary_sleep_clean", dir(outputfolder, recursive = T, full.names = T), value = T)
  NS_full_auto = data.table::fread(p4full_path, data.table = FALSE)
  NS_clean_auto = data.table::fread(p4clean_path, data.table = FALSE)

  # merge relevant info
  keep = c("ID", "filename", "calendar_date", "night",
           "cleaningcode", "fraction_night_invalid",
           "sleeponset", "wakeup", "sleeponset_ts", "wakeup_ts")
  NS_clean = merge(NS_clean_sleeplog[, keep], NS_full_auto[, keep], by = c("ID", "filename", "calendar_date", "night", "fraction_night_invalid"),
                  all.x = TRUE, all.y = FALSE, suffixes = c("_sleeplog", "_auto"))

  # Identify nights with high criterrors
  NS_clean$error_onset = NS_clean$sleeponset_sleeplog - NS_clean$sleeponset_auto
  NS_clean$error_wake = NS_clean$wakeup_sleeplog - NS_clean$wakeup_auto

  revise = which(abs(NS_clean$error_onset) >= criterror | abs(NS_clean$error_wake) >= criterror)


  # subset nightsummary
  NSconflicts = NS_clean[revise, c("ID", "filename","calendar_date", "night",
                                 "fraction_night_invalid",
                                 "sleeponset_ts_sleeplog", "sleeponset_ts_auto", "error_onset",
                                 "wakeup_ts_sleeplog", "wakeup_ts_auto", "error_wake")]

  # Create visualization of conflicting nights
  visualize_conflicting_nights(outputfolder, NSconflicts,
                               outputplotname = outputplotname)

  # save to excel file
  openxlsx::write.xlsx(x = NSconflicts, file = file.path(dirname(outputfolder),outputfile),
                       asTable = T, overwrite = TRUE,
                       creator = "Jairo Hidalgo Migueles", sheetName = "Nights to revise",
                       firstRow = TRUE, firstCol = TRUE, colWidths = "auto", na.string = " ")

  # output data frame
  return(NSconflicts)
}

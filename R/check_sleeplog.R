#' Check the readibility of a sleeplog by the GGIR package
#' @description Function to try to read a sleeplog (csv file) by the GGIR package
#'
#' @param loglocation Path to csv file containing the sleeplog (see details)
#' @param coln1 Column number in the sleep log spreadsheet where the onset of the first night starts
#' @param colid Column number in the sleep log spreadsheet in which the participant ID code is stored (default = 1)
#' @param nnights Number of nights for which sleep log information should be available. It assumes that this is constant within a study. If sleep log information is missing for certain nights then leave these blank
#' @param sleeplogidnum Should the participant identifier as stored in the sleeplog be interpretted as a number (TRUE=default) or a character (FALSE)?
#' @param sleeplogsep Value used as sep argument for reading sleeplog csv file, usually "," or ";".
#' @param desiredtz Character (default = "", i.e., system timezone). Timezone in which device was configured and experiments took place. If experiments took place in a different timezone, then use this argument for the timezone in which the experiments took place and argument configtz to specify where the device was configured. See also https://en.wikipedia.org/wiki/Zone.tab
#' @param meta.sleep.folder Path to part3 milestone data, only specify if sleeplog is in advanced format.
#' @param SPTlowerLimit Limit to define a extremely short night in hours
#' @param SPTupperLimit Limit to define a extremely long night in hours
#' @param advanced_sleeplog Logical, if sleeplog is formatted as advanced or basic GGIR format
#' @param outputfile If defined, path to xlsx file to store the output
#' @importFrom GGIR g.loadlog
#' @import openxlsx
#' @return Save excel file with extremely short and long nights, daysleeper nights, and missing nigths from sleeplog.
#' @export

check_sleeplog = function(loglocation, coln1 = c(), colid = c(), nnights = c(),
                          sleeplogidnum = TRUE, sleeplogsep = ",",
                          desiredtz = "", meta.sleep.folder = c(),
                          SPTlowerLimit = 4, SPTupperLimit = 10,
                          advanced_sleeplog = FALSE,
                          outputfile = c()) {

  # check consecutive dates

  # check arguments meta.sleep.folder
  if (advanced_sleeplog & length(meta.sleep.folder) == 0) {
    stop("Please, define meta.sleep.folder when using an advanced sleeplog")
  }

  # read sleeplog
  log = GGIR::g.loadlog(loglocation = loglocation, coln1 = coln1, colid = colid, nnights = nnights,
                        sleeplogidnum = sleeplogidnum, sleeplogsep = sleeplogsep,
                        desiredtz = desiredtz, meta.sleep.folder = meta.sleep.folder)


  # get data
  sleeplog = log$sleeplog
  nonwearlog = log$nonwearlog
  naplog = log$naplog
  rm(log); gc()

  # Are there puntuation symbols other than : in times?
  rPunct_onset = grep("(?![:])[[:punct:]]", sleeplog$sleeponset, perl = TRUE)
  rPunct_wake = grep("(?![:])[[:punct:]]", sleeplog$sleepwake, perl = TRUE)
  rLetters_onset = grep("[A-z]", sleeplog$sleeponset)
  rLetters_wake = grep("[A-z]", sleeplog$sleepwake)
  rSemiColon_onset = which(lengths(gregexpr(":", sleeplog$sleeponset)) != 2)
  rSemiColon_wake = which(lengths(gregexpr(":", sleeplog$sleepwake)) != 2)

  rFormat = c(rPunct_onset, rPunct_wake, rLetters_onset, rLetters_wake, rSemiColon_onset, rSemiColon_wake)
  rFormat_Message = "Revise formatting of these timestamps"

  formatDurs = round(sleeplog$duration[rFormat], 1)
  formatOnsets = sleeplog$sleeponset[rFormat]
  formatWakes = sleeplog$sleepwake[rFormat]

  # short or long nights
  shortSPT = which(sleeplog$duration <= SPTlowerLimit)
  shortDurs = round(sleeplog$duration[shortSPT], 1)
  shortOnsets = sleeplog$sleeponset[shortSPT]
  shortWakes = sleeplog$sleepwake[shortSPT]
  shortMessage = paste0("Night shorter than ", SPTlowerLimit, " hours")

  longSPT = which(sleeplog$duration >= SPTupperLimit)
  longDurs = round(sleeplog$duration[longSPT], 1)
  longOnsets = sleeplog$sleeponset[longSPT]
  longWakes = sleeplog$sleepwake[longSPT]
  longMessage = paste0("Night longer than ", SPTupperLimit, " hours")

  # daysleeper
  wakeup = strsplit(sleeplog$sleepwake, ":")
  wakeup_hr = c()
  for (i in 1:length(wakeup)) {
    W = as.numeric(wakeup[[i]])
    wakeup_hr[i] = W[1] + W[2]/60 + W[3]/3600
  }
  daysleeper = which(wakeup_hr >= 12)
  if (length(daysleeper) > 0) {
    daysleeperDurs = round(sleeplog$duration[daysleeper], 1)
    daysleeperOnsets = sleeplog$sleeponset[daysleeper]
    daysleeperWakes = sleeplog$sleepwake[daysleeper]
  }
  if (length(daysleeper) == 0) daysleeperDurs = daysleeperOnsets = daysleeperWakes = NULL
  dayslMessage = "Daysleeper (woke up after noon)"

  # missing days
  uniqueIDs = unique(sleeplog$ID)
  missing_nights = data.frame(ID = NA, night = NA, observations = NA,
                              duration = NA, onset = NA, wakeup = NA)
  for (i in 1:length(uniqueIDs)) {
    if (i == 1) row = 1
    nights = sleeplog$night[which(sleeplog$ID == uniqueIDs[i])]
    missing_tmp = which(!(1:nnights %in% nights))
    if (length(missing_tmp) > 0) {
      rows2fill = row:(row + length(missing_tmp) - 1)
      missing_nights[rows2fill, 1] = uniqueIDs[i]
      missing_nights[rows2fill, 2] = missing_tmp
      missing_nights[rows2fill, 3] = "Missing, sleeplog will not be used for this night"
      row = nrow(missing_nights) + 1
    }
  }

  # output: nights to revise
  nights2rev = c(rFormat, shortSPT, longSPT, daysleeper)
  output = data.frame(ID = sleeplog$ID[nights2rev],
                      night = sleeplog$night[nights2rev],
                      observations = c(rep(rFormat_Message, length(rFormat)),
                                       rep(shortMessage, length(shortSPT)),
                                       rep(longMessage, length(longSPT)),
                                       rep(dayslMessage, length(daysleeper))),
                      duration = c(formatDurs, shortDurs, longDurs, daysleeperDurs),
                      onset = c(formatOnsets, shortOnsets, longOnsets, daysleeperOnsets),
                      wakeup = c(formatWakes, shortWakes, longWakes, daysleeperWakes))

  output = rbind(output, missing_nights)

  output = output[order(output$ID),]

  # output summary: nights to revise
  SUM = data.frame(Issue = c("Revise formatting", "Short nights", "Long nights", "Day sleeper", "Missing nights", "TOTAL"),
                   Times = c(length(rFormat), length(shortSPT), length(longSPT), length(daysleeper), nrow(missing_nights),
                             sum(c(length(rFormat), length(shortSPT), length(longSPT), length(daysleeper), nrow(missing_nights)))))

  # save to excel file
  openxlsx::write.xlsx(x = list(SUM, output), file = outputfile, asTable = T, overwrite = TRUE,
                       creator = "Jairo Hidalgo Migueles", sheetName = c("Summary", "Details"),
                       firstRow = TRUE, firstCol = TRUE, colWidths = "auto", na.string = " ")
  # openxlsx::write.xlsx(x = output, file = outputfile, asTable = T, overwrite = TRUE)
  return(list(SUM, output))
}

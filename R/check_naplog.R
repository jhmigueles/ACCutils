#' Check the readibility of a nap log by the GGIR package
#' @description Function to try to read a nap log from an advanced sleeplog (csv file) by the GGIR package
#'
#' @param loglocation Path to csv file containing the sleeplog (see details)
#' @param coln1 Column number in the sleep log spreadsheet where the onset of the first night starts
#' @param colid Column number in the sleep log spreadsheet in which the participant ID code is stored (default = 1)
#' @param nnights Number of nights for which sleep log information should be available. It assumes that this is constant within a study. If sleep log information is missing for certain nights then leave these blank
#' @param sleeplogidnum Should the participant identifier as stored in the sleeplog be interpretted as a number (TRUE=default) or a character (FALSE)?
#' @param sleeplogsep Value used as sep argument for reading sleeplog csv file, usually "," or ";".
#' @param desiredtz Character (default = "", i.e., system timezone). Timezone in which device was configured and experiments took place. If experiments took place in a different timezone, then use this argument for the timezone in which the experiments took place and argument configtz to specify where the device was configured. See also https://en.wikipedia.org/wiki/Zone.tab
#' @param meta.sleep.folder Path to part3 milestone data, only specify if sleeplog is in advanced format.
#' @param NapLowerLimit Limit to define a extremely short nap in hours
#' @param NapUpperLimit Limit to define a extremely long nap in hours
#' @param outputfile If defined, path to xlsx file to store the output
#'
#' @importFrom GGIR g.loadlog
#' @import openxlsx
#' @return Save excel file with extremely short and long nights, daysleeper nights, and missing nigths from sleeplog.
#' @export

check_naplog = function(loglocation, coln1 = c(), colid = c(), nnights = c(),
                        sleeplogidnum = TRUE, sleeplogsep = ",",
                        desiredtz = "", meta.sleep.folder = c(),
                        NapLowerLimit = 15/60, NapUpperLimit = 2,
                        outputfile = c()) {

  # check arguments meta.sleep.folder
  if (length(meta.sleep.folder) == 0) {
    stop("Please, define meta.sleep.folder when using an advanced sleeplog")
  }

  # read sleeplog
  log = GGIR::g.loadlog(loglocation = loglocation, coln1 = coln1, colid = colid, nnights = nnights,
                        sleeplogidnum = sleeplogidnum, sleeplogsep = sleeplogsep,
                        desiredtz = desiredtz, meta.sleep.folder = meta.sleep.folder)
  # get data
  log = log$naplog
  t0 = 3; t1 = 4 # nap start and nap end columns

  # remove rows with no naps
  log = log[-which(log[, t0] == "" & log[, t1] == ""),]

  # nap durations
  t0_hr = t1_hr = NapDurs = c()
  for (i in 1:nrow(log)) {
    time0 = as.numeric(strsplit(log[i, t0],":")[[1]])
    time1 = as.numeric(strsplit(log[i, t1],":")[[1]])
    t0_hr[i] = time0[1] + time0[2]/60 + time0[3]/3600
    t1_hr[i] = time1[1] + time1[2]/60 + time1[3]/3600
    NapDurs = t1_hr - t0_hr
  }

  # Find missing start or end of naps
  missingNap = which(is.na(NapDurs))
  missingOnset = log[missingNap, t0]
  missingWake = log[missingNap, t1]
  missingMessage = "Empty onset or offset time of the nap"

  # Are there puntuation symbols other than : in times?
  rPunct_t0 = grep("(?![:])[[:punct:]]", log[, t0], perl = TRUE)
  rPunct_t1 = grep("(?![:])[[:punct:]]", log[, t1], perl = TRUE)
  rLetters_t0 = grep("[A-z]", log[, t0])
  rLetters_t1 = grep("[A-z]", log[, t1])
  rSemiColon_t0 = which(lengths(gregexpr(":", log[, t0])) != 2)
  rSemiColon_t1 = which(lengths(gregexpr(":", log[, t1])) != 2)
  rFormat = c(rPunct_t0, rPunct_t1, rLetters_t0, rLetters_t1, rSemiColon_t0, rSemiColon_t1)
  rFormat_Message = "Revise formatting of these timestamps"
  formatOnsets = log[, t0][rFormat]
  formatWakes = log[, t1][rFormat]
  formatDurs = NapDurs[rFormat]

  # Very short/long naps
  shortNap = which(NapDurs <= NapLowerLimit)
  shortDurs = round(NapDurs[shortNap], 2)
  shortOnsets = log[, t0][shortNap]
  shortWakes = log[, t1][shortNap]
  shortMessage = paste0("Nap shorter than ", NapLowerLimit, " hours")

  longNap = which(NapDurs >= NapUpperLimit)
  longDurs = round(NapDurs[longNap], 2)
  longOnsets = log[, t0][longNap]
  longWakes = log[, t1][longNap]
  longMessage = paste0("Nap longer than ", NapUpperLimit, " hours")

  # output: nights to revise
  naps2rev = c(rFormat, shortNap, longNap, missingNap)
  output = data.frame(ID = log$ID[naps2rev],
                      night = log$date[naps2rev],
                      observations = c(rep(rFormat_Message, length(rFormat)),
                                       rep(shortMessage, length(shortNap)),
                                       rep(longMessage, length(longNap)),
                                       rep(missingMessage, length(missingNap))),
                      duration = c(formatDurs, shortDurs, longDurs, rep("", length(missingNap))),
                      onset = c(formatOnsets, shortOnsets, longOnsets, missingOnset),
                      wakeup = c(formatWakes, shortWakes, longWakes, missingWake))

  output = output[order(output$ID),]

  # output summary: nights to revise
  SUM = data.frame(Issue = c("Revise formatting", "Short naps", "Long naps", "Missing naps", "TOTAL"),
                   Description = c("Time format has an unexpected character (e.g., letters, points)",
                                   shortMessage, longMessage, missingMessage, ""),
                   N = c(length(rFormat), length(shortNap), length(longNap), length(missingNap),
                         sum(c(length(rFormat), length(shortNap), length(longNap), length(missingNap)))))

  # save to excel file
  openxlsx::write.xlsx(x = list(SUM, output), file = outputfile, asTable = T, overwrite = TRUE,
                       creator = "Jairo Hidalgo Migueles", sheetName = c("Summary", "Details"),
                       firstRow = TRUE, firstCol = TRUE, colWidths = "auto", na.string = " ")
  # openxlsx::write.xlsx(x = output, file = outputfile, asTable = T, overwrite = TRUE)
  return(list(SUM, output))
}

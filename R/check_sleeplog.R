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
#'
#' @return
#' @export
#' @details

check_sleeplog = function(loglocation, coln1 = c(), colid = c(), nnights = c(),
                          sleeplogidnum = TRUE, sleeplogsep = ",",
                          desiredtz = "") {
  # read sleeplog
  log = GGIR::loadlog(loglocation = loglocation, coln1 = coln1, colid = colid, nnights = nnights,
                                      sleeplogidnum = sleeplogidnum, sleeplogsep = sleeplogsep,
                                      desiredtz = desiredtz)

}

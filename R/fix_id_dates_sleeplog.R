#' Check that ID and dates are logical and fix them if needed
#'
#' @param loglocation Path to csv file containing the sleeplog (see details)
#' @param colid Column number in the sleep log spreadsheet in which the participant ID code is stored (default = 1)
#' @param meta.sleep.folder Path to part3 milestone data, only specify if sleeplog is in advanced format.
#' @param advanced_sleeplog Logical, if sleeplog is formatted as advanced or basic GGIR format
#' @param dateformat Format of date in sleeplog as in \link{strptime}
#' @param outputxlsx If provided, path to xlsx file to save the revised sleeplog with tracked changes
#' @param outputGGIR Path to csv file for sleeplog to be read by GGIR
#'
#' @return Save excel and csv file with new sleeplog to be read by GGIR
#' @export
#' @import openxlsx
fix_sleeplog = function(loglocation, colid, meta.sleep.folder,
                        advanced_sleeplog = TRUE,
                        dateformat = "%d/%m/%Y",
                        fix_start_date_diff = 30,
                        outputxlsx,
                        outputGGIR) {
  # read sleeplog
  log = read.csv(loglocation)

  # remove extra rows
  log = log[which(!is.na(log[colid])),]

  # check arguments meta.sleep.folder
  if (advanced_sleeplog) {
    if (length(meta.sleep.folder) == 0) {
      stop("Please, define meta.sleep.folder when using an advanced sleeplog")
    } else {
      m3files_fn = dir(meta.sleep.folder, full.names = TRUE)
      m3files = basename(m3files_fn)
    }

    # identify columns
    datecols = grep("date", colnames(log), ignore.case = TRUE)
    wakecols = grep("wake|outbed", colnames(log), ignore.case = TRUE)
    onsetcols = grep("onset|inbed", colnames(log), ignore.case = TRUE)
    napcols = grep("nap", colnames(log), ignore.case = TRUE)
    nonwearcols = grep("nonwear", colnames(log), ignore.case = TRUE)

    # remove extra columns
    log_bu = log
    log = log[, sort(c(colid, datecols, wakecols, onsetcols, napcols, nonwearcols))]

    # style to highlight changes
    cols = rows = c()
    highlight = openxlsx::createStyle(numFmt = "GENERAL", borderStyle = "none", fgFill = "#F8E469")

    # Revise sleeplog
    for (i in 1:nrow(log)) {
      # load ID part 3
      id = log[i, colid]
      cat(id, " ")
      ID = rec_starttime = NA      # metadata part 3
      file2load = grep(paste0("^",id), m3files)
      if (length(file2load) == 1) {
        load(m3files_fn[file2load])
        matched = TRUE
      } else if (length(file2load) == 0) {
        warning(paste0("ID: ", id, " not matched"))
        matched = FALSE
        next
      }

      # fix ID if needed
      if (matched == TRUE) {
        log[i, colid] = ID
        if (ID != id) {
          cols = c(cols, colid)
          rows = c(rows, i + 1)
        }
      }

      # start date
      log_startdate = as.POSIXct(log[i, datecols[1]], format = dateformat, tz = "")
      firstdate = log_startdate
      if (matched == TRUE) rec_startdate = as.POSIXct(substr(rec_starttime, 1, 10), tz = "")
      if (matched == FALSE) rec_startdate = log_startdate

      if (!is.na(log_startdate)) {
        if (abs(difftime(log_startdate, rec_startdate)) > fix_start_date_diff) {
          log[i, datecols[1]] = as.character(rec_startdate)
          firstdate = rec_startdate

          if (log_startdate != log[i, datecols[1]]) {
            cols = c(cols, datecols[1])
            rows = c(rows, i + 1)
          }
        } else {
          log[i, datecols[1]] = as.character(log_startdate)
          firstdate = log_startdate
        }
      } else {
        # are there future dates in sleeplog?
        futuredates = datecols[which(!is.na(log[i, datecols]) & nchar(log[i, datecols]) > 2)]
        if (length(futuredates) == 0) log_startdate = rec_startdate
        log[i, datecols[1]] = as.character(rec_startdate)
        firstdate = rec_startdate
        cols = c(cols, datecols[1])
        rows = c(rows, i + 1)
      }



      # next dates
      for (curdatecol in datecols[-1]) {
        if (curdatecol == datecols[2]) {
          curdate = c()
          prev_date = firstdate
        } else {
          prev_date = curdate
        }

        # redefine current date
        curdate = prev_date + as.difftime(1, units = "days")
        curdatechar = as.character(curdate)
        if (nchar(curdatechar) > 10) {
          curdatechar_bu = curdatechar
          curdatechar = substr(curdatechar, 1, 10)
          curdate = as.POSIXct(curdatechar, tz = "")
          if (substr(curdatechar_bu, 12, 13) == "23") {
            curdate = prev_date + as.difftime(1.5, units = "days")
            curdatechar = substr(as.character(curdate), 1, 10)
            curdate = as.POSIXct(curdatechar, tz = "")
          }
        }

        log_date = as.POSIXct(log[i, curdatecol], format = dateformat, tz = "")
        log[i, curdatecol] = as.character(curdate)

        if (is.na(log_date)) {
          cols = c(cols, curdatecol)
          rows = c(rows, i + 1)
        } else if (log_date != curdate) {
          cols = c(cols, curdatecol)
          rows = c(rows, i + 1)
        }
      }

      # punctuation typos in times
      for (curdatecol in datecols) {
        nextdatecol = datecols[which(datecols == curdatecol) + 1]
        if (is.na(nextdatecol)) nextdatecol = ncol(log) + 1
        columns2revise = (curdatecol + 1):(nextdatecol - 1)

        for (column in columns2revise) {
          rPunct = rLetters = c()
          rPunct = grep("(?![:])[[:punct:]]", log[, column], perl = TRUE)
          rLetters = grep("[A-z]", log[, column])

          if (length(rPunct) > 0) {
            log[rPunct, column] = gsub("(?![:])[[:punct:]]", ":", log[rPunct, column], perl = TRUE)
            cols = c(cols, rep(column, length(rPunct)))
            rows = c(rows, rPunct + 1)
          }

          if (length(rLetters) > 0) {
            log[rLetters, column] = gsub("[A-z]", "", log[rLetters, column], perl = TRUE)
            cols = c(cols, rep(column, length(rLetters)))
            rows = c(rows, rLetters + 1)
          }
        }
      }
    }

    # create workbook
    log2 = openxlsx::createWorkbook(creator = "Jairo Hidalgo Migueles", title = "sleeplog_GGIR")
    openxlsx::addWorksheet(log2, "sleeplog")
    openxlsx::writeData(wb = log2, sheet = 1, x = log,
                        startCol = colid, startRow = 1, colNames = TRUE)

    # highlight cells
    openxlsx::addStyle(wb = log2, sheet = 1, style = highlight,
                       cols = cols, rows = rows)

    # save workbook and log
    openxlsx::saveWorkbook(log2, file = outputxlsx, overwrite = TRUE)
    write.csv(log, file = outputGGIR, row.names = FALSE)
  }
}

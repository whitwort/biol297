# File naming utilities
currentUser <- function() { Sys.info()[["user"]] }

studentPath <- function(username = currentUser()) {
  file.path(homePath, username, config$package$studentProject)
}

latestVersion   <- function(base, path = submitPath) { sortTime(base, path)[1] }

previousVersion <- function(base, path = submitPath) { sortTime(base, path)[2] }

sortTime <- function(base, path = submitPath) {
  files <- list.files( path    = path
                     , pattern = paste("^", base, ".*", "html$", sep = "")
                     )
  
  if (length(files) > 0) {
    sort(splitTime(files), decreasing = TRUE) 
  } else {
    NA
  }
}

fileBase  <- function(files) { splitter(files, 1) }

splitTime <- function(files) { splitter(files, 2) }

splitExt  <- function(files) {
  s <- splitter(files, 3:4)
  if (is.na(s[2])) { s[1] } else { s[2] }
}

splitter  <- function(files,  i) {
  sapply( strsplit(basename(files), split =".", fixed = TRUE)
        , function(parts) { parts[i] }
        )
}

readFile  <- function(file) { paste(readLines(file), sep = "\n") }

file.move <- function(from, to) {
  file.copy(from, to)
  file.remove(from)
}

ctimeStr  <- function(time = Sys.time()) {
  format(time, paste("%Y", "%m", "%d", "%H", "%M", "%S", sep = "-"))
}
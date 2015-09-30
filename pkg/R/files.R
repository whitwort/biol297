# File naming utilities
currentUser <- function() { Sys.info()[["user"]] }

studentPath <- function(username = currentUser()) {
  conf <- loadConfig()
  file.path(conf$homePath, username, conf$package$studentProject)
}

latestVersion   <- function(base, path) { sortTime(base, path)[1] }
previousVersion <- function(base, path) { sortTime(base, path)[2] }
sortTime <- function(base, path) {
  files <- list.files( path    = path
                     , pattern = paste("^", base, ".*", "html$", sep = "")
                     )
  
  if (length(files) > 0) {
    sort(splitTime(files), decreasing = TRUE)
  } else {
    NA
  }
}

file.move <- function(from, to) {
  file.copy(from, to)
  file.remove(from)
}

ctimeStr  <- function(time = Sys.time()) {
  format(time, paste("%Y", "%m", "%d", "%H", "%M", "%S", sep = "-"))
}

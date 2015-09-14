## TODO:  configuration

# source
projectPath <- "/home/gregg/biol297"
contentPath <- file.path(projectPath, "content")
dataPath    <- file.path(contentPath, "data")

# students
submitPath  <- "submitted"

## API

#' Open the course website in the Viewer pane
#' 
#' @param url A string containing the URL to open.  Defaults to the course
#'   homepage.
#'
#' @export
#'
view <- function(url = "http://rna.wlu.edu/bio297") { rstudio::viewer(url) }

#' List available excercises for this course
#'
#' @return A data.frame with exercise titles and file names.  Start an
#'   exercise using \code{\link{start}}.
#' @export
#'
listExercises <- function() {
  manifest   <- courseR::manifest(projectPath)
  exercises  <- sapply(manifest$section, yaml::yaml.load) == "Exercises"
  
  data.frame( title = sapply(manifest$title[exercises], yaml::yaml.load)
            , file  = sapply(manifest$file[exercises],  yaml::yaml.load)
            , row.names = 1:(sum(exercises))
            )
  
}

#' Start new exercises for this course
#' 
#' Calling \code{\link{start}} will create a local copy of any new .r or .rmd 
#' files for exercises for this course and update your data subfolder with 
#' related data files.
#' 
#' Existing files will NOT be overwriten.  If you want a fresh copy of an 
#' exercise simply rename your old file and call \code{\link{start}} again.
#' 
#' See \code{\link{submit}} to submit your solutions for review.
#' 
#' @param data A character string.  The directory where data files should be
#'   copied.  Defaults to "data".
#'   
#' @export
#' 
start <- function(data = "data") {
  
  wd <- getwd()
  if (basename(wd) != "bio297") {
    stop("Please create a new project in RStudio in a directory named 'bio297', 
          then run this function with that project loaded (your current working 
          directory must be '~/bio297').")
  }
  
  exercises <- listExercises()
  for (file in exercises$file) {
    if (!file.exists(file)) {
      file.copy( file.path(contentPath, file)
               , file
               , overwrite = FALSE
               )
      message("Exercise file created: ", file)
    }
  }
  
  if (!dir.exists(data)) { dir.create(data) }
  
  dataFiles <- list.files(dataPath)
  for (file in dataFiles) {
    local <- file.path(data, file)
    if (!file.exists(local)) {
      file.copy( file.path(dataPath, file)
               , local
               )
      message("Data file created: ", data, "/", file)
    }
  }
  
  if (!dir.exists(submitPath)) {
    dir.create(submitPath)
    file.symlink(data, file.path(submitPath, data))
  }
  
}

#' Submit your solution to an exercise
#'
#' @param exercise A character string.  The name of the file to submit.  See 
#'   \code{\link{listExercises}} for a list of excercise file names for this
#'   course, or \code{\link{status}} to see which exercises you have started but
#'   not submitted.
#'
#' @export
submit <- function(exercise) {
  
  if (!dir.exists(submitPath)) {
    dir.create(submitPath)
    file.symlink(data, file.path(submitPath, data))
  }
  
  if (!file.exists(exercise)) {
    stop("There is no source file named: ", exercise)
  }
  
  fileBase <- strsplit(exercise, ".", fixed = TRUE)[[1]][1]
  subFile  <- paste(fileBase, as.numeric(Sys.time()), "rmd", sep = ".")
  file.copy(exercise, file.path(submitPath, subFile))
  
  message("Submitted ", exercise, " as ", subFile)
}

#' Check the status of exercises you're working on for this course.
#'
#' @export
#'
status <- function() {
  message("The cake is a lie.  Check back later to see the status of your submissions.")
}

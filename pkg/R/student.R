## API

#' Open the course website in the Viewer pane
#' 
#' @param url A string containing the URL to open.  Defaults to the course
#'   homepage.
#'
#' @export
#'
view <- function(url = config$templateData$baseURL) { rstudio::viewer(url) }

#' List available excercises for this course
#'
#' @return A data.frame with exercise titles and file names.  Start an
#'   exercise using \code{\link{start}}.
#' @export
#'
listExercises <- function() {
  exercises   <- manifest$section == "Exercises"
  
  data.frame( title = manifest$title[exercises]
            , file  = manifest$file[exercises]
            , row.names = 1:(sum(exercises))
            , stringsAsFactors = FALSE
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
  checkStudent()
  
  exercises <- listExercises()
  for (file in exercises$file) {
    
    if (!file.exists(file)) {
      s <- readLines(file.path(contentPath, file))
      writeLines(s[s != "##"],  file)
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
  
  if (!dir.exists(submitPath)) { dir.create(submitPath) }
  
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
  checkStudent()
  
  if (!dir.exists(submitPath)) { dir.create(submitPath) }
  
  archivePath <- file.path(submitPath, "archive")
  if (!dir.exists(archivePath)) { dir.create(archivePath) }
  
  if (!file.exists(exercise)) { 
    stop("There is no source file named: ", exercise)
  }
  
  base     <- fileBase(exercise)
  outPath  <- file.path(submitPath, paste(base, ctimeStr(), "html", sep = "."))
  
  message("Knitting & rendering...")
  rmarkdown::render( exercise
                   , output_file = outPath
                   , output_dir  = submitPath
                   )
  
  message("Submitted: ", base)

}

#' Check the status of exercises you're working on for this course.
#'
#' @export
#'
status <- function() {
  checkStudent()
  username <- currentUser()
  message("Status for: ", username)
  
  dataPath <- file.path(exercisePath, paste(username, "rds", sep = "."))
  if (!file.exists(dataPath)) {
    stop("You do not appear to be a student in this course.")
  }
  
  studentData <- readRDS(dataPath)
  plyr::ldply( fileBase(listExercises()$file)
             , function(base) {
                  version   <- latestVersion(base)
                  submitted <- if (is.na(version)) { FALSE } else { TRUE }
                  received  <- !( is.null(studentData$exercises[[base]]) || 
                                  is.null(studentData$exercises[[base]][[version]])
                                )
                  if (received) {
                    response <- !(is.na(studentData$exercises[[base]][[version]]$response))
                    done     <- studentData$exercises[[base]][[version]]$done
                  } else {
                    response <- FALSE
                    done     <- FALSE
                  }
                  
                  data.frame( exercise      = base
                            , submitted     = submitted
                            , received      = received
                            , feedback      = response
                            , done          = done
                            , version = version
                            )
               }
             )
}

checkStudent <- function() {
  if (getwd() != studentPath()) {
    stop("You do not appear to be calling this function from a student project.

         Please create a new project in RStudio in a directory named 'bio297', 
         then run this function with that project loaded (your current working 
         directory must be '~/bio297').")
  }
}




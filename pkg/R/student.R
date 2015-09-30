## API

#' Open the course website in the Viewer pane
#' 
#' @param url A string containing the URL to open.  Defaults to the course
#'   homepage.
#'
#' @export
#'
view <- function(url = config$templateData$baseURL) { 
  config <- loadConfig()
  rstudio::viewer(url) 
}

#' List available excercises for this course
#'
#' @return A data.frame with exercise titles and file names.  Start an
#'   exercise using \code{\link{start}}.
#' @export
#'
listExercises <- function() {
  manifest  <- loadManifest()
  exercises <- manifest$section == "Exercises"
  
  data.frame( title = manifest$title[exercises]
            , file  = manifest$file[exercises]
            , row.names = 1:(sum(exercises))
            , stringsAsFactors = FALSE
            )
  
}

#' Fetch new exercises and data for this course
#' 
#' Calling \code{\link{Fetch}} will create a local copy of any new .r or .rmd 
#' files for exercises for this course and update your data subfolder with 
#' related data files.
#' 
#' Existing files will NOT be overwriten.  If you want a fresh copy of an 
#' exercise simply rename your old file and call \code{\link{Fetch}} again.
#' 
#' See \code{\link{submit}} to submit your solutions for review.
#' 
#' @param data A character string.  The directory where data files should be
#'   copied.  Defaults to "data".
#'   
#' @export
#' 
fetch <- function(data = "data") {
  checkStudent()
  conf <- loadConfig()
  
  exercises <- listExercises()
  for (file in exercises$file) {
    
    if (!file.exists(file)) {
      file.copy(file.path(conf$exercisePath, file), file)
      message("Exercise file created: ", file)
    }
    
  }
  
  if (!dir.exists(data)) { dir.create(data) }
  
  dataFiles <- list.files(conf$dataPath)
  for (file in dataFiles) {
    
    local <- file.path(data, file)
    if (!file.exists(local)) {
      file.copy( file.path(conf$dataPath, file)
               , local
               )
      message("Data file created: ", data, "/", file)
    }
    
  }
  
  if (!dir.exists(conf$submitPath)) { dir.create(conf$submitPath) }
  
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
  conf <- loadConfig()
  
  if (!dir.exists(conf$submitPath)) { dir.create(conf$submitPath) }
  
  if (!file.exists(exercise)) { 
    stop("There is no source file named: ", exercise)
  }
  
  base     <- fileBase(exercise)
  outPath  <- file.path(conf$submitPath, paste(base, ctimeStr(), "html", sep = "."))
  
  message("Knitting & rendering...")
  html <- rmarkdown::html_document()
  
  exDb         <- readRDS(file.path(conf$exercisePath, "exercises.rds"))
  gradedChunks <- exDb[[base]]

  chunkFeedback <- function(x, options) {
    if (options$label %in% gradedChunks) {
      insert <- paste("<div>", formatFeedbackChunk(options$label), "</div>", sep="")
      paste(x, insert, sep = "\n")
    } else {
      x
    }
  }
  html$knitr <- list(knit_hooks = list(chunk = chunkFeedback))
  
  rmarkdown::render( exercise
                   , output_format = html
                   , output_file   = outPath
                   , output_dir    = conf$submitPath
                   )
  
  message("Submitted: ", base)

}

#' Check the status of exercises you're working on for this course.
#'
#' @export
#'
status <- function() {
  checkStudent()
  conf <- loadConfig()
  
  username <- currentUser()
  message("Status for: ", username)
  
  dataPath <- file.path(conf$exercisePath, paste(username, "rds", sep = "."))
  if (!file.exists(dataPath)) {
    stop("You do not appear to be a student in this course.")
  }
  
  message("Use bio297::feedback() to launch the feedback viewer.")
  
  studentData <- readRDS(dataPath)
  plyr::ldply( fileBase(listExercises()$file)
             , function(base) {
                  version   <- latestVersion(base, conf$submitPath)
                  submitted <- if (is.na(version)) { FALSE } else { TRUE }
                  received  <- !( is.null(studentData$exercises[[base]]) || 
                                  is.null(studentData$exercises[[base]][[version]])
                                )
                  if (received) {
                    done     <- studentData$exercises[[base]][[version]]$done
                    if (done) {
                      response <- TRUE
                    } else {
                      response <- !(is.na(studentData$exercises[[base]][[version]]$response))
                    }
                  } else {
                    response <- FALSE
                    done     <- FALSE
                  }
                  
                  data.frame( exercise      = base
                            , submitted     = submitted
                            , received      = received
                            , feedback      = response
                            , done          = done
                            , version       = version
                            , stringsAsFactors = FALSE
                            )
               }
             )
}

#' Launch a shiny app to view exercise feedback.
#'
#' @export
#'
feedback <- function(skin = "purple") {
  checkStudent()
  conf    <- loadConfig()
  student <- currentUser()
  
  message("You may find bugs in this feature; please let me know!")
  
  dataPath <- file.path(conf$exercisePath, paste(student, "rds", sep = "."))
  if (!file.exists(dataPath)) {
    stop("You do not appear to be a student in this course.")
  }
  studentData <- readRDS(dataPath)
  
  app <- shinyApp( ui     = feedbackUI(conf, studentData, skin)
                 , server = feedbackServer(conf, studentData)
                 )
  
  runApp(app)
}

formatFeedbackChunk <- function(chunk) {
  paste("{{chunk-feedback-", chunk, "}}", sep = "")
}

checkStudent <- function() {
  if (getwd() != studentPath()) {
    stop("You do not appear to be calling this function from a student project.

         Please create a new project in RStudio in a directory named 'bio297', 
         then run this function with that project loaded (your current working 
         directory must be '~/bio297').")
  }
}

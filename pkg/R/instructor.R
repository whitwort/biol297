# studentData RDS file data structure:
#   $username
#   $submitPath
#   $exercises
#     $exercise  (base name: "03-tidy-data-1")
#       $version (time string:  "2015-09-16-21-18-52")
#         $html
#         $response
#         $done

#' Update database of student submittions (for Instructors)
#'
#' Scans students' project directories to check for newly (re)submitted
#' assignments.
#'
#' @param students A character vector of student usernames.  Defaults to all.
#'
#' @export
#'
update <- function( students = readLines(studentList)
                  ) {
  
  checkInstructor()
  for (student in students) { updateStudent(student) }
  
}

updateStudent <- function(username) {
  message("Updating ", username, "...")
  
  studentDataFile <- file.path(exercisePath, paste(username, "rds", sep = "."))
  if ( !file.exists(studentDataFile) ) {
    studentData <- list( username   = username
                       , submitPath = file.path(studentPath(username), submitPath)
                       , exercises  = list() # [[exercise]][[version]] = $done, $response, $html
                       )
  } else {
    studentData <- readRDS(studentDataFile)
  }
  
  if (!dir.exists(studentData$submitPath)) {
    stop( "Submission path for student `"
        , username
        , "` does not exist:\n"
        , studentData$submitPath
        )
  }
  
  saveRDS(checkSubmissions(studentData), studentDataFile)
  Sys.chmod(studentDataFile, mode = "0755")
}

checkSubmissions <- function(studentData) {
  
  exercises <- listExercises()
  
  for (exercise in exercises$file) {
    base <- fileBase(exercise)
    
    if (is.null(studentData[[base]])) { 
      studentData$exercises[[base]] <- list() 
    }
    
    latest <- latestVersion(base, studentData$submitPath)
    if (!is.na(latest) && is.null(studentData$exercises[[latest]])) {
      fileName   <- paste(base, latest, "html", sep = ".") 
      submission <- list( html     = readFile(file.path(studentData$submitPath, fileName))
                        , response = NA
                        , done     = FALSE
                        )
      studentData$exercises[[base]][[latest]] <- submission
    }
    
  }
  
  studentData
}

checkInstructor <- function() { 
  if (!file.exists(studentList)) {
    stop("You do not appear to be in an instructor project.")
  }
}
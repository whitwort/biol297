# studentData RDS file data structure:
#   $username
#   $submitPath
#   $exercises
#     $exercise  (base name: "03-tidy-data-1")
#       $version (time string:  "2015-09-16-21-18-52")
#         $htmlPath
#         $response: $chunk: $grade, $body
#         $done
#         $version
#         $student
#         $exercise
#         $rdsPath

# submissionData structure:
# $exercise
#   $student
#     $version
#         $htmlPath
#         $response: $chunk: $grade, $body
#         $done
#         $version
#         $student
#         $exercise
#         $rdsPath

#' Update database of student submittions (for Instructors)
#'
#' Scans students' project directories to check for newly (re)submitted
#' assignments.
#'
#'
#' @export
#'
updateData <- function() {
  checkInstructor()
    
  conf           <- loadConfig()
  students       <- readLines(conf$studentList)
  
  exercises      <- fileBase(listExercises()$file)
  submissionData <- sapply(exercises, function(name) { list() })
  
  submissionData$summary <- data.frame(student = students, row.names = students)
  for (student in students) { 
    studentSubmissions <- updateStudent(student)
    for (exercise in exercises) {
      submissionData$summary[[exercise]] <- FALSE
      
      subs <- studentSubmissions$exercises[[exercise]]
      submissionData[[exercise]][[student]] <- subs
      
      if (length(subs) > 0) {
        versions <- sort(names(subs), decreasing = TRUE)
        latest   <- subs[[versions[1]]]
        if (!is.null(latest) && latest$done == TRUE) {  
          submissionData$summary[student, exercise] <- TRUE
        }
      }

    }
  }
  
  invisible(submissionData)
}

#' TODO
#'
#' @export
#'
grade <- function() {
  
  checkInstructor()
  conf           <- loadConfig()
  submissionData <- updateData()
  solutionData   <- readRDS(file.path(conf$contentPath, "solutions.rds"))
  
  app <- shinyApp( ui     = gradeUI(conf, submissionData, solutionData)
                 , server = gradeServer(conf, submissionData, solutionData)
                 )
  
  runApp(app)
}

updateStudent <- function(username) {
  message("Updating ", username, "...")
  conf <- loadConfig()
  
  rdsPath <- file.path(conf$exercisePath, paste(username, "rds", sep = "."))
  if ( !file.exists(rdsPath) ) {
    studentData <- list( username   = username
                       , submitPath = file.path(studentPath(username), conf$submitPath)
                       , exercises  = list() # [[exercise]][[version]] = $done, $response, $htmlPath, $version depr: $html
                       )
  } else {
    studentData <- readRDS(rdsPath)
  }
  
  if (!dir.exists(studentData$submitPath)) {
    stop( "Submission path for student `"
        , username
        , "` does not exist:\n"
        , studentData$submitPath
        )
  }
  
  studentSubmissions <- checkSubmissions(username, studentData, rdsPath, conf$grades)
  saveRDS(studentSubmissions, rdsPath)
  Sys.chmod(rdsPath, mode = "0755")
  
  studentSubmissions
}

checkSubmissions <- function(student, studentData, rdsPath, grades) {
  
  exercises <- listExercises()
  
  for (exercise in exercises$file) {
    base <- fileBase(exercise)
    
    if (is.null(studentData$exercises[[base]])) { 
      studentData$exercises[[base]] <- list() 
    }
    
    latest <- latestVersion(base, studentData$submitPath)
    if (!is.na(latest) && is.null(studentData$exercises[[base]][[latest]])) {
      fileName   <- paste(base, latest, "html", sep = ".") 
      submission <- list( htmlPath = file.path(studentData$submitPath, fileName)  #html     = readFile(file.path(studentData$submitPath, fileName))
                        , response = NA
                        , done     = FALSE
                        , version  = latest
                        , student  = student
                        , exercise = base
                        , rdsPath  = rdsPath
                        )
      studentData$exercises[[base]][[latest]] <- submission
    }
    
  }
  
  studentData
}

checkInstructor <- function() { 
  conf <- loadConfig()
  if (!file.exists(conf$studentList)) {
    stop("You do not appear to be in an instructor project.")
  }
}
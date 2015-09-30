packageVersion <- 0.7

readFile  <- function(file) { paste(readLines(file, warn = FALSE), collapse = "\n") }
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

preBuild <- function(config, contentFiles, projectPath) {
  
  message("Prebuild")
  
  templateFile  <- file.path(projectPath, config$paths$templates, "grade-block.rmd")
  gradeTemplate <- readFile(templateFile)
  
  clearGraded <- function(x, options) {
    if(!is.null(options$grade) && options$grade) {
      gradeTemplate
    } else {
      x
    }
  }
  
  config$knitr <- list(knit_hooks = list(chunk = clearGraded))
  config
}

updatePackage <- function(config, manifest, projectPath) {
  # Paths needed by package functions
  
  config$projectPath    <- projectPath
  config$packageVersion <- packageVersion
  
  # Project
  config$contentPath  <- file.path(projectPath, config$paths$content)
  
  # Students
  config$homePath     <- config$package$homePath
  config$submitPath   <- config$package$submitPath
  
  # Instructor
  config$exercisePath <- file.path(projectPath, config$package$exercisePath)
  config$dataPath     <- file.path(config$exercisePath, config$package$dataPath)
  config$studentList  <- file.path(config$exercisePath, config$package$studentList)
  config$grades       <- config$package$grades
  
  config$instPath     <- file.path(projectPath, config$package$packagePath, "inst")
  if (!dir.exists(config$instPath)) { dir.create(config$instPath) }
  
  saveRDS( list(config = config, manifest = manifest, projectPath = projectPath)
         , file = file.path(config$instPath, "projectdata.rds")
         )
  
  message("Project data updated.")
  
  compileExercises(config, manifest, projectPath)
  message("Exercises updated.")
}

compileExercises <- function(config, manifest, projectPath) {
  
  if (!dir.exists(file.path(config$exercisePath, config$package$dataPath))) {
    file.symlink( file.path(config$contentPath,  config$package$dataPath)
                , file.path(config$exercisePath, config$package$dataPath)
                )
  }
  
  templateFile  <- file.path(projectPath, config$paths$templates, "grade-block.rmd")
  gradeTemplate <- readFile(templateFile)
  
  solDbPath <- file.path(config$contentPath, "solutions.rds")
  solDb     <- list()
  
  exDbPath  <- file.path(config$exercisePath, "exercises.rds")
  exDb      <- list()
  
  for (file in manifest[manifest$section == "Exercises", "file"]) {
    message("Compiling excercise: ", file)
    
    s <- readFile(file.path(config$contentPath, file))
    
    # Remove slide markup ##
    s <- gsub(pattern = "\n##\\s*\n", s, replacement = "")
    
    # Remove solutions
    l      <- strsplit(s, "\\n")[[1]]
    starts <- grep("```\\{r (.*?)grade(\\s*?)=(\\s*?)TRUE(.*?)\\}", l)
    ends   <- grep("```$", l)
    if (length(starts) > 0) {
      for (i in 1:(length(starts))) {
        start <- starts[i]
        end   <- ends[ends > start][1]
        repl  <- paste(l[start:end], collapse = "\n")
        s <- sub(repl, gradeTemplate, s, fixed = TRUE)
      }
    }
    
    # Save the file to exercises (student readable)
    cat(s, file = file.path(config$exercisePath, file))
    
    base   <- fileBase(file)
    fileDb <- compileSolutions(config, file)
    
    solDb[[base]] <- fileDb
    exDb[[base]]  <- names(fileDb)
    
  }
  
  saveRDS(solDb, solDbPath)
  saveRDS(exDb,  exDbPath )
  
}

compileSolutions <- function(config, file) {
  
  solution <- list()
  
  # run a mock knit to save solutions and graded exercises
  defOpts <- knitr::knit_hooks$get()
  
  saveSource <- function(x, options) {
    if (!is.null(options$grade) && options$grade) {
      s <- paste(x, collapse = "\n", sep = "\n")
      solution[[options$label]] <<- list( chunk = options$label
                                        , code  = s
                                        )
    }
    x
  }
  
  knitr::knit_hooks$set(source = saveSource)
  
  text <- readFile(file.path(config$contentPath, file))
  knitr::knit(text = text, quiet =  TRUE)
  
  knitr::knit_hooks$restore(defOpts)
  solution
   
}

loadData   <- function() {
  dataPath <- system.file("projectdata.rds", package = "bio297")
  data <- readRDS(dataPath)
  
  if (data$config$packageVersion > packageVersion) {
    message("The bio297 package needs to be updated.  Please restart your R session.")
  }
  
  data
}
loadConfig <- function() {
  data <- loadData()
  data$config
}
loadManifest <- function() {
  data <- loadData()
  data$manifest
}



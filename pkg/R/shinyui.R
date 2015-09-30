#' @import shiny 
#' @import shinydashboard
NULL

formatTabName <- function(exercise, student) { 
  paste("grade", exercise, student, sep = "-")
}

formatSubmissions <- function(conf, submissions, solutions) {
  students <- readLines(conf$studentList)
  sapply( names(submissions)
        , function(exercise) {
            ex <- list()
            for (student in students) {
              submission <- submissions[[exercise]][[student]]
              if (!is.null(submission) && (length(submission) > 0)) {
                versions   <- sort(names(submission), decreasing = TRUE)
                latest     <- submission[[versions[1]]]
                previous   <- submission[[versions[2]]]
                
                defaultResponses <- function(sub) {
                  if (is.null(sub)) { return(sub) }
                  if (is.na(sub$response)) { sub$response = list() }
                  #if (is.null(sub$exercise)) { print(sub) }
                  for (chunk in names(solutions[[sub$exercise]])) {
                    if (is.null(sub$response[[chunk]])) {
                      sub$response[[chunk]] <- list( grade   = conf$grades$default
                                                   , comment = ""
                                                   )
                    }
                  }
                  
                  sub
                }
                
                if (!is.null(latest) && latest$done == FALSE) {
                  ex[[student]] <- list( latest   = defaultResponses(latest)
                                       , previous = defaultResponses(previous)
                                       )
                }
              }
            }
            
            ex
          }
       )
}

splatExerciseMenu <- function(submissions) {
  sapply( names(submissions)
        , function(exercise) { splatStudentSubmenu(submissions, exercise) }
        , simplify = FALSE
        )
}

splatStudentSubmenu <- function(submissions, exercise) {
  subitems <- sapply( names(submissions[[exercise]])
                    , function(student) {
                        menuSubItem(student, tabName = formatTabName(exercise, student))
                      }
                    , simplify = FALSE
                    )
  do.call(menuItem, c(list(text = exercise, subitems)))
}

splatSubmissionTabs <- function(input, output, submissions, solutions, grades) {
  nestedTabs <- lapply( names(submissions)
                      , function(exercise) {
                          lapply( names(submissions[[exercise]])
                                , function(student) {
                                    page <- submissionPage( exercise
                                                          , student
                                                          , input
                                                          , output
                                                          , submissions[[exercise]][[student]]
                                                          , solutions[[exercise]]
                                                          , grades
                                                          )
                                    tabItem( tabName = formatTabName(exercise, student)
                                           , page
                                           )
                                  }
                                )
                          }
                        )
  do.call(c, nestedTabs)
}

formatSubmissionId <- function(exercise, student, version) {
  paste(exercise, student, version, sep = "-")
}

renderGradeUI <- function(response, grades, chunk, id) {
  grade   <- grades$values[[response$grade]]
  choices <- names(grades$values)
  names(choices) <- sapply( grades$values
                          , function(g) { g$text }
                          , simplify = FALSE
                          )
  
  fluidRow( id = chunk
          , box( title = chunk
               , radioButtons( id('grade')
                             , label    = "Grade"
                             , choices  = choices
                             , selected = response$grade
                             , inline   = TRUE
                             )
               , tags$textarea( id    = id('comment')
                              , label = "Comment"
                              , rows  = 3
                              , cols  = 80
                              , response$comment
                              )
               , status      = grade$status
               , solidHeader = TRUE
               , width       = 12
               , collapsible = TRUE
               )
          )
  
}

filterChunks <- function(response, gradeName) {
  if (is.null(names(response))) { return(list()) }
  
  response[ sapply( names(response)
                  , function(n) { response[[n]]$grade == gradeName }
                  )
          ]
}

checkDone   <- function(response, doneGrades) {
  isDone <- sapply( names(response)
                  , function(n) { response[[n]]$grade %in% doneGrades}
                  )
  all(isDone)
}

renderSubmission <- function(path, solution, func) {
  raw   <- readFile(path)
  body  <- regexpr(pattern = "<body>(.*)</body>", text = raw)
  start <- as.vector(body)
  n     <- attr(body, "match.length")
  s     <- substr(raw, start = start, stop = start + n)
  
  for (chunk in names(solution)) {
    target <- formatFeedbackChunk(chunk)
    ui     <- func(chunk)
    s      <- sub(target, ui, s, fixed = TRUE)
  }
  
  s
}

highlightjs <- "<script>
                  $('pre code').each(function(i, block) { 
                     hljs.highlightBlock(block); 
                  })
                </script>"

gradeHandlers <- function(title, input, output, data, solution, grades) { 
  
  if (!file.exists(data$htmlPath)) {
    return(p("The student has deleted this file."))
  }
  
  feedback <- do.call(reactiveValues, data)
  
  idFunc <- function(chunk) {
    function(inputName) { 
      paste( paste(feedback$exercise, feedback$student, feedback$version, chunk, sep = "-")
           , inputName
           , sep = "-"
           ) 
    }
  }
  
  doneGrades <- names(grades$values)[ sapply( names(grades$values)
                                            , function(n) { grades$values[[n]]$done }
                                            )
                                    ]
  observe({
    for (chunk in names(solution)) {
      id <- idFunc(chunk)
      if (!is.null(input[[id("grade")]])) {
        # isolate comment box
        isolate({
          grade <- grades$values[[ input[[id("grade")]] ]]
          if (grade$insertSolution && input[[id("comment")]] == "") {
            code <- solution[[chunk]]$code
            s <- paste( "Reference solution:\n"
                      , "```r"
                      , code
                      , "```"
                      , sep = "\n"
                      , collapse = "\n"
                      )
            feedback$response[[chunk]]$comment <- s
          } else {
            feedback$response[[chunk]]$comment <- input[[id("comment")]]
          }
        })
        
        # Trigger update
        feedback$response[[chunk]]$grade   <- input[[id("grade")]]
      }
    }
    
    feedback$done <- checkDone(feedback$response, doneGrades)
  })
  
  id     <- formatSubmissionId(feedback$exercise, feedback$student, feedback$version)
  saveId <- paste(id, "save", sep = "-")
  
  observeEvent(input[[saveId]], { 
    
    # update comments
    for (chunk in names(solution)) {
      isolate({
        feedback$response[[chunk]]$comment <- input[[idFunc(chunk)("comment")]]  
      })
    }
    
    rds <- readRDS(feedback$rdsPath)
    rds$exercises[[feedback$exercise]][[feedback$version]] <- reactiveValuesToList(feedback)
    saveRDS(rds, feedback$rdsPath)
    
    message("Saved update to: ", feedback$rdsPath)
  })
  
  output[[id]] <- renderUI({
    
    s <- renderSubmission( feedback$htmlPath
                         , solution
                         , function(chunk) {
                             renderGradeUI( feedback$response[[chunk]]
                                          , grades
                                          , chunk
                                          , idFunc(chunk)
                                          )
                           }
                         )
    
    width <- 12 / length(grades$values)
    gradeBar <- lapply( names(grades$values) 
                      , function(gradeName) {
                          chunks <- filterChunks(feedback$response, gradeName)
                          valueBox( value = length(chunks)
                                  , subtitle = grades$values[[gradeName]]$text
                                  , color    = grades$values[[gradeName]]$color
                                  , icon     = icon(grades$values[[gradeName]]$icon)
                                  , width    = width
                                  )
                        }
                      )
    
    box( title       = title
       , HTML(s)
       , fluidRow(gradeBar)
       , fluidRow( column( width = width  
                         , actionButton( saveId
                                       , "Save"
                                       , icon = icon("save")
                                       , class = "btn btn-lg"
                                       )
                         )
                 )
       , HTML(highlightjs)
       , width       = 12
       , solidHeader = TRUE
       , status      = if (feedback$done) { "success" } else { "warning" }
       )
    
  })
  
  uiOutput(id)
  
}

submissionBox <- function(title, input, output, data, solution, grades) {
  if (is.null(data)) {
    div()
  } else {
    fluidRow(gradeHandlers(title, input, output, data, solution, grades))
  }
}

submissionPage <- function(exercise, student, input, output, submission, solution, grades) {
  latest   <- submission$latest
  previous <- submission$previous
  
  tagList( submissionBox( paste("Latest:", latest$version)
                        , input
                        , output
                        , latest
                        , solution
                        , grades
                        )
         , submissionBox( paste("Previous:", previous$version)
                        , input
                        , output
                        , previous
                        , solution
                        , grades
                        )
         )
}

formatNotice <- function(gradeName) {
  paste(gradeName, "notice", sep = "-", collapse = "-")
}

gradeUI <- function(conf, submissionData, solutionData) {
  resourcePath <- file.path(conf$projectPath, conf$paths$app)
  jsPath  <- file.path(resourcePath, "js", "highlight.min.js")
  cssPath <- file.path(resourcePath, "css", "highlight.css")
  
  ui <- dashboardPage( header  = dashboardHeader(title = "Instructor view")
                     , sidebar = dashboardSidebar( sidebarMenuOutput('sidebar')
                                                 , uiOutput('heartbeat')
                                                 )
                     , body    = dashboardBody( tags$head(includeScript(jsPath))
                                              , tags$head(includeCSS(cssPath))
                                              , uiOutput("body")
                                              )
                     , skin    = "purple"
                     )
  
  ui
}

gradeServer <- function(conf, submissionData, solutionData) {
  function(input, output, session) {
    
    solutions   <- solutionData
    submissions <- formatSubmissions(conf, submissionData, solutionData)
    
    output$sidebar <- renderMenu({
      exerciseMenus <- splatExerciseMenu(submissions)
      sidebarMenu( id    = "tabs"
                 , .list = c( list(menuItem("Overview", tabName = "overview"))
                            , exerciseMenus
                            )
                 )
    })
    
    output$body <- renderUI({
      items <- splatSubmissionTabs(input, output, submissions, solutions, grades = conf$grades)
      tabs  <- do.call( tabItems
                      , c( list(tabItem( tabName = "overview"
                                       , fluidRow(dataTableOutput("summary"))
                                       )
                               )
                         , items
                         )
                      )
      
      tabs
    })
    
    output$summary <- renderDataTable({
      submissionData$summary
    })
    
    # Hack: for some reason app disconnects after 60s of inactivity when
    # launched in Rstudio server
    output$heartbeat <- renderUI({
      invalidateLater(58 * 1000, session)
      p(Sys.time(), style = "visibility: hidden;")
    })
    
  }
}

splatFeedbackMenu <- function(conf, studentData) {
  menus <- lapply( names(studentData$exercises) 
                 , function(exercise) {
                     entry <- menuItem( text    = exercise
                                      , tabName = exercise
                                      )
                     
                     subs  <- studentData$exercises[[exercise]]
                     if (length(subs) > 0) {
                       versions <- sort(names(subs), decreasing = TRUE)
                       if (length(versions) > 0 && subs[[versions[1]]]$done) {
                         entry <- menuItem( text       = exercise
                                          , tabName    = exercise
                                          , badgeLabel = "done"
                                          , badgeColor = "green"
                                          )
                       }
                     }
                     
                     entry
                   }
                 )
  
  sidebarMenu(.list = menus)
}

splatFeedbackTabs <- function(conf, studentData, output) {
 
  tabs <- lapply( names(studentData$exercises)
                , function(exercise) {
                    tabItem( tabName = exercise
                           , feedbackPage(exercise, conf, studentData, output)
                           )
                  }
                )
  
  do.call(tabItems, tabs) 
}

feedbackPage <- function(exercise, conf, studentData, output) {
  subs     <- studentData$exercises[[exercise]]
  
  if (length(subs) < 1) {
    return( tagList( p("No submissions have been received for this exercise yet.  If you've submitted one just check back later.")
                   , p("You can also check on the status of your submissions with `bio297::status()`.")
                   )
          )
  }
  
  versions <- sort(names(subs), decreasing = TRUE)
  boxes <- lapply( versions
                 , function(version) {
                     submission <- subs[[version]]
                     if (length(submission$response) < 1 || is.na(submission$response)) {
                       fluidRow( box( title  = submission$version
                                    , status      = "info"
                                    , width       = 12
                                    , solidHeader = TRUE
                                    , collapsible = TRUE
                                    , p("There is no feedback for this version.")
                                    )
                               )
                     } else {
                       fluidRow( feedbackBox(submission, conf, output) )
                     }
                     
                   }
                 )
  
  tagList(boxes)
}


renderFeedbackUI <- function(response, grades, output) {
  
  grade <- grades$values[[response$grade]]
  
  s <- markdown::markdownToHTML( text          = response$comment
                               , fragment.only = TRUE
                               )
  fluidRow( box( title = paste( grade$text
                              , if (grade$done) { "(done)" }
                              )
               , status      = grade$status
               , solidHeader = TRUE
               , width       = 12
               , collapsible = TRUE
               , HTML(s)
               )
          )
}

feedbackBox <- function(submission, conf, output) {
  
  s <- readFile(submission$htmlPath)
  for (chunk in names(submission$response)) {
    target <- formatFeedbackChunk(chunk)
    ui     <- renderFeedbackUI( submission$response[[chunk]]
                              , conf$grades
                              , output
                              )
    s      <- sub(target, ui, s, fixed = TRUE)
  }
  
  grades    <- conf$grades
  width     <- 12 / length(grades$values)
  statusBar <- lapply( names(grades$values) 
                     , function(gradeName) {
                         chunks <- filterChunks(submission$response, gradeName)
                         valueBox( value    = length(chunks)
                                 , subtitle = grades$values[[gradeName]]$text
                                 , color    = grades$values[[gradeName]]$color
                                 , icon     = icon(grades$values[[gradeName]]$icon)
                                 , width    = width
                                 )
                     }
  )
  
  box( title       = submission$version
     , fluidRow(statusBar)
     , HTML(s)
     , HTML(highlightjs)
     , width       = 12
     , solidHeader = TRUE
     , collapsible = TRUE
     , status      = if (submission$done) { "success" } else { "warning" }
     )
}

feedbackUI <- function(conf, studentData, skin = "purple") {
  resourcePath <- file.path(conf$projectPath, conf$paths$app)
  jsPath       <- file.path(resourcePath, "js", "highlight.min.js")
  cssPath      <- file.path(resourcePath, "css", "highlight.css")
  
  dashboardPage( header  = dashboardHeader(title = studentData$username)
               , sidebar = dashboardSidebar( splatFeedbackMenu(conf, studentData)
                                           , uiOutput('heartbeat')
                                           )
               , body    = dashboardBody( tags$head(includeScript(jsPath))
                                        , tags$head(includeCSS(cssPath))
                                        , uiOutput("body")
                                        )
               , skin    = "purple"
               )
}

feedbackServer <- function(conf, studentData) {
  function(input, output, session) {
    
    output$body <- renderUI({
      splatFeedbackTabs(conf, studentData, output)
    })
    
    output$heartbeat <- renderUI({
      invalidateLater(58 * 1000, session)
      p(Sys.time(), style = "visibility: hidden;")
    })
    
  }
}



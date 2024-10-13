#' To record the time
#'
#' @description
#' To record the time, often for logging purposes
#' @param status, a string vector of length 1 to label the task type
#' @param new_line, whether to add a new line on the console. 
#' @details
#' status can be 
#' \enumerate{
#'   \item new, to start a new task and reclock
#'   \item continue, to continue an existing task
#'   \item off, to end the task and turn off the timer
#' }
#' 

#' @return Nothing. Only show the time elapsed in the console.
#' @export
timeit <- function(status='continue', new_line = TRUE) {
    cur_time <- Sys.time()
    new_line_str <- ""
    if(new_line) {
        new_line_str <- "\n"
    }
    if(status == 'new') {
            cat(new_line_str, "Timer On: ", as.character(cur_time), new_line_str)
        assign(".GLOBAL_TIMESTAMP", cur_time, envir = .GlobalEnv)
        assign(".GLOBAL_TIMESTAMP_BEGIN", cur_time, envir = .GlobalEnv)
    } else if(status == 'continue') {
        if(exists(".GLOBAL_TIMESTAMP", envir = .GlobalEnv) &&
           exists(".GLOBAL_TIMESTAMP_BEGIN", envir = .GlobalEnv)) {
            GLOBAL_TIMESTAMP <- get('.GLOBAL_TIMESTAMP', envir = .GlobalEnv)
            GLOBAL_TIMESTAMP_BEGIN <- get('.GLOBAL_TIMESTAMP_BEGIN', envir = .GlobalEnv)
            cat(new_line_str,
                "[Time Ellapsed: ",
                format(difftime(cur_time, GLOBAL_TIMESTAMP), digits=2),
                " of ",
                format(difftime(cur_time, GLOBAL_TIMESTAMP_BEGIN), digits=2),
                " @", as.character(cur_time),
                "]", new_line_str, sep = '')
            assign(".GLOBAL_TIMESTAMP", cur_time, envir = .GlobalEnv)
        } else {
            cat(new_line_str, 
                "Timer On: ", 
                as.character(cur_time), 
                new_line_str)
            assign(".GLOBAL_TIMESTAMP", cur_time, envir = .GlobalEnv)
            assign(".GLOBAL_TIMESTAMP_BEGIN", cur_time, envir = .GlobalEnv)
        }
    } else if(status == 'off') {
        if(exists(".GLOBAL_TIMESTAMP", envir = .GlobalEnv)) {
            rm(".GLOBAL_TIMESTAMP", envir = .GlobalEnv)
        }
        if(exists(".GLOBAL_TIMESTAMP_BEGIN", envir = .GlobalEnv)) {
            rm(".GLOBAL_TIMESTAMP_BEGIN", envir = .GlobalEnv)
        }
        return(new_line_str, "Timer Off", new_line_str)
    } else {
        if(exists(".GLOBAL_TIMESTAMP", envir = .GlobalEnv) &&
           exists(".GLOBAL_TIMESTAMP_BEGIN", envir = .GlobalEnv)) {
            GLOBAL_TIMESTAMP <- get('.GLOBAL_TIMESTAMP', envir = .GlobalEnv)
            GLOBAL_TIMESTAMP_BEGIN <- get('.GLOBAL_TIMESTAMP_BEGIN', envir = .GlobalEnv)
            cat(new_line_str, status, " finished [Time Ellapsed: ",
                format(difftime(cur_time, GLOBAL_TIMESTAMP), digits=2),
                " of ",
                format(difftime(cur_time, GLOBAL_TIMESTAMP_BEGIN), digits=2),
                " @", as.character(cur_time),
                "]", new_line_str, sep = '')
            assign(".GLOBAL_TIMESTAMP", cur_time, envir = .GlobalEnv)
        } else {
            cat(new_line_str, "Timer On: ", as.character(cur_time), new_line_str)
            assign(".GLOBAL_TIMESTAMP", cur_time, envir = .GlobalEnv)
            assign(".GLOBAL_TIMESTAMP_BEGIN", cur_time, envir = .GlobalEnv)
        }
    }
}

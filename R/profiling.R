#' @export
journal <- function(x, ...) UseMethod("journal")

#' @export
journal.Future <- function(x, ...) {
  data <- x$.journal

  result <- x$result
  if (!is.null(result)) {
    t <- data.frame(step = "evaluation_begin", time = result$started)
    data <- rbind(data, t)
    t <- data.frame(step = "evaluation_end", time = result$finished)
    data <- rbind(data, t)
  }
  
  data
}


#' @export
journal_append <- function(x, ...) UseMethod("journal_append")

#' @export
journal_append.Future <- function(x, step, time = Sys.time()) {
  data <- data.frame(step = step, time = time)
  x$.journal <- rbind(x$.journal, data)
  invisible(x)
}


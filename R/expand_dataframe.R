#' Expand a dataframe, by splitting a text field
#' @param data A dataframe
#' @param column Which column to use as a basis for splitting
#' @param sep The separator in the column, separating the fields
#' @examples
#' data <- data.frame(group = c("A","B"), addresses = c("one;two;three","four;five"))
#' expand_dataframe_split(data, "addresses", sep = ";")
#' @export
expand_dataframe_split <- function(data, column = "label", sep = ";"){

  labels <- function(x){
    str_trim(strsplit(as.character(x), sep)[[1]])
  }

  nlabels <- function(x){
    length(labels(x))
  }

  l <- lapply(1:nrow(data), function(i){
    if(nlabels(data[i, column]) <=  1){
      return(data[i,])
    } else {
      labs <- labels(data[i,column])
      out <- replicate(length(labs), data[i,], simplify = FALSE)

      out <- do.call(rbind, out)

      out[,column] <- labs
      return(out)
    }
  })
  as.data.frame(do.call(rbind, l))

}



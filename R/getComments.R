#' Get comments below a video
#'
#'
#'
#'
#' @param Vid Video ID
#' @param api_key Your API key
#' @export
#' @description
#'
#' @return That'll give you all the comments with their IDs ... you can take those IDs, then, and run them through the comments->list endpoint to get more granular info.

getComments <- function(Vid,api_key){
  url=paste0("https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&videoId=",Vid,
             "&key=",api_key)
  tmp.data <- readLines(url, warn="F",encoding = "UTF-8")
  tmp  <- fromJSON(tmp.data)
  comments <- tmp$items$snippet
  return(comments)
}



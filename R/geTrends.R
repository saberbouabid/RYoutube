#' Getting trends from youtube using YouTube API
#'
#' For most popular video
#' @param part The part parameter specifies a comma-separated list of one or more video resource properties that the API response will include.
#' If the parameter identifies a property that contains child properties, the child properties will be included in the response.
#' For example, in a video resource, the snippet property contains the channelId, title, description, tags, and categoryId properties.
#' As such, if you set part=snippet, the API response will contain all of those properties.
#' @param chart :The chart parameter identifies the chart that you want to retrieve. (string) like 'mostPopular'
#' @param regionCode : string
#' The regionCode parameter instructs the API to select a video chart available in the specified region. This parameter can only be used
#' in conjunction with the chart parameter. The parameter value is an ISO 3166-1 alpha-2 country code https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#Current_codes
#' @param maxResults unsigned integer . The maxResults parameter specifies the maximum number of items that should be returned in the result set.
#' Note: This parameter is supported for use in conjunction with the myRating parameter, but it is not supported for use in conjunction with the id parameter.
#' Acceptable values are 1 to 50, inclusive. The default value is 5.
#' @param api_key : Google API key
#' @description Extract youtube trends of most popular or most viewed by country .
#' @return Data frame
#' @export





geTrends<-function(chart="mostPopular",regionCode,api_key,maxResults=5){
  url<-paste0("https://www.googleapis.com/youtube/v3/videos?part=snippet,contentDetails,statistics&chart=",chart,"&maxResults=",maxResults,"&regionCode=",regionCode,"&key=",api_key)
  tmp.data <- readLines(url, warn="F",encoding = "UTF-8")
  tmp  <- jsonlite::fromJSON(tmp.data)
  id<-tmp$items$id
  statistics<-getStatistics(tmp)
  contentDetails<-getContentDetails(tmp)
  snippet<-getSnippet(tmp)
  df<-as.data.frame(cbind(id,snippet,contentDetails,statistics))
  return(df)
}

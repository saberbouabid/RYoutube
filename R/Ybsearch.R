#' Make a query on Youtube using it's API
#'
#'
#' @param chid  The \code{chid} parameter indicates that the API response should only contain resources created by the channel.
#' Note: Search results are constrained to a maximum of 500 videos if your request specifies a value for the channelId parameter
#'  and sets the \code{type} parameter value to \code{video}, but it does not also set one of the \code{forContentOwner}, \code{forDeveloper}, or \code{forMine} filters.
#' @param api_key  Users api keys please check \href{google console}{https://console.developers.google.com}
#' @param CType string The channelType parameter lets you restrict a search to a particular type of channel.
#' @param location 	string  The location parameter, in conjunction with the locationRadius parameter, defines a circular geographic area and also restricts a search to videos that specify, in their metadata, a geographic location that falls within that area. The parameter value is a string that specifies latitude/longitude coordinates e.g. (37.42307,-122.08427).
#' The location parameter value identifies the point at the center of the area.
#' The \code{Radius} parameter specifies the maximum distance that the location associated with a video can be from that point for the video to still be included in the search results.
#' The API returns an error if your request specifies a value for the location parameter but does not also specify a value for the locationRadius parameter.
#' @param Radius 	string The Radius parameter, in conjunction with the location parameter, defines a circular geographic area.
#' The parameter value must be a floating point number followed by a measurement unit. Valid measurement units are m, km, ft, and mi. For example, valid parameter values include 1500m, 5km, 10000ft, and 0.75mi. The API does not support locationRadius parameter values larger than 1000 kilometers.
#' @param maxR	unsigned integer  The maxResults parameter specifies the maximum number of items that should be returned in the result set. Acceptable values are 0 to 50, inclusive. The default value is 5.
#' @param order string
#' The order parameter specifies the method that will be used to order resources in the API response. The default value is relevance.
#' Acceptable values are:
#'
#'   \code{date} , Resources are sorted in reverse chronological order based on the date they were created\cr
#'   \code{rating} , Resources are sorted from highest to lowest rating.\cr
#'   \code{relevance}, Resources are sorted based on their relevance to the search query. This is the default value for this parameter.\cr
#'   \code{title} Resources are sorted alphabetically by title.\cr
#'   \code{videoCount} , Channels are sorted in descending order of their number of uploaded videos.\cr
#'   \code{viewCount} , Resources are sorted from highest to lowest number of views. For live broadcasts, videos are sorted by number of concurrent viewers while the broadcasts are ongoing.
#' @param after datetime The publishedAfter parameter indicates that the API response should only contain resources created after the specified time. The value is an RFC 3339 formatted date-time value (1970-01-01T00:00:00Z)
#' @param before 	datetime The publishedBefore parameter indicates that the API response should only contain resources created before the specified time. The value is an RFC 3339 formatted date-time value (1970-01-01T00:00:00Z).
#' @param regionCode 	string The regionCode parameter instructs the API to return search results for the specified country. The parameter value is an ISO 3166-1 alpha-2 country code.
#' @param safe 	string The safeSearch parameter indicates whether the search results should include restricted content as well as standard content.
#'  Acceptable values are:
#'  \code{moderate} , YouTube will filter some content from search results and, at the least, will filter content that is restricted in your locale. Based on their content, search results could be removed from search results or demoted in search results. This is the default parameter value.\cr
#'  \code{none} , YouTube will not filter the search result set.\cr
#'  \code{strict} , YouTube will try to exclude all restricted content from the search result set.\cr
#'  Based on their content, search results could be removed from search results or demoted in search results.
#' @param type 	string The type parameter restricts a search query to only retrieve a particular type of resource.
#' The value is a comma-separated list of resource types. Acceptable values are ;\code{channel},\code{playlist} or \code{video} , The default value all of them.
#' @param VType 	string The videoType parameter lets you restrict a search to a particular type of videos. If you specify a value for this parameter, you must also set the type parameter's value to video.
#'  Acceptable values are:
#'  \code{any} ,Return all videos.\cr
#'  \code{episode} ,Only retrieve episodes of shows.\cr
#' @import  jsonlite
#' @export
#' @examples
#' My_api_key<-"AIzaSyCudPERm98VkhkZUkS71QIkqycWVeZeltM"
#' mysearch<-Ybsearch(chid=)
#' @description Returns a collection of search results that match the query parameters specified in the API request.
#' By default, a search result set identifies matching video, channel, and playlist resources, but you can also configure queries to only retrieve a specific type of resource.
#' @return Data frame that contient all informations about your query .
#'


Ybsearch <- function(chid=NULL,
                     safe=NULL,
                     type=NULL,
                     Vtype=NULL,
                      order =NULL,CType=NULL,after=NULL,
                      before=NULL,location=NULL,Radius=NULL,
                      regionCode=NULL,maxR=5,api_key)
  {
# building url request
  url.base=paste0("https://www.googleapis.com/youtube/v3/search?&part=snippet")
  url.chid<-makeurl(chid,"&id=")
  url.user<-makeurl(username,"&forUsername=")
  url.Ctype<-makeurl(Ctype,"&channelType=",possiblevalues = c(1:100))
  url.safe<-makeurl(safeSearch,"&safeSearch=",possiblevalues = c("moderate" ,"strict" ,"none"))
  url.order<-makeurl(order,"&order=",possiblevalues = c("rating","relevance","date","title","videoCount","viewCount"))
  url.type<-makeurl(type,"&type=",possiblevalues = c("channel","playlist","video"))
  url.Vtype<-makeurl(VType,"&videoType=")
  url.Ttype<-makeurl(CType,"&channelType")
  url.after<-makeurl(after,"&publishedAfter=")
  url.before<-makeurl(before,"&publishedBefore=")
  url.location<-makeurl(location,"&location=")
  url.locationR<-makeurl(Radius,"&locationRadius=")
  url.regionCode<-makeurl(regionCode,"&regionCode=")
  url.max<-makeurl(maxR,"&maxResults=")
  url.key<-makeurl(api_key,"&key=")

# merging all suburls
  url<-paste0(url.base,
              url.chid,
              url.Ctype,
              url.order,
              url.safe,
              url.type,
              url.Vtype,
              url.Ttype,
              url.after,
              url.before,
              url.location,
              url.locationR,
              url.regionCode,
              url.max,
              url.key)


  #submiting url and scrapping returned Json table

  raw.data <- readLines(url, warn="F",encoding = "UTF-8")
  tmp  <- jsonlite::fromJSON(raw.data)
  channelId<-tmp$items$snippet$channelId
  Kind <- gsub("youtube#","", tmp$items$id$kind)
  videoId <- tmp$items$id$videoId
  snippet<-getSnippet(tmp)
  df<-as.data.frame(cbind(channelId,Kind,videoId,snippet))
  return(df)
}

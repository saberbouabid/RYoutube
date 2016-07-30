#' Extract All Statistics about a Youtube video
#'
#' Using YouTube API to get all possible information about a video posted in Youtube including statistics
#'
#' @param part 	string  The part parameter specifies a comma-separated list of one or more video resource properties that the API response will include.
#' If the parameter identifies a property that contains child properties, the child properties will be included in the response.
#' @param mostPopular 	Boolean The chart parameter identifies the chart that you want to retrieve.
#' Acceptable values are:  "mostPopular". Return the most popular videos for the specified content region and video category.
#' @param  vid 	string
#' The vid parameter specifies a comma-separated list of the YouTube video ID(s) for the resource(s) that are being retrieved. In a video resource, the id property specifies the video's ID.
#' @param myRating 	string This parameter can only be used in a properly authorized request. Set this parameter's value to like or dislike to instruct the API to only return videos liked or disliked by the authenticated user.
#' Acceptable values are
#'   dislike , Returns only videos disliked by the authenticated user.
#'      like , Returns only video liked by the authenticated user.
#' @param hl 	string
#' The hl parameter instructs the API to retrieve localized resource metadata for a specific application language that the YouTube website supports. The parameter value must be a language code included in the list returned by the i18nLanguages.list method.
#' If localized resource details are available in that language, the resource's snippet.localized object will contain the localized values. However, if localized details are not available, the snippet.localized object will contain resource details in the resource's default language.
#' @param maxResults 	unsigned integer
#' The maxResults parameter specifies the maximum number of items that should be returned in the result set.
#' Note: This parameter is supported for use in conjunction with the myRating parameter, but it is not supported for use in conjunction with the id parameter. Acceptable values are 1 to 50, inclusive. The default value is 5.
#' @param OBCOwner 	string
#' This parameter can only be used in a properly authorized request. Note: This parameter is intended exclusively for YouTube content partners.
#' The onBehalfOfContentOwner parameter indicates that the request's authorization credentials identify a YouTube CMS user who is acting on behalf of the content owner specified in the parameter value. This parameter is intended for YouTube content partners that own and manage many different YouTube channels. It allows content owners to authenticate once and get access to all their video and channel data, without having to provide authentication credentials for each individual channel. The CMS account that the user authenticates with must be linked to the specified YouTube content owner.
#' @param pageToken 	string
#' The pageToken parameter identifies a specific page in the result set that should be returned. In an API response, the nextPageToken and prevPageToken properties identify other pages that could be retrieved.
#' Note: This parameter is supported for use in conjunction with the myRating parameter, but it is not supported for use in conjunction with the id parameter.
#' @param regionCode 	string
#' The regionCode parameter instructs the API to select a video chart available in the specified region. This parameter can only be used in conjunction with the chart parameter. The parameter value is an ISO 3166-1 alpha-2 country code.
#' @param videoCategoryId 	string
#' The videoCategoryId parameter identifies the video category for which the chart should be retrieved. This parameter can only be used in conjunction with the chart parameter. By default, charts are not restricted to a particular category. The default value is 0.

#' @param  api_key  users api keys please check https://console.developers.google.com
#' @export
#' @return  Data frame combining all collected informations .
#' @description Function that can extract essentiel information about plubic video posted in youtube

#'


getVideos <- function(vid=NULL,part="snippet",mostPopular=FALSE,myRating=FALSE,maxResults=5,
                      pageToken=NULL,regionCode=NULL,videoCategoryId=NULL,
                      api_key){
  require(jsonlite)

  url.base=paste0("https://www.googleapis.com/youtube/v3/videos?")

  if(!(is.null(vid)==TRUE)){
    url.id<-paste0("&id=",vid)
  }
  if(!(is.null(api_key)==TRUE)){
    url<-paste0()
  }
  if(!(is.null(part)==TRUE)){
   # snippet,contentDetails,statistics,status
    url.part<-paste0("&part=",part)
  }
  if(!(is.null()==TRUE)){
    url.<-paste0()
  }
  if(mostPopular == TRUE){
    url.or<-paste0("&order=mostPopular")
  }
  if(myRating==TRUE){
    url.mr<-paste0("&myrating=TRUE")
  }
  if(!(is.null(maxResults)==TRUE)){
    url.max<-paste0("&maxResults=",maxResults)
  }
  if(!(is.null(oBCOwner)==TRUE)){
    url.obc<-paste0("&onBehalfOfContentOwner=",oBCOwner)
    }
  if(!(is.null(pageToken)==TRUE)){
    url.pt<-paste0("&pageToken=",pageToken)}

  if(!(is.null(regionCode)==TRUE)){
    url.rgc<-paste0("&regionCode=",regionCode)
  }
  if(!(is.null(videoCategoryId)==TRUE)){
    url.vcid<-paste0("&videoCategoryId=",videoCategoryId)
  }

  url<-paste0(url.base,"&key=",api_key)
  tmp.data <- readLines(url, warn="F",encoding = "UTF-8")
  tmp  <- jsonlite::fromJSON(tmp.data)
  VideoId <- tmp$items$id
  snippet<-getSnippet(tmp)
  contentDetails <- getContentDetails(tmp)
  statistics <- getStatistics(tmp)
  df<-as.data.frame(cbind(Vid,snippet,contentDetails,statistics))
  return(df)
}



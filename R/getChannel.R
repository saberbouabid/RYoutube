#' Returns a collection of zero or more channel resources that match the request criteria
#'
#'
#' @param part	string
#' The part parameter specifies a comma-separated list of one or more channel resource properties that the API response will include.
#'
#' Filters (specify exactly one of the following parameters) :
#'
#' @param categoryId 	string
#' The categoryId parameter specifies a YouTube guide category, thereby requesting YouTube channels associated with that category.
#' @param username string The forUsername parameter specifies a YouTube username, thereby requesting the channel associated with that username.
#' @param chid 	string The id parameter specifies a comma-separated list of the YouTube channel ID(s) for the resource(s) that are being retrieved. In a channel resource, the id property specifies the channel's YouTube channel ID.
#' @param managedByMe 	boolean this parameter can only be used in a properly authorized request. Note: This parameter is intended exclusively for YouTube content partners.
#' Set this parameter's value to true to instruct the API to only return channels managed by the content owner that the onBehalfOfContentOwner parameter specifies. The user must be authenticated as a CMS account linked to the specified content owner and onBehalfOfContentOwner must be provided.
#' @param mine 	boolean This parameter can only be used in a properly authorized request. Set this parameter's value to true to instruct the API to only return channels owned by the authenticated user.
#'
#' **Optional parameters :
#'
#' @param hl 	string The hl parameter instructs the API to retrieve localized resource metadata for a specific application language that the YouTube website supports. The parameter value must be a language code included in the list returned by the i18nLanguages.list method.
#' If localized resource details are available in that language, the resource's snippet.localized object will contain the localized values. However, if localized details are not available, the snippet.localized object will contain resource details in the resource's default language.
#' @param maxResults 	unsigned integer The maxResults parameter specifies the maximum number of items that should be returned in the result set. Acceptable values are 0 to 50, inclusive. The default value is 5.
#' @param OBCOwner  (onBehalfOfContentOwner) 	string This parameter can only be used in a properly authorized request. Note: This parameter is intended exclusively for YouTube content partners.
#'  The onBehalfOfContentOwner parameter indicates that the request's authorization credentials identify a YouTube CMS user who is acting on behalf of the content owner specified in the parameter value. This parameter is intended for YouTube content partners that own and manage many different YouTube channels. It allows content owners to authenticate once and get access to all their video and channel data, without having to provide authentication credentials for each individual channel. The CMS account that the user authenticates with must be linked to the specified YouTube content owner.
#' @param pageToken 	string
#'  The pageToken parameter identifies a specific page in the result set that should be returned. In an API response, the nextPageToken and prevPageToken properties identify other pages that could be retrieved.
#' @export
#' @import  jsonlite
#' @description Function that can fetch all Youtube channel's information .
#' @return Information about Youtube channel
#' @examples  \dontrun{
#' My_api_key<-"AIzaSyCudPERm98VkhkZUkS71QIkqycWVeZeltM"
#' BBJkaly<-"UCspYHrPHi-FhJZX9QXGJ-2g"
#' info_channel<-getChannel(chid= BBJkaly,api_key = My_api_key)
#' }



getChannel<- function(part=NULL,
                      chid =NULL,
                      username=NULL,
                      managedByMepage=FALSE,
                      mine=FALSE,
                      hl=NULL,
                      maxResults=5,
                      OBCOwner= NULL,
                      pageToken=NULL,
                      api_key){

  url.base<-paste0("https://www.googleapis.com/youtube/v3/channels?part=snippet,contentDetails,statistics,status")

  url.chid<-makeurl(chid,"&id=")
  url.user<-makeurl(username,"&forUsername=")
  url.key<-makeurl(api_key,"&key=")


url<-paste0(url.base,url.chid,url.user,url.key)

raw.data <- readLines(url, warn="F",encoding = "UTF-8")
tmp  <- jsonlite::fromJSON(raw.data)
Channelid <- tmp$items$id
contentDetails <- getContentDetails(tmp,channel = TRUE)
snippet<-getSnippet(tmp )
statistics<-getStatistics(tmp)
status<-tmp$items$status
df<-as.data.frame(cbind(Channelid,contentDetails,snippet,statistics,status))
return(df)
}


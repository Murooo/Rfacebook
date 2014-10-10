#' @rdname getPage
#' @export
#'
#' @title 
#' Extract list of posts from a public Facebook page
#'
#' @description
#' \code{getPage} retrieves information from a public Facebook page. Note that
#' information about users that have turned on the "follow" option on their 
#' profile can also be retrieved with this function.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getUsers}}, \code{\link{getPost}}, \code{\link{fbOAuth}}
#'
#' @param page A page ID or page name.
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Number of posts of page to return. Note that number can be sometimes
#' higher or lower, depending on status of API.
#'
#' @param feed If \code{TRUE}, the function will also return posts on the page
#' that were made by others (not only the admin of the page).
#'
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'	load("fb_oauth")
#'	fb_page <- getPage(page="facebook", token=fb_oauth)
#' ## Getting posts on Humans of New York page, including posts by others users
#' ## (not only owner of page)
#'  page <- getPage(page="humansofnewyork", token=fb_oauth, feed=TRUE)
#' }
#'

	df <- pageDataToDF(content$data)
			df.list <- c(df.list, list(pageDataToDF(content$data)))
getInsights <- function(object_id, token, metric, period='day', n=5){
  url <- paste0('https://graph.facebook.com/', object_id,
                '/insights/', metric, '?period=', period)
  
  # making query
  content <- callAPI(url=url, token=token)
  l <- length(content$data[[1]]$values)
  if (l==0){ 
    stop("No public posts mentioning the string were found")
  }
  
  ## retrying 3 times if error was found
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    print(url)
    content <- callAPI(url=url, token=token)		
    if (error==3){ stop(content$error_msg) }
  }
  if (length(content$data)==0){ 
    stop("No public posts mentioning the string were found")
  }
  
  if (n>nrow(df)){
    df.list <- list(df)
    while (l<n & l>0 & 
             !is.null(content$paging$`next`)&
             period != 'lifetime'){
      # waiting one second before making next API call...
      Sys.sleep(0.5)
      url <- content$paging$`next`
      content <- callAPI(url=url, token=token)
      l <- l + nrow(df)
      
      ## retrying 3 times if error was found
      error <- 0
      while (length(content$error_code)>0){
        cat("Error!\n")
        Sys.sleep(0.5)
        error <- error + 1
        content <- callAPI(url=url, token=token)		
        if (error==3){ stop(content$error_msg) }
      }
      
    }
    df <- do.call(rbind, df.list)
  }
  cat(l, "objects found ")
  return(df)
}

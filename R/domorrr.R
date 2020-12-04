
# much of this is borrowed from DomoR
# add empty .domo_env as parent
.domorrr_env <- new.env()

.onLoad <- function(libname, pgkname) {
  packageStartupMessage('Welcome to domorrr, a work-in-progress, proceeed with caution!')
}


#' Get Access Token
#'
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @return
#' @export
#'
#' @examples
#'
init <- function() {
# getAccessToken <- function() {
  client_id <- Sys.getenv('DOMO_CLIENT_ID')
  client_secret <- Sys.getenv('DOMO_CLIENT_SECRET')
  scope <- Sys.getenv('DOMO_SCOPE')

  resp <- httr::GET('https://api.domo.com/oauth/token',
              httr::authenticate(user = client_id, password = client_secret),
              query = list(grant_type = 'client_credentials', scope = scope))

  httr::stop_for_status(resp)
  access_token <- httr::content(resp)$access_token
  .domorrr_env$access_token <- access_token
}



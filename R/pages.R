


listPages <- function(limit = 50, offset = 0) {
  args <- list(limit = limit, offset = offset)
  resp <- httr::GET(
    'https://api.domo.com/v1/pages',
    httr::add_headers('Accept' = 'application/json',
                      "Authorization" = paste('Bearer', gsub('\n', '', domorrr:::.domorrr_env$access_token))),
    query = purrr::compact(args)
  )
  httr::stop_for_status(resp)
}


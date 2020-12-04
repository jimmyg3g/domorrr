#' List Datasets
#'
#' List the datasets available to you in Domo.
#'
#' @seealso \url{https://developer.domo.com/docs/dataset-api-reference/dataset}
#'
#' @param limit Maximum number of entries to return
#' @param offset Starting index into the result
#' @param sort Sort the results column, e.g. 'name'
#'
#' @return A \code{data.frame} containing the available datasets
#' @export
#'
#' @examples
listDatasets <- function(limit = 50, offset = 0, sort = NULL) {
  args <- list(limit = limit, offset = offset, sort = sort)
  resp <- httr::GET(
    'https://api.domo.com/v1/datasets',
    httr::add_headers('Accept' = 'application/json',
                "Authorization" = paste('Bearer', gsub('\n', '', domorrr:::.domorrr_env$access_token))),
    query = purrr::compact(args)
  )
  httr::stop_for_status(resp)

  resp %>% httr::content('text') %>% jsonlite::fromJSON(flatten = TRUE) %>% dplyr::tbl_df() %>% janitor::clean_names()
}


#' Get Dataset Fields
#'
#' Get the fields of a dataset by dataset ID
#'
#' @seealso \url{https://developer.domo.com/docs/dataset-api-reference/dataset}
#'
#' @param dataset_id Dataset ID to query
#'
#' @return A \code{data.frame} containing the dataset metadata
#' @export
#'
#' @examples
getDatasetFields <- function(dataset_id) {
  resp <- httr::POST(glue('https://api.domo.com/v1/datasets/query/execute/{dataset_id}'),
                     add_headers('Accept' = 'application/json',
                                 "Authorization" = paste('Bearer', gsub('\n', '', domorrr:::.domorrr_env$access_token))),
                     body = list(sql = 'SELECT * FROM table LIMIT 1'),
                     encode = 'json'
  )
  stop_for_status(resp)
  d <- content(resp, 'text') %>% jsonlite::fromJSON()
  cbind(field = d$columns, d$metadata) %>% as.data.frame(stringsAsFactors = FALSE) %>%
    mutate(field = as.character(field))
}



fnFixClasses <- function(data, these_fields) {
  fields_to_date <- these_fields %>% filter(type == 'DATE') %>% pull(field)
  fields_to_datetime <- these_fields %>% filter(type == 'DATETIME') %>% pull(field)
  fields_to_numeric <- these_fields %>% filter(type %in% c('LONG', 'DOUBLE', 'DECIMAL')) %>% pull(field)
  fields_other <- these_fields %>% filter(!field %in% c(fields_to_date, fields_to_datetime, fields_to_numeric)) %>% pull(field)
  fields_order <- these_fields %>% pull(field)

  list(
    data %>% select(all_of(fields_to_date)) %>% mutate_all(as_date),
    data %>% select(all_of(fields_to_datetime)) %>% mutate_all(lubridate::as_datetime),
    data %>% select(all_of(fields_to_numeric)) %>% mutate_all(as.numeric) %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0))),
    data %>% select(all_of(fields_other))
  ) %>%
    bind_cols() %>%
    select(all_of(fields_order))
}



#' Query Dataset
#'
#' Query a dataset by dataset ID with SQL.
#'
#' @seealso \url{https://developer.domo.com/docs/dataset-api-reference/dataset}
#'
#' @param dataset_id Dataset ID to query
#' @param sql to query the dataset
#'
#' @return A \code{list} containing \code{data} and \code{metadata} from the queried dataset.
#' @export
#'
#' @examples
#' SQL dialect is MySQL (i think)
#' SELECT * FROM table LIMIT 10
#' SELECT DISTINCT `Device Category`, `Web Property`, `Account` FROM table
#' SELECT SUM(Pageviews) pv, sum(Sessions) sessions FROM table GROUP BY Hostname
#' SELECT * FROM table LIMIT 10, 25"), # Limit to 10 offset by 25, nice!!!
#' SELECT * FROM table WHERE period = 201907 LIMIT 500000, 500000"
queryDataset <- function(dataset_id, sql = 'SELECT * FROM table LIMIT 10') {
  resp <- httr::POST(glue::glue('https://api.domo.com/v1/datasets/query/execute/{dataset_id}'),
               httr::add_headers('Accept' = 'application/json',
                                 "Authorization" = paste('Bearer', gsub('\n', '', domorrr:::.domorrr_env$access_token))),
               body = list(sql = sql),
               encode = 'json'
  )
  stop_for_status(resp)
  d <- content(resp, 'text') %>% jsonlite::fromJSON()
  x <- list()
  x$data <- d$rows %>% as.data.frame(stringsAsFactors = FALSE) %>% dplyr::rename_at(vars(everything()), list(~d$columns))
  x$meta <- cbind(field = d$columns, d$metadata) %>% as.data.frame(stringsAsFactors = FALSE) %>% mutate(field = as.character(field))
  x$data <- fnFixClasses(x$data, x$meta)
  x$data %>% as_tibble()
}






#' pipedrive_fromJSON
#'
#' @param .obj extracted object
#'
#' @importFrom jsonlite fromJSON
pipedrive_fromJSON <- function(.obj){
  
  jsonlite::fromJSON(.obj, simplifyVector = TRUE, flatten = TRUE)
  
}

#' pipedrive_api_get
#'
#' @param .url pipedrive url
#' @param .path api path
#' @param .query api query
#' @param .verbose TRUE/FALSE
#'
#' @importFrom httr GET
pipedrive_api_get <- function(.url, .path, .query, .verbose = FALSE){
  
  if(.verbose){
    httr::GET(.url, path = .path, query = .query, httr::verbose())
  }else{
    httr::GET(.url, path = .path, query = .query)
  }
}

#' pipedrive_api_content
#'
#' @param .res api response
#' @param .as parsing as text, json, ...
#'
#' @importFrom httr content
#' @importFrom stringi stri_unescape_unicode
pipedrive_api_content <- function(.res, .as = 'text'){
  
  if(is.null(.as)){
    return(httr::content(.res, encoding = 'UTF8'))
  }
  
  if(.as == 'text'){
    return(httr::content(.res, as = .as, encoding = 'UTF8') %>%
             stringi::stri_unescape_unicode())
  }
  
  stop('not implemented')
  
}

#' pipedrive_api_dealfields
#'
#' @param .url api url
#' @param .token api token
#' @param .start pagination start
#' @param .limit pagination limit (500 max)
#' @param .verbose TRUE/FALSE
#'
#' @export
#' @importFrom dplyr tbl_df
pipedrive_api_dealfields <- function(.url, .token, .start = 0, .limit = 500, .verbose = FALSE){
  
  .res <- pipedrive_api_get(.url, 'v1/dealFields', list(
    api_token = .token,
    start = .start, 
    limit = .limit
  ), .verbose) %>% 
    pipedrive_api_content() %>% 
    pipedrive_fromJSON()
  
  .res$data <- dplyr::tbl_df(.res$data)
  .res
}

#' pipedrive_api_deals
#'
#' @param .url api url
#' @param .token api token
#' @param .start pagination start
#' @param .limit pagination limit (500 max)
#' @param .verbose TRUE/FALSE
#'
#' @export
#' @importFrom dplyr tbl_df
pipedrive_api_deals <- function(.url, .token, .start = 0, .limit = 500, .verbose = FALSE){
  
  .res <- pipedrive_api_get(.url, 'v1/deals', list(
    api_token = .token,
    start = .start, 
    limit = .limit
  ), .verbose) %>% 
    pipedrive_api_content() %>% 
    pipedrive_fromJSON()
  
  .res$data <- dplyr::tbl_df(.res$data)
  .res
}


#' pipedrive_paginate
#'
#' @param .api api functional
#' @param .start pagination start
#' @param .limit pagination limit (500 max)
#' @param .page_limit maximum number of downloaded pages
#'
#' @export
#' @importFrom plyr llply
#' @importFrom dplyr bind_rows
pipedrive_paginate <- function(.api, .start = 0, .limit = 500, .page_limit = 100){
  
  ## functional api
  jsob <- .api(.start, .limit)
  
  e1 <- new.env()
  assign('1', jsob, envir = e1)
  i <- 1
  
  while (jsob$additional_data$pagination$more_items_in_collection) {
    
    i <- i + 1
    jsob <- .api(.start = jsob$additional_data$pagination$next_start, 
                 .limit = 500)
    assign(as.character(i), jsob, envir = e1) 
    if(i > .page_limit){
      message('pipedrive_paginate reached limit')
      break()
    }
    
  }
  
  tbl1 <- plyr::llply(ls(e1), function(item){ 
    e1[[item]][['data']] 
  }) %>% 
    dplyr::bind_rows()
  
  list(data = tbl1, obj = e1)
  
}


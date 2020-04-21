





#' Define an address field
#' @description Used to define an address field, as a starting point to match the address in the BAG.
#' @param data Either a dataframe (if columns argument is used), or a vector (if template argument is used).
#' @param columns List, see Details.
#' @param template Character string. See Details.
#' @details This function can be used in two ways. Either provide a dataframe, and \code{columns} to define
#' which columns to combine into an address field, or provide a text vector and a "template" defining
#' how the address information is included.
#'
#' With the columns argument, provide a list like: list(straat = "straatnaam", huisnummer = "huisnr"),
#' where the allowed fields are 'straat','huisnummer','woonplaats','huisnummer',
#' 'huisnummertoevoeging', 'postcode'. The right-hand value of the list (between "") is the name of the colum
#' in the provided dataset.
#'
#' When \code{template} is used, it is used to define how the address is provided. For example,
#' with data = "Voorbeeldplein 2 Eendhoven", template should be "{straat}{huisnummer}{woonplaats}".
#' Use curly braces to separate the fields.
#' @export
make_address_field <- function(data,
                               columns = NULL,

                               template = NULL
){

  stopifnot(!is.null(columns) | !is.null(template))

  if(!is.null(columns)){

    fields <- names(columns)

    vars <- unlist(columns)

    if(!all(vars %in% names(data))){
      stop("Not all columns present in data.")
    }

    out <- data[, vars] %>%
      setNames(fields)

    for(i in seq_len(ncol(out))){

      nm <- fields[i]

      out[,i] <- switch(nm,
                        straat = out[,i],
                        postcode = postcode_nospace(out[,i]),
                        huisnummer = as.character(out[,i]),
                        huisletter = out[,i],
                        huisnummertoevoeging = out[,i],
                        woonplaats = out[,i]
      )

    }

    adres_string <- apply(out, 1, function(x){

      paste0(emp(x["postcode"]), " ",
             emp(x["straat"]), " ",
             emp(x["huisnummer"]),
             emp(x["huisletter"]), " ",
             emp(x["huisnummertoevoeging"])) %>%
        tolower(.) %>%
        str_trim(.) %>%
        remove_bad_chars(.)

    })

  } else {


    fields <- fields_from_template(template)

    adres_string <- data %>%
      tolower %>%
      str_trim %>%
      remove_bad_chars

  }

  adres_string <- unname(adres_string)
  class(adres_string) <- "adres_field"
  attr(adres_string, "adres_fields") <- fields

  return(adres_string)
}


#' @export
print.adres_field <- function(x, n = 10, ...){

  x <- unclass(x)
  cat(paste("Adres field with",
              length(x),
              "values and",
              sum(is.na(x)),
              "missing values.\n"))
  cat(paste("Fields:",
            paste(attr(x, "adres_fields"), collapse= ", "),
            "\n\n"
            ))

  if(length(x)){
    print.default(x[1:n])
  }

}




#' Match addresses to the BAG
#' @param x An address-field object, defined with \code{make_address_field}, see Examples.
#' @param bag The BAG for this Gemeente (dataframe).
#' @param bag_columns Which columns from the BAG to return as a result.
#' @examples
#'
#' \dontrun{
#'
#' # Example for Nederweert
#' # 'bag' has to be read in first, using a local copy.
#'
#' adr <- c("Veldweverstraat 3", "Banendijk 5")
#'
#' make_address_field(adr, template = "{straat}{huisnummer}") %>%
#'   match_bag_address(., bag = bag)
#' }
#'
#' @export
match_bag_address <- function(x, bag, bag_columns = "all"){

  stopifnot(inherits(x, "adres_field"))

  if(bag_columns[1] == "all"){
    bag_columns <- names(bag)
  }

  fields <- attr(x, "adres_fields") %>%
    convert_adres_fields(.)

  if(!all(fields %in% names(bag))){
    stop("Not all fields found in BAG.")
  }

  txt_ <- unclass(x)

  find_ <- bag_paste_columns(bag, fields) %>%
    tolower

  ff <- fuzzy_find(txt_, find_)

  # adressen zonder huisnummer moeten NA zijn
  ff[!grepl("[0-9]", txt_), ] <- NA

  # huisnummers moeten gelijk zijn
  bag_match <- bag[match(ff$match, find_),]

  mtch_huisnr <- rep(FALSE, length(txt_))
  for(i in seq_along(txt_)){
    nr <- bag_match$huisnummer[i]
    if(is.na(nr))next
    mtch_huisnr[i] <- any(grep(glue("\\b{nr}\\b"), txt_[i]))
  }

  ff[!mtch_huisnr, ] <- NA

  b <- cbind(bag[match(ff$match, find_), bag_columns],
             data.frame(char_distance = ff$distance))

return(b)
}





#' Search for BAG addresses in a text field
#' @description Searches for addresses in a free text field, which may also contain other text.
#' For each field, all BAG addresses are searched - so this function is slow for larger queries, and
#' especially larger BAG datasets.
#' @param txt Free text field (vector)
#' @param bag BAG dataset (dataframe). In-memory.
#' @param template The template to look for in the free text field.
#' @param progressbar If TRUE (the default), displays a simple progress bar.
#' @examples
#'
#' \dontrun{
#' search_bag_address("Er is iets gebeurd op Voorbeeldplein 11",
#' bag, template = "{straat}{huisnummer}")
#' }
#' @export
search_bag_address <- function(txt, bag,
                         template = "{straat} {huisnummer}",
                         progressbar = TRUE){


  fields <- fields_from_template(template) %>%
    convert_adres_fields(.)

  find_ <- bag_paste_columns(bag, fields) %>%
    tolower

  txt_ <- tolower(txt) %>%
    postcode_nospace %>%
    remove_bad_chars

  out <- vector("list", length = length(txt_))

  if(progressbar){
    wp <- txtProgressBar(title = "Finding BAG adresses in text fields.",
                         label = "", min = 0, max = length(find_), initial = 0,
                         width = 80, style = 3)
  }

  for(i in seq_along(find_)){

    regex <- sprintf("\\b%s\\b", find_[i])

    ii <- grep(regex, txt_, ignore.case=TRUE)

    if(length(ii)){
      for(j in ii){
        out[[j]] <- c(out[[j]], i)
      }
    }

    if(progressbar)setTxtProgressBar(wp, i)
  }
  if(progressbar)close(wp)

return(out)
}



expand_dataframe_labels <- function(data, label_col = "label"){

  labels <- function(x){
    str_trim(strsplit(as.character(x), ";")[[1]])
  }

  nlabels <- function(x){
    length(labels(x))
  }

  l <- lapply(1:nrow(data), function(i){
    if(nlabels(data[i, label_col]) <=  1){
      return(data[i,])
    } else {
      labs <- labels(data[i,label_col])
      out <- replicate(length(labs), data[i,], simplify = FALSE)

      out <- do.call(rbind, out)

      out[,label_col] <- labs
      return(out)
    }
  })
  as.data.frame(do.call(rbind, l))

}





# Deze functie doet OF een match, OF een zoek-door-veld.
# (sneller dan altijd zoek-door-veld)
# adres naar BAG object_id
# bag_find_address <- function(txt,
#                            bagdata,
#                            template = "{openbareruimte} {huisnummer}",
#                            out_columns = "all",
#                            progressbar = TRUE){
#
#   if(out_columns[1] == "all"){
#     out_columns <- names(bagdata)
#   }
#
#   if(length(template) == 1){
#
#     v <- validate_address(txt, bagdata = bagdata, template = template)
#
#     ii <- seq_along(txt)
#     nona <- which(!is.na(v))
#     ii <- setdiff(ii,nona)
#
#     if(length(ii)){
#       ff <- find_address(txt[ii], bagdata = bagdata, template = template)
#       v[ii] <- ff
#     }
#
#     return(unname(v))
#
#   } else {
#
#     # Als meerdere templates als input, doorzoek alle templates en
#     # return de eerste non-empty result
#     out <- lapply(template, function(x)get_address(txt, bagdata, x, progressbar))
#     first_nonempty_value(out)
#
#   }
# }






#' Define an address field
#' @description Used to define an address field, as a starting point to match the address in the BAG.
#' @param data Either a dataframe (if columns argument is used), or a vector (if template argument is used).
#' @param columns List, see Details.
#' @param template Character string. See Details.
#' @details This function can be used in two ways. Either provide a dataframe, and `columns` to define
#' which columns to combine into an address field, or provide a text vector and a "template" defining
#' how the address information is included.
#'
#' With the columns argument, provide a list like: list(straat = "straatnaam", huisnummer = "huisnr"),
#' where the allowed fields are 'straat','huisnummer','woonplaats','huisnummer',
#' 'huisnummertoevoeging', 'postcode'. The right-hand value of the list (between "") is the name of the colum
#' in the provided dataset.
#'
#' When `template` is used, it is used to define how the address is provided. For example,
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
    fields[fields == "straat"] <- "openbareruimtenaam"

    vars <- unname(unlist(columns))

    if(!all(vars %in% names(data))){
      stop("Not all columns present in data.")
    }

    out <- data[, vars] %>%
      setNames(fields)

    adres_string <- make_adres_string(out, fields)

  } else {


    fields <- fields_from_template(template)

    adres_string <- data %>%
      tolower %>%
      str_trim %>%
      remove_bad_chars

  }

  adres_string <- unname(adres_string)
  class(adres_string) <- ifelse(is.null(columns), "adres_template", "adres_columns")

  attr(adres_string, "adres_fields") <- fields
  attr(adres_string, "inputtype") <-

  return(adres_string)
}


paste_prep_columns <- function(data){

  apply(data, 1, paste, collapse = "_") %>%
    tolower(.) %>%
    remove_extra_underscores(.) %>%
    no_space(.) %>%
    remove_bad_chars(.)

}

make_adres_string <- function(data, fields){


  data <- data[, fields]

  part1_cols <- intersect(fields, c("postcode", "openbareruimtenaam", "huisnummer"))
  part2_cols <- intersect(fields, c("huisletter", "huisnummertoevoeging", "woonplaats"))

  data_part1 <- data[, part1_cols]
  data_part1[is.na(data_part1)] <- ""

  out <- paste_prep_columns(data_part1)

  if(length(part2_cols) > 0){

    data_part2 <- data[, part2_cols]
    data_part2[is.na(data_part2)] <- ""

    # Huisletter is een uitzondering: plak naast huisnummer
    if("huisletter" %in% part2_cols){

      have_hl <- !data$huisletter %in% c(NA, "")
      prefix <- ifelse(have_hl, "", "_")
    }

    part2 <- paste0(prefix, paste_prep_columns(data_part2))

    out <- paste0(out, part2) %>%
      remove_extra_underscores(.)
  }

out
}



#' @export
print.adres_columns <- function(x, n = 10, ...){

  n <- min(n, length(x))

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

#' @export
print.adres_template <- print.adres_columns



#' Match addresses to the BAG
#' @param x An address-field object, defined with `make_address_field`, see Examples.
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
match_bag_address <- function(x, ...){

  UseMethod("match_bag_address")

}

#' @export
match_bag_address.adres_columns <- function(x, bag, bag_columns = "all"){


  if(bag_columns[1] == "all"){
    bag_columns <- names(bag)
  }

  fields <- attr(x, "adres_fields") %>%
    convert_adres_fields(.)

  if(!all(fields %in% names(bag))){
    stop("Not all fields found in BAG.")
  }

  txt_ <- unclass(x) %>% no_space

  find_ <- make_adres_string(bag, fields)

  ff <- fuzzy_find(txt_, find_)
  if(!is_tibble(ff))ff <- bind_rows(ff)  # bug fix met 1 record

  # adressen zonder (huis)nummer moeten NA zijn
  ff[!grepl("[0-9]", txt_), ] <- NA

  # Nu dat we matches hebben gedaan op char distance, hebben we veel slechte matches,
  # huisnummers komen niet overeen.
  hr_f <- which(fields == "huisnummer")
  hr_data <- sapply(strsplit(x, "_"), function(row){

    if(length(row) < hr_f){
      return("")
    } else {
      row[hr_f]
    }

  })

  if(!all(hr_data == "")){
    hr_data <- gsub("[a-z]","",hr_data)

    bag_match_hr <- as.character(bag[match(ff$match, find_), ]$huisnummer)

    have_no_match <- which(bag_match_hr != hr_data)
    ff[have_no_match, ] <- NA
  }


  b <- cbind(bag[match(ff$match, find_), bag_columns],
             data.frame(char_distance = ff$distance))

return(b)
}




#' @export
match_bag_address.adres_template <- function(x, bag, bag_columns = "all"){

  if(bag_columns[1] == "all"){
    bag_columns <- names(bag)
  }

  fields <- attr(x, "adres_fields") %>%
    convert_adres_fields(.)

  if(!all(fields %in% names(bag))){
    stop("Not all fields found in BAG.")
  }

  txt_ <- unclass(x) %>% no_space

  paste_columns <- function(data, fields){

    data <- data[,fields]

    data <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE)

    data[is.na(data)] <- ""

    apply(data, 1, paste, collapse = " ") %>% str_trim
  }

  find_ <- paste_columns(bag, fields) %>%
    tolower %>% no_space

  ff <- fuzzy_find(txt_, find_)
  if(!is_tibble(ff))ff <- bind_rows(ff)  # bug fix met 1 record

  # adressen zonder huisnummer moeten NA zijn
  ff[!grepl("[0-9]", txt_), ] <- NA

  # huisnummers moeten gelijk zijn
  bag_match <- bag[match(ff$match, find_),]

  mtch_huisnr <- rep(FALSE, length(txt_))

  for(i in seq_along(txt_)){
    nr <- bag_match$huisnummer[i]
    if(is.na(nr))next

    # Huisnummer gevolgd door letter (huisletter, hopelijk) of 'word boundary' (space / eol)
    mtch_huisnr[i] <- any(grepl(glue("(\\b|[a-z]){nr}(\\b|[a-z])"), txt_[i]))
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
search_bag_address <- function(txt,
                               bag,
                         template = "{straat} {huisnummer}",
                         bag_column = "adresseerbaarobject",
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

  # Vervang index met gevraagde kolom, plak in 1.
  out <- sapply(out, function(x){
    paste(bag[x, bag_column], collapse=";")
  })

return(out)
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






has_num <- function(x)grepl("[0-9]", x)


is_empty <- function(x){
  is.null(x) || is.na(x) || length(x) == 0 || x == ""
}


remove_extra_underscores <- function(x){
  out <- gsub("(_)\\1+", "\\1", x)
  out <- gsub("^_","", out)
  out <- gsub("_$","", out)
  out
}


emp <- function(x)ifelse(is.na(x), "", as.character(x))

no_space <- function(x){
  str_replace_all(x, "[[:space:]]", "")
}

clength <- function(x){
  sapply(strsplit(x, ";"), length)
}

cvec <- function(x){
  strsplit(x, ";")[[1]] %>%
    str_trim
}

first_nonempty_value <- function(x){

  out <- x[[1]]

  for(i in 2:length(x)){
    out[out == ""] <- x[[i]][out == ""]
  }
  return(out)
}

filter_in <- function(data, x, values, all_value = ""){

  if(is.null(values) | identical(values, all_value)){
    return(data)
  }
  x <- sym(x)
  filter(data, !!x %in% values)

}


# Sorteer tekst op numerieke deel (prefix),
# dus: 1, 1A, 2, 3, 10, niet
# 1, (1A? NA?), 10, 2, 3
sort_leading_num <- function(x, decreasing = FALSE){
  if(length(x) == 1)return(x)

  num <- gsub("[a-z].*", "", x, ignore.case = TRUE)
  ii <- order(as.numeric(num), decreasing = decreasing)
  x[ii]
}



# "{part} {one}{two}" naar c("part", "one",  "two")
fields_from_template <- function(x){

  str_extract_all(x, "\\{(.*?)\\}")[[1]] %>%
    gsub("\\{","", .) %>%
    gsub("\\}","", .)

}



postcode_nospace <- function(x){

  pc <- str_extract(x, "[0-9]{4}[[:space:]]+[a-zA-Z]{2}")
  pc_ns <- str_replace(pc, "[[:space:]]", "")

  out <- str_replace(x, pc, pc_ns)
  out[is.na(out)] <- x[is.na(out)]

  return(out)
}


remove_doublespace <- function(x){
  gsub(" +"," ",x)
}

# Verwijder karakters die nooit in straatnamen voorkomen
remove_bad_chars <- function(x,
                             chars=c(",",";","/","&","%","@","#","$","!","?","*","(",")"),
                             double_space=TRUE){

  for(i in seq_along(chars)){
    x <- gsub(chars[i], " ", x, fixed = TRUE)
  }

  if(double_space){
    x <- remove_doublespace(x)
  }

  return(x)
}


# adres fields omzetten naar kolom namen in het BAG
convert_adres_fields <- function(x){

  x[x == "straat"] <- "openbareruimtenaam"
  x[x == "woonplaats"] <- "woonplaatsnaam"

  x <- x[x %in% c("postcode","openbareruimtenaam","huisnummer","woonplaatsnaam")]

  # Huisnummer vervangen door huisnr/ltr/toev.
  ii <- which(x == "huisnummer")
  if(length(ii)){
    x <- as.list(x)
    x[[ii]] <- c("huisnummer","huisletter","huisnummertoevoeging")
    x <- unlist(x)
  }

  x
}



bag_paste_columns <- function(data, fields){

  data <- data[,fields]

  make_adres_string(data)
}




fuzzy_find <- function(x, y){

  if(length(x) == 1){

    # Probeer eerst exact match te vinden.
    ii <- which(x == y)
    if(length(ii)){

      return(list(
        match = y[ii][1],
        distance = 0
      ))

    }

    # Niet gelukt? Dan de entry met de kortste char afstand.
    out <- try(unlist(aregexec(x, y, fixed=TRUE)))

    candidates <- y[out != -1]

    if(length(candidates) == 0){
      return(list(match = NA, distance = NA))
    }

    cand_dist <- adist(x, candidates)

    mindist <- min(cand_dist)
    mtch <- unique(candidates[cand_dist == mindist])

    return(dplyr::bind_rows(list(match = mtch[1],
                distance = mindist[1])))

  } else {
    dplyr::bind_rows(lapply(x, fuzzy_find, y = y))
  }
}







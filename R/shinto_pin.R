#' R6 Class with methods for rsconnect data pins
#' @description Get BAG & CBS data in Shiny applications, fast and without hassle.
#' @export
shintoPin <- R6::R6Class(
  public = list(

    #' @field server URL of the Rstudio Connect server
    server = NULL,

    #' @field temp_directory Location to download temporary files
    temp_directory = NULL,

    #' @description Create a new shinto rsconnect pin object
    #' @param server URL of the server (defaults to connect server at shinto Labs)
    #' @param temp_directory If NULL, defaults to 'cache' in current working directory
    initialize = function(server = "https://connect.shintolabs.net",
                          temp_directory = NULL){

      self$server <- server

      if(is.null(temp_directory)){
        self$temp_directory <- "cache"
      } else {
        self$temp_directory <- temp_directory
        dir.create(self$temp_directory, showWarnings = FALSE)
      }

      self$register()

    },

    #' @description Register the rsconnect board
    #' @param config_file Defaults to `options(path_shinydev_config = "<<filehere>>")`
    register = function(config_file= getOption("path_shinydev_config")){
      stopifnot(!is.null(config_file))
      key <- yaml::read_yaml(config_file)$rsconnect_api_key
      pins::board_register_rsconnect(server = self$server,
                                     key = key)

    },

    #' @description Name of the pin for the BAG for a gemeente
    #' @param gemeente E.g. "Eindhoven"
    pin_bag_name = function(gemeente){
      glue("bag-{tolower(gemeente)}-sf-rds")
    },

    #' @description Name of the pin for the CBS for a gemeente
    #' @param gemeente E.g. "Eindhoven"
    pin_cbs_name = function(gemeente){
      glue("cbs-{tolower(gemeente)}-sf-rds")
    },

    #' @description
    #' Store a pin on the rsconnect board. Simple wrapper around pin::pin, with typical settings.
    #' @param data Dataframe
    #' @param name Name to be used on rsconnect board (choose wisely!)
    #' @param description Description to show on rsconnect board
    pin = function(data, name, description){
      pin(I(data),
          name = name,
          description = description,
          board = "rsconnect")
    },


    #' @description
    #' Create a pin for the BAG and CBS data for a gemeente.
    #' @param gemeente E.g. "Eindhoven"
    pin_bag_cbs_data = function(gemeente){

      tictoc::tic(glue("BAG, CBS pins for gemeente {gemeente}"))
      flog.info(glue("Downloading from devpostgres02 for gemeente {gemeente}"))
      shintobag::download_gemeente_opendata(gemeente, self$temp_directory, re_download = FALSE)
      flog.info(glue("Download complete"))
      bag_fn_out <- file.path(p$temp_directory, glue("bag_{gemeente}_sf.rds"))
      cbs_fn_out <- file.path(p$temp_directory, glue("geo_{gemeente}.rds"))

      stopifnot(file.exists(bag_fn_out))
      stopifnot(file.exists(cbs_fn_out))

      bag <- readRDS(bag_fn_out)
      cbs <- readRDS(cbs_fn_out)

      bag_fn <- self$pin_bag_name(gemeente)
      cbs_fn <- self$pin_cbs_name(gemeente)

      suppressWarnings({
        pin(I(cbs),
            name = cbs_fn,
            description = glue("CBS (buurt/wijk/grens), gemeente {gemeente}"),
            board = "rsconnect")

        pin(I(bag),
            name = bag_fn,
            description = glue("BAG (R binary, sf-dataframe), gemeente {gemeente}"),
            board = "rsconnect")
      })

      flog.info(glue("Pins uploaded to rsconnect: {bag_fn} and {cbs_fn}"))
      tictoc::toc()
    },

    #' @description Get a pinned dataset
    #' @param what The name of the pin on the rsconnect board
    get_pin_data = function(what){

      pins::pin_get(what, board = "rsconnect")

    },

    #' @description Get pinned BAG for a gemeente
    #' @param gemeente E.g. "Eindhoven"
    get_bag_pin = function(gemeente){

      fn <- self$pin_bag_name(gemeente)
      self$get_pin_data(fn)

    },

    #' @description Get pinned CBS for a gemeente
    #' @param gemeente E.g. "Eindhoven"
    get_cbs_pin = function(gemeente){

      fn <- self$pin_cbs_name(gemeente)
      self$get_pin_data(fn)

    }



  )
)




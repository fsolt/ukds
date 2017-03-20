#' Download datasets from the UK Data Service
#'
#' \code{ukds_download} provides a programmatic and reproducible means to download datasets 
#'   from the UK Data Service's data archive
#'
#' @param file_id The unique identifier (or optionally a vector of these identifiers).
#'  for the dataset(s) to be downloaded (see details).
#' @param org,email,password Your UK Data Service organization, username, and password (see details).
#' @param use The number of a 'use of data' you have registered with the UK Data Service (see details).
#' @param reset If TRUE, you will be asked to re-enter your organization, username, and password.
#' @param download_dir The directory (relative to your working directory) to
#'   which files from the UK Data Service will be downloaded.
#' @param msg If TRUE, outputs a message showing which data set is being downloaded.
#' @param convert If TRUE, converts downloaded file(s) to .RData format.
#' @param delay If the speed of your connection to the UK Data Service is particularly slow, 
#'   \code{ukds_download} may encounter problems.  Increasing the \code{delay} parameter
#'   may help.
#'
#' @details 
#'  To avoid requiring others to edit your scripts to insert their own organization, email,  
#'  password, and use or to force them to do so interactively, the default is set to fetch 
#'  this information from the user's .Rprofile.  Before running \code{ukds_download}, 
#'  then, you should be sure to add these options to your .Rprofile substituting your 
#'  info for the example below:
#'
#'    \code{
#'        options("ukds_org" = "UK Data Service",
#'                "ukds_user" = "ukf0000000000",
#'                "ukds_password" = "password123!",
#'                "ukds_use" = "111111")
#'    }
#'
#' @return The function returns downloaded files.
#'
#' @examples
#' \dontrun{
#'  ukds_download(file_id = c())
#' }
#' 
#' @import RSelenium
#' @importFrom stringr str_detect str_subset
#' @importFrom magrittr '%>%'
#' @importFrom rio convert
#' @importFrom tools file_path_sans_ext
#' 
#' @export
ukds_download <- function(file_id, 
                          org = getOption("ukds_org"),
                          user = getOption("ukds_user"),
                          password = getOption("ukds_password"),
                          use = getOption("ukds_use"),
                          reset = FALSE,
                          download_dir = "ukds_data",
                          msg = TRUE,
                          convert = TRUE,
                          delay = 5) {
    
    # detect login info
    if (reset){
        org <- user <- password <- NULL
    }
    
    if (is.null(org)){
        ukds_org <- readline(prompt = "The UK Data Service requires your user account information.  Please enter your organization: \n")
        options("ukds_org" = ukds_org)
        org <- getOption("ukds_org")
    }
    
    if (is.null(user)){
        ukds_user <- readline(prompt = "Please enter your UK Data Service username: \n")
        options("ukds_user" = ukds_user)
        user <- getOption("ukds_user")
    }
    
    if (is.null(password)){
        ukds_password <- readline(prompt = "Please enter your UK Data Service password: \n")
        options("ukds_password" = ukds_password)
        password <- getOption("ukds_password")
    }
    
    if (is.null(use)) {
        ukds_use <- readline(prompt = "Please enter the ID number of a use of data registered with the UK Data Service: \n")
        options("ukds_use" = ukds_use)
        use <- getOption("ukds_use")
    }
    
    # build path to chrome's default download directory
    if (Sys.info()[["sysname"]]=="Linux") {
        default_dir <- file.path("home", Sys.info()[["user"]], "Downloads")
    } else {
        default_dir <- file.path("", "Users", Sys.info()[["user"]], "Downloads")
    }
    
    # create specified download directory if necessary
    if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
    
    # initialize driver
    if(msg) message("Initializing RSelenium driver")
    rD <- rsDriver(browser = "chrome", verbose = TRUE)
    remDr <- rD[["client"]]
    
    # sign in
    signin <- "https://qa.esds.ac.uk/secure/UKDSRegister_start.asp"
    remDr$navigate(signin)
    Sys.sleep(delay)
    remDr$findElement(using = "partial link text", "Let me choose")$clickElement()
    Sys.sleep(delay/2)
    remDr$findElement(using = "class", "as-selections")$sendKeysToElement(list(org))
    remDr$findElement(using = "class", "btn-enabled")$clickElement()
    Sys.sleep(delay/2)
    remDr$findElement(using = "id", "j_username")$sendKeysToElement(list(user))
    remDr$findElement(using = "id", "j_password")$sendKeysToElement(list(password))
    remDr$findElement(using = "class", "input-submit")$clickElement()
    Sys.sleep(delay)

    # loop through files
    for (i in seq(file_id)) { 
        item <- file_id[[i]]
        if(msg) message("Downloading UK Data Service file: ", item, sprintf(" (%s)", Sys.time()))
        
        # get list of current default download directory contents
        dd_old <- list.files(default_dir)
        
        # navigate to download page
        url <- paste0("https://discover.ukdataservice.ac.uk/catalogue/?sn=", item, "&type=Data%20catalogue")
        
        remDr$navigate(url)
        remDr$findElement(using = "partial link text", "Download")$clickElement()
        Sys.sleep(delay/2)
        remDr$findElement(using = "partial link text", "Login")$clickElement()
        Sys.sleep(delay/2)
        
        # select use
        remDr$findElement(using = "xpath", paste0("//input[@value=", use,"]"))$clickElement() # choose project
        Sys.sleep(delay)
        remDr$findElement(using = "xpath", "//input[@value='Add Datasets']")$clickElement() # add datasets
        Sys.sleep(delay/2)
        try(remDr$findElement(using = "xpath", "//input[@value='Add Datasets']")$clickElement()) # add datasets
        Sys.sleep(delay)
        
        # accept special terms, if any
        if (length(remDr$findElements(using = "partial link text", "Accept"))!=0) {
            remDr$findElement(using = "partial link text", "Accept")$clickElement()
            Sys.sleep(delay)
            remDr$findElement(using = "xpath", "//input[@value='I accept']")$clickElement()
            Sys.sleep(delay)
        }

        remDr$findElement(using = "xpath", paste0('//input[contains(@onclick,', item,')]'))$clickElement() # "Download"
        remDr$findElement(using = "xpath", "//input[@value='I accept']")$clickElement() # End User License
        
        remDr$findElement(using = "xpath", "//input[@value='STATA']")$clickElement() # Stata
        
        # check that download has completed
        dd_new <- list.files(default_dir)[!list.files(default_dir) %in% dd_old]
        wait <- TRUE
        tryCatch(
            while(all.equal(stringr::str_detect(dd_new, "\\.part$"), logical(0))) {
                Sys.sleep(1)
                dd_new <- list.files(default_dir)[!list.files(default_dir) %in% dd_old]
            }, error = function(e) 1 )
        while(any(stringr::str_detect(dd_new, "\\.crdownload$"))) {
            Sys.sleep(1)
            dd_new <- list.files(default_dir)[!list.files(default_dir) %in% dd_old]
        }
        
        # unzip into specified directory and convert to .RData
        dld_old <- list.files(download_dir)
        unzip(file.path(default_dir, dd_new), exdir = download_dir)
        unlink(file.path(default_dir, dd_new))
        dld_new <- list.files(download_dir)[!list.files(download_dir) %in% dld_old]
        file.rename(file.path(download_dir, dld_new), file.path(download_dir, item))
        
        data_file <- list.files(path = file.path(download_dir, item), recursive = TRUE) %>%
            str_subset("\\.dta")
        if (convert == TRUE) {
                rio::convert(file.path(download_dir, item, data_file),
                             paste0(tools::file_path_sans_ext(file.path(download_dir,
                                                                        item,
                                                                        basename(data_file))), ".RData"))
        }
    }
    
    # Close driver
    remDr$close()
    rD[["server"]]$stop()
}  


#' Download datasets from the UK Data Service
#'
#' \code{ukds_download} provides a programmatic and reproducible means to download datasets 
#'   from the UK Data Service's data archive
#'
#' @param file_id The unique identifier (or optionally a vector of these identifiers)
#'  for the dataset(s) to be downloaded (see details).
#' @param org,email,password Your UK Data Service organization, username, and password (see details)
#' @param reset If TRUE, you will be asked to re-enter your organization, username, and password.
#' @param download_dir The directory (relative to your working directory) to
#'   which files from the UK Data Service will be downloaded.
#' @param msg If TRUE, outputs a message showing which data set is being downloaded.
#' @param convert If TRUE, converts downloaded file(s) to .RData format.
#'
#' @details 
#'  To avoid requiring others to edit your scripts to insert their own email and  
#'  password or to force them to do so interactively, the default is set to fetch 
#'  this information from the user's .Rprofile.  Before running \code{ukds_download}, 
#'  then, you should be sure to add these options to your .Rprofile substituting your 
#'  info for the example below:
#'
#'    \code{
#'        options("ukds_org" = "UK Data Service",
#'                "ukds_user" = "ukf0000000000",
#'                "ukds_password" = "password123!")
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
#' @importFrom stringr str_detect str_replace
#' @importFrom haven read_por
#' @importFrom foreign read.spss
#' 
#' @export
ukds_download <- function(file_id, 
                          org = getOption("ukds_org"),
                          user = getOption("ukds_user"),
                          password = getOption("ukds_password"),
                          reset = FALSE,
                          download_dir = "ukds_data",
                          msg = TRUE,
                          convert = TRUE) {
    
    # Detect login info
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
    
    # Initialize driver
    if(msg) message("Initializing RSelenium driver")
    fprof <- makeFirefoxProfile(list(
        browser.download.dir = file.path(getwd(), download_dir),
        browser.download.folderList = 2L,
        browser.download.manager.showWhenStarting = FALSE,
        pdfjs.disabled=TRUE,
        plugin.scan.plid.all = FALSE,
        plugin.scan.Acrobat = "99.0",
        browser.helperApps.neverAsk.saveToDisk = "application/x-zip, application/x-zip-compressed, application/pdf"))
    rD <- rsDriver(browser = "firefox", extraCapabilities = fprof, verbose = FALSE)
    remDr <- rD[["client"]]
    
    # Sign in
    signin <- "https://qa.esds.ac.uk/secure/UKDSRegister_start.asp"
    remDr$navigate(signin)
    remDr$findElement(using = "partial link text", "Let me choose")$clickElement()
    Sys.sleep(2)
    remDr$findElement(using = "class", "as-selections")$sendKeysToElement(list(org))
    remDr$findElement(using = "class", "btn-enabled")$clickElement()
    Sys.sleep(2)
    remDr$findElement(using = "id", "j_username")$sendKeysToElement(list(user))
    remDr$findElement(using = "id", "j_password")$sendKeysToElement(list(password))
    remDr$findElement(using = "class", "input-submit")$clickElement()
    Sys.sleep(3)
    
    # Loop through files
    for (i in seq(file_id)) { 
        item <- file_id[[i]]
        if(msg) message("Downloading UK Data Service file: ", item, sprintf(" (%s)", Sys.time()))
        
        # Get list of current download directory contents
        if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
        dd_old <- list.files(download_dir)
        
        # build url
        url <- paste0("https://discover.ukdataservice.ac.uk/catalogue/?sn=", item, "&type=Data%20catalogue")
        
        # navigate to download page, start download
        remDr$navigate(url)
        remDr$findElement(using = "partial link text", "Download")$clickElement()
        Sys.sleep(2)
        remDr$findElement(using = "partial link text", "Login")$clickElement()
        Sys.sleep(2)
        
        # check for project (todo)
        remDr$findElement(using = "name", "pn")$clickElement() # choose project
        remDr$findElement(using = "css", "input:nth-child(3)")$clickElement() # add datasets
        
        remDr$findElement(using = "partial link text", "Accept")$clickElement()
        remDr$findElement(using = "name", "submit1")$clickElement() 
        
        remDr$findElement(using = "name", "Button2")$clickElement() # "Download"
        remDr$findElement(using = "name", "submit1")$clickElement() # End User License
        
        remDr$findElement(using = "css", "tr:nth-child(4) input")$clickElement() # Stata
        
        
        # check that download has completed
        dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
        wait <- TRUE
        tryCatch(
            while(all.equal(stringr::str_detect(dd_new, "\\.part$"), logical(0))) {
                Sys.sleep(1)
                dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
            }, error = function(e) 1 )
        while(any(stringr::str_detect(dd_new, "\\.part$"))) {
            Sys.sleep(1)
            dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
        }
        
        # create item directory and move file
        if (!dir.exists(file.path(download_dir, item))) dir.create(file.path(download_dir, item))
        file.rename(file.path(download_dir, dd_new), file.path(download_dir, item, dd_new))
        
        # convert to .RData
        if (convert == TRUE) {
            x <- tryCatch(haven::read_por(file.path(download_dir, item, dd_new)),
                          error = function(e) {
                              foreign::read.spss(file.path(download_dir, item, dd_new),
                                                 to.data.frame = TRUE,
                                                 use.value.labels = FALSE)
                          })
            save(x, file = stringr::str_replace(file.path(download_dir, item, dd_new), ".por", ".RData"))
        }
        

    }
    
    # Close driver
    remDr$close()
    rD[["server"]]$stop()
}  


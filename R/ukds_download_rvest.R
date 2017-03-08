#' Download datasets from the UK Data Service
#'
#' \code{ukds_download} provides a programmatic and reproducible means to download datasets 
#'   from the UK Data Service's data archive
#'
#' @param file_id The unique identifier (or optionally a vector of these identifiers)
#'  for the dataset(s) to be downloaded (see details).
#' @param org,email,password Your UK Data Service organization, username, and password (see details)
#' @param use The number of a 'use of data' you have registered with the UK Data Service (see details)
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
#' @importFrom stringr str_detect str_replace
#' @importFrom dplyr last '%>%'
#' @importFrom rio convert
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
    
    if (is.null(use)) {
        ukds_use <- readline(prompt = "Please enter the ID number of a use of data registered with the UK Data Service: \n")
        options("ukds_use" = ukds_use)
        use <- getOption("ukds_use")
    }
    
    # initialize driver
    if(msg) message("Initializing RSelenium driver")
    fprof <- makeFirefoxProfile(list(
        browser.download.dir = file.path(getwd(), download_dir),
        browser.download.folderList = 2L,
        browser.download.manager.showWhenStarting = FALSE,
        pdfjs.disabled=TRUE,
        plugin.scan.plid.all = FALSE,
        plugin.scan.Acrobat = "99.0",
        browser.helperApps.neverAsk.saveToDisk = "application/x-zip-compressed"))
    rD <- rsDriver(browser = "firefox", extraCapabilities = fprof, verbose = TRUE)
    remDr <- rD[["client"]]
    
    # sign in
    signin <- "https://qa.esds.ac.uk/secure/UKDSRegister_start.asp"

    s <- html_session(signin)
    org_link <- html_nodes(s, "option") %>% 
        .[str_detect(., org)] %>% 
        str_match('(?<=\\")[^"]*') %>%
        as.character()

    form <- html_form(s)[[1]] %>% 
        set_values(origin = org_link)
    fake_submit_button <- list(name = "submit-btn",
                               type = "submit",
                               value = "Continue",
                               checked = NULL,
                               disabled = NULL,
                               readonly = NULL,
                               required = FALSE)
    attr(fake_submit_button, "class") <- "btn-enabled"
    form[["fields"]][[7]] <- fake_submit_button
    s1 <- submit_form(s, form)
    
    form1 <- html_form(s1)[[1]] %>% 
        set_values(j_username = user,
                   j_password = password)
    
    s2 <- submit_form(s1, form1)
    
    # loop through files
    for (i in seq(file_id)) { 
        item <- file_id[[i]]
        if(msg) message("Downloading UK Data Service file: ", item, sprintf(" (%s)", Sys.time()))
        
        # get list of current download directory contents
        if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
        dd_old <- list.files(download_dir)
        
        # navigate to download page
        #url <- paste0("https://discover.ukdataservice.ac.uk/catalogue/?sn=", item, "&type=Data%20catalogue")
        url <- paste0("http://www.esds.ac.uk/newRegistration/addProjectDataset.asp?snAdd=", item)

        s <- html_session(url)
        
        data_page <- s2 %>% 
            jump_to(url)
        
        project_page <- s %>% 
            follow_link("Login") 
        
        form2 <- html_form(project_page)[[1]] %>% 
            set_values(origin = org_link)
        
        fake_submit_button <- list(name = "submit-btn",
                                   type = "submit",
                                   value = "Continue",
                                   checked = NULL,
                                   disabled = NULL,
                                   readonly = NULL,
                                   required = FALSE)
        attr(fake_submit_button, "class") <- "btn-enabled"
        form2[["fields"]][[7]] <- fake_submit_button
        p1 <- submit_form(project_page, form2)
        
            jump_to(data_page %>%
                        html_nodes("a") %>%
                        html_attr("href") %>%
                        .[str_detect(., "UKDSRegister")] %>%
                        first())
        
        
        remDr$findElement(using = "partial link text", "Download")$clickElement()
        Sys.sleep(2)
        remDr$findElement(using = "partial link text", "Login")$clickElement()
        Sys.sleep(2)
        
        # select use
        remDr$findElement(using = "xpath", paste0("//input[@value=", use,"]"))$clickElement() # choose project
        remDr$findElement(using = "xpath", "//input[@value='Add Datasets']")$clickElement() # add datasets
        Sys.sleep(2)
        
        # accept special terms, if any
        if (length(remDr$findElements(using = "partial link text", "Accept"))!=0) {
            remDr$findElement(using = "partial link text", "Accept")$clickElement
            Sys.sleep(2)
            remDr$findElement(using = "xpath", "//input[@value='I accept']")$clickElement()
            Sys.sleep(2)
        }
        
        remDr$findElement(using = "xpath", "//input[contains(@onclick, item)]")$clickElement() # "Download"
        remDr$findElement(using = "xpath", "//input[@value='I accept']")$clickElement() # End User License
        
        remDr$findElement(using = "xpath", "//input[@value='STATA']")$clickElement() # Stata
        
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
        
        # unzip and convert to .RData
        unzip(file.path(download_dir, dd_new), exdir = download_dir)
        unlink(file.path(download_dir, dd_new))
        dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
        
        data_file <- list.files(path = file.path(download_dir, dd_new), recursive = TRUE) %>%
            str_subset("\\.dta") %>%
            dplyr::last()
        if (convert == TRUE) {
            rio::convert(file.path(download_dir, dd_new, data_file),
                         paste0(tools::file_path_sans_ext(file.path(download_dir,
                                                                    dd_new,
                                                                    basename(data_file))), ".RData"))
        }
        
        file.rename(file.path(download_dir, dd_new), file.path(download_dir, item))
        
    }
    
    # Close driver
    remDr$close()
    rD[["server"]]$stop()
}  


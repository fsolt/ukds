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
    
    
    signin <- "http://esds.ac.uk/newRegistration/newLogin.asp"
    httr::handle_reset(signin)
    
    # get to org page and enter org
    p0 <- html_session(signin) %>% 
        follow_link("Login")
    org_link <- html_nodes(p0, "option") %>% 
        .[str_detect(., org)] %>% 
        str_match('(?<=\\")[^"]*') %>%
        as.character()

    f0 <- html_form(p0)[[1]] %>%
        set_values(origin = org_link)
    fake_submit_button <- list(name = "submit-btn",
                               type = "submit",
                               value = "Continue",
                               checked = NULL,
                               disabled = NULL,
                               readonly = NULL,
                               required = FALSE)
    attr(fake_submit_button, "class") <- "btn-enabled"
    f0[["fields"]][["submit"]] <- fake_submit_button
    
    c0 <- cookies(p0)$value
    names(c0) <- cookies(p0)$name
    p1 <- submit_form(session = p0, form = f0, config = config(cookiejar = 'cookies.txt'))
   # p1 <- submit_form(session = p0, form = f0, config = set_cookies(.cookies = c0))

    # enter user and password
    f1 <- html_form(p1) %>%
        first() %>% 
        set_values("j_username" = user,
                   "j_password" = password)
    c1 <- cookies(p1)$value
    names(c1) <- cookies(p1)$name
    
    p2 <- submit_form(session = p1, form = f1, config = config(cookiejar = 'cookies.txt'))
    #p2 <- submit_form(session = p1, form = f1, config = set_cookies(.cookies = c1))

    # click through
    f2 <- p2 %>%
        html_form() %>%
        first()
    c2 <- cookies(p2)$value
    names(c2) <- cookies(p2)$name
    
    p3 <- submit_form(p2, f2, config = config(cookiejar = 'cookies.txt'))
    
    # build url
    url <- "https://discover.ukdataservice.ac.uk/catalogue/?sn=8116&type=Data%20catalogue"
    
    # navigate to download page, get data
    item_page <- p3 %>% 
        jump_to(url) %>% 
        follow_link("Download") %>% 
        follow_link("Login")
    
    # get login cookies
    login_cookies = cookies(p3)$value
    names(login_cookies)=cookies(p3)$name
    
    
    login_ukds = function(user, password) {
        require(httr)
        require(rvest)
        
        set_config( config( ssl_verifypeer = 0L ) )
        
        #important - httr preserves cookies on subsequent requests to the same host, we don't need that because of sessions expiration
        handle_reset("https://usa.ipums.org/")
        
        #set login and password
        login1 = GET( "https://usa.ipums.org/usa-action/users/login" )
        form_auth = list( "j_username" = user , "j_password" = password )
        
        l1_cookies=login1$cookies$value
        names(l1_cookies)=login1$cookies$name
        
        #receive auth tokens as html hidden fields in a form
        login2 = POST(login1$url, body = form_auth, set_cookies(.cookies=l1_cookies), encode="form")
        login2_form = read_html(login2$content) %>% html_form() 
        
        l2_cookies=login2$cookies$value
        names(l2_cookies)=login2$cookies$name
        
        #submit the form back (browser submits it back automatically with JS)
        login3 = POST(login2_form[[1]]$url, body=list(RelayState=login2_form[[1]]$fields$RelayState$value, 
                                                      SAMLResponse=login2_form[[1]]$fields$SAMLResponse$value), 
                      set_cookies(.cookies=l2_cookies), 
                      encode="form")
        
        #now we have what we came for - _shibsession_* and JSESSION id cookie
        login_cookies = login3$cookies$value
        names(login_cookies)=login3$cookies$name
        
        return=login_cookies
    }
    
    
    
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
        url2 <- paste0("https://qa.esds.ac.uk/newregistration/UKDSaddProjectDataset.asp?snAdd=", item)
        
        s <- html_session(url2)
        
        s1 <- s %>% 
            follow_link("Login") 
        
        form1 <- html_form(s1)[[1]] %>% 
            set_values(origin = org_link)
        
        fake_submit_button <- list(name = "submit-btn",
                                   type = "submit",
                                   value = "Continue",
                                   checked = NULL,
                                   disabled = NULL,
                                   readonly = NULL,
                                   required = FALSE)
        attr(fake_submit_button, "class") <- "btn-enabled"
        form1[["fields"]][[7]] <- fake_submit_button
        s2 <- submit_form(s1, form1)
        
        form2 <- html_form(s2)[[1]] %>% 
            set_values(j_username = user,
                       j_password = password)
        
        s3 <- submit_form(s2, form2) 
        
        
        
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


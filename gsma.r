



## scraping gsmarena website ##


library(rvest)
library(dplyr)
library(magrittr)
library(htmltab)
library(purrr)
library(jsonlite)
library(stringr)

# setwd("S2/viz/ass3/")

options(stringsAsFactors = F)


build_oem_table <- function(...){
  
  sesh <- html_session("https://www.gsmarena.com/makers.php3") 
  makers <- read_html(sesh)
  
  maker_nodes <- makers %>% html_nodes(".st-text a") 
  
  maker_names <- maker_nodes %>% html_text()
  
  maker_devices_count <- makers %>% html_nodes(".st-text span") %>% html_text()
  
  oem_names <- mapply(gsub, maker_devices_count, "", maker_names) %>% `names<-`(NULL)
  
  maker_devices_count <- gsub(pattern = " devices", replacement = "", x = maker_devices_count) %>% as.numeric()
  
  maker_url = maker_nodes %>% html_attr('href')
  
  oem_table <- data.frame(maker = oem_names, device_count = maker_devices_count, resource_location = maker_url)
  
  return(oem_table)
}

# oem_table <- build_oem_table()

parse_resource_locator <- function(location){
  paste0("https://www.gsmarena.com/", location)
}




oem_urls <- function(oem_base_url){
  src <- read_html(oem_base_url); Sys.sleep(3)
  
  items <- src %>% html_nodes(".nav-pages strong , .nav-pages a") %>% html_text()
  
  if (length(items) != 0){
    page_range <- 1:(items[length(items)] %>% as.numeric())
    
    maker_id <- stringr::str_match(oem_base_url, "https://www.gsmarena.com/(.*?)-phones-")[2]
    maker_indx <- stringr::str_match(oem_base_url, ".*-phones-(.*?).php")[2]
    
    map_chr(page_range, 
            function(pg_count) {
              paste0("https://www.gsmarena.com/", maker_id, "-phones-f-",
                     maker_indx, "-0-p", pg_count, ".php"
              )
            }
    )
  } else {
    oem_base_url
  }
  
  
}

# oem_urls("https://www.gsmarena.com/samsung-phones-9.php")


listed_devices <- function(page_url){
  
  src <- read_html(page_url)
  nodes <- src %>% html_nodes("#review-body a") 
  
  devices <- nodes %>% html_text()
  devices_url <- nodes %>% html_attr('href')
  
  data.frame(device_name = devices, device_resource = devices_url)
}




scrape_df <- function(url) {
  
  src <- read_html(url)
  n_head <- src %>% html_nodes("th") %>% html_text() %>% length()
  
  doc <- xml2::download_xml(url)
  
  get_head_tbl <- function(head_indx) {
    
    out = tryCatch(
      {
        suppressMessages(htmltab(doc, which = head_indx ) %>% 
                           as.data.frame() %>% 
                           rbind(colnames(.), .) %>% 
                           `colnames<-`(c("type", "sub_type", "val")))
      },
      
      error = function(e){
        xp <- '//th | //*[contains(concat( " ", @class, " " ), concat( " ", "ttl", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "nfo", " " ))]'
        print(paste("Fetching chunk", head_indx, "of", n_head))
        
        suppressMessages(htmltab(url, which = head_indx, body = xp ) %>% 
                           as.data.frame() %>% 
                           rbind(colnames(.), .) %>% 
                           `colnames<-`(c("type", "sub_type", "val")))
      }
    )
    
    out
    
  }
  
  df <- map(1:n_head, get_head_tbl) %>% bind_rows()
  
  system("rm *.php")
  df
  
}


safe_scraper <- safely(scrape_df, otherwise = NULL)

get_os_type <- function(raw_df) {
  if (nrow(raw_df[raw_df$sub_type == "OS",]) == 0) {
    return (NA)
  } else {
    os_info <- raw_df %>% filter(sub_type == "OS") %>% select(val) %>% 
      str_split(pattern = ",|;", simplify = T) %>% extract(1)
    return(os_info)
  }
}

get_camera <- function(raw_df) {
  if (length(raw_df$val[raw_df$type %in% c("Main Camera", "Camera")]) == 0) {
    return(NA)
  } else if (raw_df$val[raw_df$type %in% c("Main Camera", "Camera")][1] %in% c("NO", "no", "No")) {
    return (NA)
  } else {
    return(raw_df$sub_type[raw_df$type %in% c("Main Camera", "Camera")][1])
  }
}



df_cols <- function(raw_df) {
  
  raw_df$sub_type <- str_trim(raw_df$sub_type, side = "both")
  
  dimensions <- strsplit(raw_df %>% filter(sub_type == "Dimensions") %>% select(val) %>% str_extract(".*mm") %>% gsub(" mm", "", .), split = " x ") %>%
    unlist() %>% as.numeric()
  
  ram <- str_extract(raw_df$val[raw_df$sub_type == 'Internal'], "\\d+ (GB|MB) RAM") %>% paste(sep = " ", collapse = " ")
  ram_gigabytes <- if(grepl("MB", ram, ignore.case = T)){
    (str_extract(ram, "\\d+") %>% as.numeric())/1024
  } else {
    str_extract(ram, "\\d+") %>% as.numeric()
  }
  
  weight <- raw_df$val[raw_df$sub_type == 'Weight'] %>% str_extract(".* g") %>% gsub(" g", "", .) %>% as.numeric()
  
  display_size_inches <- raw_df$val[raw_df$sub_type == 'Size'] %>% str_extract("(.*)inches") %>% gsub(" inches", " ", .) %>% as.numeric()
  
  display_tech <- raw_df$val[raw_df$sub_type == 'Type'] %>% str_extract(., "[A-Z\\s]{3,}") %>% str_trim(side = "both")
  
  display_res <- raw_df$val[raw_df$sub_type == "Resolution"] %>% str_extract(".*x.*pixel") %>% gsub(" pixel", "", .)
  
  os <- get_os_type(raw_df)
  
  battery_cap <- raw_df %>% filter(type == "Battery", sub_type == "") %>% 
    select(val) %>% str_extract("\\d+(?= mAh)") %>% as.numeric()
  
  price <- safely(raw_df$val[raw_df$sub_type == "Price"], otherwise = NA)
  price <- price()$result
  
  cam <- get_camera(raw_df)
  
  audio_jack <- raw_df$val[raw_df$sub_type == "3.5mm jack"]
  
  bt_v <- raw_df$val[raw_df$sub_type == "Bluetooth"] %>% str_extract("\\d+(.\\d+)?|No|NO|no") %>% as.numeric()
  
  sensors <- raw_df$val[raw_df$sub_type == "Sensors"]
  
  announcement <- raw_df$val[raw_df$sub_type == "Announced"]
  
  df <- data.frame(
    announced = announcement,
    dim_length = dimensions[1],
    dim_breadth = dimensions[2],
    dim_thickness = dimensions[3],
    ram_gb = ram_gigabytes,
    weight = weight,
    display_size_inches = display_size_inches,
    display_tech = display_tech,
    display_res = display_res,
    os = os,
    battery_cap = battery_cap,
    price = price,
    camera_type = cam,
    audio_jack = audio_jack,
    bluetooth_v = bt_v,
    sensors = sensors
  )
  df
}




gsm_cols <- c('oem_name', 'device_name', 'announced', 'dim_length','dim_breadth','dim_thickness','ram_gb','weight','display_size_inches','display_tech','display_res','os','battery_cap','price','camera_type','audio_jack','bluetooth_v','sensors')
init_df <- matrix(data = NA, ncol = 18) %>% as.data.frame() %>% `colnames<-`(gsm_cols)
ll <- list(devices = list())

safe_df_cols <- safely(df_cols, otherwise = init_df)




loop_the_loop <- function(filter_for_assignment = F) {
  

  print("building oem table...")
  
  oem_table <- build_oem_table()
  
  print("oem table built!")
  
  for (oem in oem_table$maker[4:nrow(oem_table)] ){
    print(paste("processing OEM:", oem))
    
    oem_listings <- parse_resource_locator(oem_table$resource_location[oem_table$maker == oem]) %>% oem_urls()
    
    print(paste("Pages found:", length(oem_listings)))
    
    ll$devices[[oem]] <<- list()
    
    for (page in oem_listings) {
      devices_on_page <- listed_devices(page)
      
      for (device in devices_on_page$device_name) {
        
        print(paste("retrieving data for:", device))
        
        out = tryCatch(
          {
            gsm_data <- safe_scraper(devices_on_page %>% 
                                    filter(device_name == device) %>%
                                    select(device_resource) %>% parse_resource_locator()
            )
            
            if(!is.null(gsm_data$result)){
              
              gsm_data <- gsm_data$result
              tmp_df <- data.frame(type = c("oem", "model"), sub_type = c("", ""), val = c(oem, device))
              
              gsm_data <- rbind(tmp_df, gsm_data)
              
              ll$devices[[oem]][[device]] <<- gsm_data
              
              writeLines(toJSON(ll), "gsm.json")
              
              if (isTRUE(filter_for_assignment)) {
                processed_data <- safe_df_cols(gsm_data)$result
                
                device_info <- data.frame(oem_name = oem, device_name = device)
                
                init_df <<- bind_rows(init_df, cbind(device_info, processed_data))
                
                write.csv(init_df, "gsm.csv", na = "", row.names = F)
                
                message("success..! csv written to fs")
              }
              
            }
 
          }
        )
      }
    }
  }
  
  return(ll)
}


gsm_df <- loop_the_loop()



ll <- readLines("~/Downloads/gsm.json") %>% fromJSON()

long_to_wide <- function(df){
  as.data.frame(t(df$val)) %>% 
    `colnames<-`(paste0(df$type, "_", df$sub_type) %>% 
                   str_trim(side = "both"))
}

gsm <- ll$devices %>% flatten() %>% map(long_to_wide) %>% bind_rows()


# remove trailing underscores from col names, replace spaces with underscores, lowercase col names

colnames(gsm) <- colnames(gsm) %>% str_replace( "_\\Z", "") %>% 
  str_replace_all(" ", "_") %>%  str_trim() %>% tolower()

write.csv(gsm, "gsm.csv", na = "", row.names = F)

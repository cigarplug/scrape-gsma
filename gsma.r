## scraping gsmarena website ##

library(rvest)
library(dplyr)
library(magrittr)
library(RMySQL)

options(stringsAsFactors = F)


build_oem_table <- function(...){

  sesh <- html_session("https://www.gsmarena.com/makers.php3") ; Sys.sleep(3)
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

oem_table <- build_oem_table()

parse_resource_locator <- function(location){
  paste0("https://www.gsmarena.com/", location)
}


oem_urls <- function(oem_base_url){
  src <- read_html(oem_base_url); Sys.sleep(3)
  items <- src %>% html_nodes(".nav-pages strong , .nav-pages a") %>% html_text()
  
  if(length(items) != 0){
    page_range <- seq(as.numeric(items[1]), as.numeric(items[length(items)]), 1)
    
    ## first part of url i.e, oem name (this regex doesn't work where oems have numbers in their names, e.g O2)
    p1 <- stringr::str_match(oem_base_url, "(.*?)\\d+-")[2] 
    
    ## second part of url i.e, gsmArena's internal index
    p2 <- stringr::str_match(oem_base_url, "\\d+") [1]
    
    purrr::map_chr(page_range, function(x){paste0( p1, "f-", p2, "-0-p", x, ".php")})
      
  } else {
    oem_base_url
  }
  
}




listed_devices <- function(page_url){
  
  src <- read_html(page_url); Sys.sleep(3)
  nodes <- src %>% html_nodes("#review-body a") 

  devices <- nodes %>% html_text()
  devices_url <- nodes %>% html_attr('href')
  
  data.frame(device_name = devices, device_resource = devices_url)
}


scrape_df <- function(device_url){
  
  src <- read_html(device_url); Sys.sleep(1)
  
  # title column
  c.title <- src %>% html_nodes(".ttl") %>% html_text()
  
  # values column
  c.values <- src %>% html_nodes(".nfo") %>% html_text()
  
  tmp <- data.frame(cbind(var = c.title, val =  c.values))
  
  dimensions <- strsplit(tmp[tmp$var == 'Dimensions', "val"] %>% stringr::str_extract(".*mm") %>% gsub(" mm", "", .), split = " x ") %>% unlist() %>% as.numeric()
  ram <- stringr::str_extract(tmp$val[tmp$var == 'Internal'], "\\d+ (GB|MB) RAM")
  
  
  
  l <- c(tech = tmp[tmp$var == 'Technology', 'val'],
            announced = tmp[tmp$var == 'Announced', "val"],
            status = tmp[tmp$var == 'Status', "val"],
            dim_length = dimensions[1],
            dim_breadth = dimensions[2],
            dim_thickness = dimensions[3],
            weight = tmp$val[tmp$var == 'Weight'] %>% stringr::str_extract(".* g") %>% gsub(" g", "", .) %>% as.numeric(),
            sim = tmp$val[tmp$var == "SIM"],
            display_tech = tmp$val[tmp$var == 'Type'],
            display_size = tmp$val[tmp$var == 'Size'] %>% stringr::str_extract("(.*)inches") %>% gsub(" inches", " ", .) %>% as.numeric(),
            display_res = tmp$val[tmp$var == "Resolution"] %>% stringr::str_extract(".*x.*pixel") %>% gsub(" pixel", "", .),
            aspect_ratio = tmp$val[tmp$var == "Resolution"] %>% stringr::str_extract("\\d+(\\.\\d+)?\\:\\d+(\\.\\d+)?"),
            ip_certification = tmp$val[grep("IP\\d+ cert", tmp$val)] %>% stringr::str_extract("IP\\d+"),
            os = tmp$val[tmp$var == "OS"],
            chipset = tmp$val[tmp$var == "Chipset"],
            cpu = tmp$val[tmp$var == "CPU"],
            gpu = tmp$val[tmp$var == "GPU"],
            card_slot = tmp$val[tmp$var == "Card slot"] %>% stringr::str_extract("Yes|No|microSD"),
            ram = ifelse(isTRUE(isTRUE(grepl("MB", ram))), (gsub(" (GB|MB) RAM", "", ram) %>% as.numeric())/1024, gsub(" (GB|MB) RAM", "", ram) %>% as.numeric()),
            bt_v = tmp$val[tmp$var == "Bluetooth"] %>% stringr::str_extract("\\d+(.\\d+)?|No|NO|no") %>% as.numeric(),
            gps = tmp$val[tmp$var == "GPS"] %>% gsub(pattern = "(?<=yes|no).*", replacement = "", ignore.case = T, x = ., perl = T),
            radio = tmp$val[tmp$var == "Radio"] %>% gsub(pattern = "(?<=yes|no|fm radio).*", replacement = "", ignore.case = T, x = ., perl = T),
            nfc = tmp$val[tmp$var == "NFC"] %>% gsub(pattern = "(?<=yes|no).*", replacement = "", ignore.case = T, x = ., perl = T),
            wlan = tmp$val[tmp$var == "WLAN"] %>% gsub(pattern = "(?<=yes|no|wi-fi|wifi).*", replacement = "", ignore.case = T, x = ., perl = T),
            battery_mah = tmp$val[grep("mah", tmp$val, ignore.case = T)]%>%.[1] %>% stringr::str_extract("\\d+ mAh") %>% gsub(" mah", "", ., ignore.case = T) %>% as.numeric(),
            audio_jack = tmp$val[grep("3.5mm jack", tmp$var, ignore.case = T)],
            price = tmp$val[tmp$var == "Price"] %>% stringr::str_extract("\\d+") %>% as.numeric(),
            camera = tmp$val[tmp$var == "Primary"],
            video = tmp$val[tmp$var == "Video"],
            selfie = tmp$val[tmp$var == "Secondary"]
            
            )
  
  
  
  # reason for creating a list first rather than directly a dataframe being that lists are seemingly more
  # flexible while creation. 0L objects like character(0) can be inserted in lists, but doing the same 
  # with a dataframe throws error
  
  ## convert list to data frame, courtesy stackoverflow ##
  

  return(as.data.frame(t(l)))
} 

## testing lalala ##

tmp <- scrape_df("https://www.gsmarena.com/motorola_moto_g5-8454.php")


db.con <- dbConnect(RMySQL::MySQL(), host = host, dbname = db, user = user, password = password)



## let's build a looooop now #

gsmA <- function(){
  
  # oem_table <- build_oem_table()
  error_log <<- c()
  
  out = tryCatch(
    {
     
      print("oem table has been built. proceeding..")
      
      for (i in 1:nrow(oem_table)){
        i<<- i
        print(paste0("current oem is: ", oem_table$maker[i]))
        
        oem_listings <- parse_resource_locator(oem_table$resource_location[i]) %>% oem_urls()
        
        print(paste0(length(oem_listings), " pages found"))
        
        for (j in 1:length(oem_listings)){
          j<<- j
          
          devices_on_page <- listed_devices(oem_listings[j])
          
          for (k in 1:nrow(devices_on_page)){
            k<<- k
            
            df <- parse_resource_locator(devices_on_page$device_resource[k]) %>% scrape_df()
            cols <- c('tech', 'announced', 'status', 'dim_length', 'dim_breadth', 'dim_thickness', 'weight', 'sim', 'display_tech', 'display_size', 'display_res', 'aspect_ratio', 'ip_certification', 'os', 'chipset', 'cpu', 'gpu', 'card_slot', 'ram', 'bt_v', 'gps', 'radio', 'nfc', 'wlan', 'battery_mah', 'audio_jack', 'price', 'camera', 'video', 'selfie')
            
            missing_cols <- cols[!cols %in% colnames(df)]
            
            if(length(missing_cols) != 0){
              missing_cols_df <- matrix(ncol = length(missing_cols)) %>% as.data.frame() %>% `colnames<-`(missing_cols)          
              df <- cbind(df, missing_cols_df)
            } 
    
            
            df <- cbind( df, data.frame(oem = oem_table$maker[i], device = devices_on_page$device_name[k] %>% gsub("'", "", .)))
            
            
            
            print(paste0("processing device: ", oem_table$maker[i], " ", devices_on_page$device_name[k]))
            
            q <- paste0("insert into devices (", paste0(colnames(df), collapse = ", "), ") values (",
                        paste0("'", df[1,], "'", collapse = ", "), ")"
            )
            
            dbGetQuery(db.con, q)
          }
        }
      }
       
    },
    
    error = function(cond){
    message('error encountered')
     error_log <<- c(error_log, devices_on_page$device_resource[k]) 
    }
    
    
      
  )
  
  
  
  
}
gsmA()

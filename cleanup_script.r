gsm <- read.csv("~/Downloads/8393_390606_bundle_archive/gsm.csv")
gsmu <- read.csv("~/git/scrape-gsma/gsm.csv")

gsmu[gsmu==""]<-NA
gsm[gsm==""]<-NA


# 
# one <- df %>%
#   filter(oem == "Samsung", model == "Galaxy S7") %>%
#   select(colnames(df)[which(!is.na(colnames (df) %>% stringr::str_match("selfie")))]
# )
# 
# two <- gsm %>%
#   filter(oem == "Samsung", model == "Galaxy S7") %>%
#   select(colnames(gsm)[which(!is.na(colnames (gsm) %>% stringr::str_match("selfie")))]
# )
# 
# colnames(gsm)[colnames(gsm) %>% grep("network", .)]



# function to coalesce malformed columns into single column
clean_cols <- function(df, col_name){
  
  # cast columns as strings
  
  df <- df %>% mutate_if(is.logical ,as.character)
  
  col_vals <- do.call(coalesce, 
                 df[, which(!is.na(colnames (df) %>%  stringr::str_match(paste0(col_name, "\\.\\.\\.[0-9]+"))
                                   ))
                    ]
                 )
  
  tmp_df <- data.frame( col_vals) %>% `colnames<-`(col_name)
  
  df <- df %>% 
    select(-colnames(df)[which(!is.na(colnames (df) %>% stringr::str_match(paste0(col_name, "\\.\\.\\.[0-9]+"))
                                      ))
                         ]
           )
  
  df <- cbind(df, tmp_df)
  df
}

# print out improper column names
colnames(gsmu)[which(!is.na(colnames (gsmu) %>% stringr::str_match("\\.\\.\\.[0-9]+")))]



# colnames(gsmu)[which(!is.na(colnames (gsmu) %>% stringr::str_match("battery_talk_time\\.\\.\\.[0-9]+")))]




df <- clean_cols(gsmu, "network")
df <- clean_cols(df, "battery_talk_time")
df <- clean_cols(df, "battery_stand.by")

setdiff(  colnames(gsmu), colnames(gsm))


df %>% 
  filter(!is.na(battery_v1) & !is.na(battery)) %>%
  select(battery, battery_v1, battery_v2)


battery_stand_by <- do.call(coalesce, 
        df[, c("battery_stand.by", "battery_stand.by.1")
        ]
)

df$battery_stand.by.1 <- NULL
df$battery_stand.by <- battery_stand_by

# colnames(df)[which(!is.na(colnames (df) %>% stringr::str_match("battery(_V1)?")))]

battery <- do.call(coalesce, 
                            df[, c("battery", "battery_v1", "battery_v2")
                            ]
)

df$battery_v1 <- NULL
df$battery_v2 <- NULL
df$battery <- battery



camera <- do.call(coalesce, 
                   df[, c("camera", "camera_v1", "camera_v2")
                   ]
)


df$camera_v1 <- NULL
df$camera_v2 <- NULL
df$camera <- camera



tmp <- gsm %>% 
  filter(!is.na(selfie_camera) & !is.na(selfie_camera_v1)) %>%
  select(selfie_camera, selfie_camera_v1)


selfie <- do.call(coalesce, 
                  df[, c("selfie_camera", "selfie_camera_v1", "selfie_camera_v2")
                  ]
)

df$selfie_camera_v1 <- NULL
df$selfie_camera_v2 <- NULL
df$selfie_camera <- selfie



write.csv(df, "gsm.csv", na = "", row.names = F)

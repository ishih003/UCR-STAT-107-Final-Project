load_youtube_data <- function(path = "US_Trending.csv") {
  df <- read.csv(path, stringsAsFactors = FALSE)
  return(df)
}

clean_youtube_data <- function(df) {
  
  required_cols <- c(
    "views",
    "likes",
    "comments",
    "category_id",
    "publish_time"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  df$category_id <- as.factor(df$category_id)
  df$publish_time <- as.POSIXct(df$publish_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  df$engagement_rate <- NA
  valid_rows <- df$views > 0
  df$engagement_rate[valid_rows] <- (df$likes[valid_rows] + df$comments[valid_rows]) / df$views[valid_rows]
  
  df <- df[!is.na(df$engagement_rate), ]
  
  return(df)
}

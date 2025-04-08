library(rvest)
library(httr)
library(tidyverse)
library(jsonlite)

# Configuration 
api_config <- list(
  url = "https://api.deepseek.com/v1/chat/completions",
  model = "deepseek-chat",
  temperature = 0.1,
  max_tokens = 50,
  delay = 1,
  api_key= "Crossed_out_available_in_the_README_file"  
)
# Function to extract text from nested list structure
extract_advisory_text <- function(detail_list) {
  tryCatch({
    flattened <- unlist(detail_list, recursive = TRUE)
    flattened <- flattened[nzchar(flattened) & !is.na(flattened)]
    flattened <- as.character(flattened)
    
    return(paste0(flattened, collapse = "\n"))
  },
  error = function(e){
    print(e)
    warning("Convert to string Error!")
    return(NA_character_)
  })
}

# Enhanced system message with separator explanation
SYSTEM_PROMPT <- paste(
  "STRICT AVIATION ADVISORY CLASSIFICATION RULES:\n",
  "1. You MUST select exactly ONE category from these options:\n",
  "   [GDP] Ground Delay Program\n",
  "   [AFP] Airspace Flow Program\n",
  "   [Capacity] Airport Capacity Issues\n",
  "   [Special] Special Activities\n",
  "   [Other] Other situations\n\n",
  
  "2. You MUST include exactly ONE specifics in parentheses\n",
  "   - Only include the most critical detail\n",
  "   - Maximum 3 words in specifics\n\n",
  
  "3. RESPONSE FORMAT IS STRICTLY ENFORCED:\n",
  "   [Selected-Category] (single-specifics)\n\n",
  
  "4. REJECTION CRITERIA:\n",
  "   - Never include explanations\n",
  "   - Never add multiple specifics\n",
  "   - Never use commas in specifics\n",
  "   - Never exceed the format\n\n",
  
  "EXAMPLES OF ACCEPTABLE OUTPUT:\n",
  "[GDP] (volume)\n",
  "[Capacity] (ice)\n",
  "[Route] (TMI)\n",
  "[Special] (space launch)\n\n",
  
  "WARNING: ANY DEVIATION FROM THIS FORMAT WILL CAUSE SYSTEM FAILURES"
)

# Modified classification function
safe_classify <- function(advisory_text) {
  if(startsWith(advisory_text, "RAW TEXT")){
    return ("[Operation Plans]")
  }
  
  tryCatch({
    if (is.na(advisory_text)) return(NA_character_)
    
    response <- POST(
      url = api_config$url,
      add_headers(
        "Authorization" = paste("Bearer", api_config$api_key),
        "Content-Type" = "application/json"
      ),
      body = list(
        model = api_config$model,
        temperature = api_config$temperature,
        max_tokens = api_config$max_tokens,
        messages = list(
          list(role = "system", content = SYSTEM_PROMPT),
          list(role = "user", content = advisory_text)
        )
      ),
      encode = "json",
      timeout(60)
    )
    
    if (status_code(response) == 200) {
      return(content(response)$choices[[1]]$message$content)
    } else {
      warning("API request failed with status: ", status_code(response))
      return(NA_character_)
    }
  }, error = function(e) {
    warning("Classification error: ", conditionMessage(e))
    return(NA_character_)
  })
}

# Processing function with explicit environment
process_advisories_safe <- function(df) {
  # Initialize progress
  pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
  
  result <- df %>%
    mutate(
      Classification = map_chr(1:n(), function(i) {
        setTxtProgressBar(pb, i)
        text <- extract_advisory_text(.$Detail[[i]])
        res <- safe_classify(text)
        Sys.sleep(api_config$delay)
        return(res)
      })
    )
  
  close(pb)
  return(result)
}




library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(purrr)

BASE_URL  <- "https://www.chicago.gov"
WARDS_URL <- "https://www.chicago.gov/city/en/about/wards.html"
DELAY     <- 2
COOKIE    <- NULL  # Paste your browser cookie here if needed

# ── Helper: realistic browser GET ────────────────────────────────────────────
safe_read <- function(url, cookie = NULL) {
  headers <- c(
    `User-Agent`      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
    `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `Referer`         = "https://www.google.com/"
  )
  if (!is.null(cookie)) headers <- c(headers, Cookie = cookie)
  
  tryCatch({
    resp <- GET(url, add_headers(.headers = headers), timeout(20),
                config(followlocation = TRUE))
    if (http_error(resp)) { message("HTTP error: ", status_code(resp), " — ", url); return(NULL) }
    read_html(content(resp, as = "text", encoding = "UTF-8"))
  }, error = function(e) { message("Failed: ", url, " — ", e$message); NULL })
}

# ── Helper: extract emails ────────────────────────────────────────────────────
extract_emails <- function(page) {
  # 1. mailto: links (most reliable)
  mailto <- page |>
    html_elements("a[href^='mailto:']") |>
    html_attr("href") |>
    str_remove_all("^mailto:") |>
    str_remove_all("\\?.*$") |>
    str_trim()
  
  # 2. Regex over all page text
  pattern <- "[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}"
  text_emails <- str_extract_all(html_text2(page), pattern)[[1]]
  
  unique(tolower(c(mailto, text_emails)))
}

# ── Step 1: Fetch main wards page ─────────────────────────────────────────────
message("Fetching main wards page...")
main_page <- safe_read(WARDS_URL, cookie = COOKIE)
if (is.null(main_page)) stop("Could not load wards page. Set COOKIE at top of script.")

# ── Step 2: Diagnose & extract all links ──────────────────────────────────────
all_hrefs <- main_page |> html_elements("a") |> html_attr("href")
all_hrefs <- all_hrefs[!is.na(all_hrefs)]

message("\n── Sample of all links found on the page (first 30) ──")
print(head(all_hrefs, 30))

# Flexible match: grab anything that looks like a ward or alderman subpage
ward_links <- all_hrefs |>
  keep(~ str_detect(.x, regex("ward|alderm|council", ignore_case = TRUE))) |>
  map_chr(~ ifelse(str_starts(.x, "http"), .x, paste0(BASE_URL, .x))) |>
  unique()

message("\n── Ward/alderman links matched (", length(ward_links), ") ──")
print(ward_links)

if (length(ward_links) == 0) {
  message("\nNo ward links found with current pattern.")
  message("Check the 'Sample of all links' printed above and adjust the regex in keep().")
  stop("No ward links to scrape.")
}

# ── Step 3: Scrape each ward page ─────────────────────────────────────────────
scrape_ward <- function(url) {
  message("  Scraping: ", url)
  Sys.sleep(DELAY)
  page <- safe_read(url, cookie = COOKIE)
  if (is.null(page)) return(tibble(ward_url = url, email = NA_character_))
  
  emails <- extract_emails(page)
  if (length(emails) == 0) tibble(ward_url = url, email = NA_character_)
  else tibble(ward_url = url, email = emails)
}

results <- map_dfr(ward_links, scrape_ward)

# ── Step 4: Clean & save ──────────────────────────────────────────────────────
if (nrow(results) == 0 || !("email" %in% names(results))) {
  message("No results returned. Check that ward pages loaded successfully.")
} else {
  results_clean <- results |>
    filter(!is.na(email), str_detect(email, "@")) |>
    distinct()
  
  message("\nTotal emails found: ", nrow(results_clean))
  print(results_clean)
  
  write.csv(results_clean, "chicago_ward_emails.csv", row.names = FALSE)
  message("Saved to chicago_ward_emails.csv")
}
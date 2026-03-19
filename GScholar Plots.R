# Google Scholar Citation Analysis
# Fetches papers matching a title search and plots citations by year
#
# Requirements:
#   install.packages(c("httr2", "jsonlite", "dplyr", "ggplot2"))
#
# Setup:
#   Get a free SerpApi key at https://serpapi.com (100 free searches/month)
#   Set it as an environment variable to avoid hardcoding:
#     Sys.setenv(SERPAPI_KEY = "your_key_here")
#   Or just paste it directly into the SERPAPI_KEY variable below.

library(httr2)
library(jsonlite)
library(dplyr)
library(ggplot2)

# ── Config ────────────────────────────────────────────────────────────────────

SERPAPI_KEY      <- "48c540a15874c984ebae0dac4921206a651e9b20c236d711481aa594342314a8"  # or paste your key as a string
QUERY            <- 'political science audit study'
MAX_PAGES        <- 30       # 10 pages × 20 results = up to 200 papers
RESULTS_PER_PAGE <- 20
SLEEP_SEC        <- 1        # be polite between requests

# ── Null-coalescing operator ──────────────────────────────────────────────────

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ── Fetch data ────────────────────────────────────────────────────────────────

fetch_page <- function(query, start, api_key) {
  resp <- request("https://serpapi.com/search") |>
    req_url_query(
      engine  = "google_scholar",
      q       = query,
      api_key = api_key,
      num     = RESULTS_PER_PAGE,
      start   = start
    ) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  
  if (resp_status(resp) != 200) {
    warning("Non-200 response at start=", start, ": ", resp_status(resp))
    return(NULL)
  }
  
  resp_body_json(resp)
}

cat("Fetching results for:", QUERY, "\n")

all_results <- list()

for (page in seq(0, MAX_PAGES - 1)) {
  start <- page * RESULTS_PER_PAGE
  cat(sprintf("  Page %d/%d (start=%d)...\n", page + 1, MAX_PAGES, start))
  
  data <- fetch_page(QUERY, start, SERPAPI_KEY)
  
  if (is.null(data) || is.null(data$organic_results) || length(data$organic_results) == 0) {
    cat("  No more results, stopping early.\n")
    break
  }
  
  all_results <- c(all_results, data$organic_results)
  
  # Stop early if we've already retrieved everything
  if (!is.null(data$search_information$total_results)) {
    total <- as.numeric(gsub(",", "", data$search_information$total_results))
    if (start + RESULTS_PER_PAGE >= total) break
  }
  
  Sys.sleep(SLEEP_SEC)
}

cat(sprintf("Fetched %d papers total.\n", length(all_results)))

# ── Parse into a data frame ───────────────────────────────────────────────────

parse_paper <- function(p) {
  year <- tryCatch({
    pub_info <- p$publication_info$summary %||% ""
    m <- regmatches(pub_info, regexpr("\\b(19|20)\\d{2}\\b", pub_info))
    if (length(m) == 1) as.integer(m) else NA_integer_
  }, error = function(e) NA_integer_)
  
  citations <- tryCatch(
    as.integer(p$inline_links$cited_by$total %||% 0),
    error = function(e) 0L
  )
  
  list(
    title     = p$title %||% NA_character_,
    year      = year,
    citations = citations,
    link      = p$link %||% NA_character_
  )
}

papers_df <- bind_rows(lapply(all_results, parse_paper)) |>
  filter(!is.na(year), year >= 1990, year <= as.integer(format(Sys.Date(), "%Y"))) |>
  mutate(year = as.integer(year))

cat(sprintf("Papers with valid year data: %d\n", nrow(papers_df)))

# ── Summarise by year ─────────────────────────────────────────────────────────

by_year <- papers_df |> filter(year!=2009)|>
  group_by(year) |>
  summarise(
    n_papers         = n(),
    total_citations  = sum(citations, na.rm = TRUE),
    median_citations = median(citations, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year)

print(by_year)

# ── Add cumulative columns ────────────────────────────────────────────────────

by_year <- by_year |>
  mutate(
    cumulative_papers    = cumsum(n_papers),
    cumulative_citations = cumsum(total_citations)
  )

# ── Plot 1: Cumulative papers over time ───────────────────────────────────────

p1 <- ggplot(by_year, aes(x = year, y = cumulative_papers)) +
  geom_area(fill = "#2563EB", alpha = 0.15) +
  geom_line(color = "#2563EB", linewidth = 1.1) +
  geom_point(color = "#2563EB", size = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title    = 'Cumulative "audit study" papers over time',
    subtitle = sprintf("n = %d papers retrieved from Google Scholar", nrow(papers_df)),
    x        = "Year",
    y        = "Cumulative number of papers",
    caption  = "Source: Google Scholar via SerpApi"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "gray40"),
    panel.grid.minor = element_blank()
  )

print(p1)
ggsave("scholar_cumulative_papers.png", p1, width = 9, height = 5, dpi = 150)

# ── Plot 2: Cumulative citations over time ────────────────────────────────────

p2 <- ggplot(by_year%>%filter(year!=2009), aes(x = year, y = cumulative_citations)) +
  geom_area(fill = "#16A34A", alpha = 0.15) +
  geom_line(color = "#16A34A", linewidth = 1.1) +
  geom_point(color = "#16A34A", size = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::comma) +
  labs(
    title    = 'Cumulative citations of "Political Science Audit Study" papers over time',
    subtitle = "Sum of all citations received by papers published up to each year",
    x        = "Year",
    y        = "Cumulative citations",
    caption  = "Source: Google Scholar via SerpApi"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "gray40"),
    panel.grid.minor = element_blank()
  )

print(p2)
ggsave("scholar_cumulative_citations.png", p2, width = 9, height = 5, dpi = 150)

# ── Save raw data ─────────────────────────────────────────────────────────────

write.csv(papers_df, "scholar_audit_study_papers.csv", row.names = FALSE)
write.csv(by_year,   "scholar_audit_study_by_year.csv",  row.names = FALSE)

cat("\nDone! Output files:\n")
cat("  scholar_cumulative_papers.png\n")
cat("  scholar_cumulative_citations.png\n")
cat("  scholar_audit_study_papers.csv\n")
cat("  scholar_audit_study_by_year.csv\n")
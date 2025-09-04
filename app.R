# UKHab Field Classifier — correct definitions per level + recording panel
suppressPackageStartupMessages({
  library(shiny); library(bslib)
  library(readr); library(dplyr); library(stringr); library(tibble)
  library(tools)    # for file_ext
})

DATA_DIR  <- "data"
HIER_FILE <- file.path(DATA_DIR, "ukhab_hierarchy.csv")
SEC_FILE  <- file.path(DATA_DIR, "secondary_codes.csv")
DEFS_FILE <- file.path(DATA_DIR, "definitions.csv")  # use the full descriptors you generated
OBS_FILE  <- file.path(DATA_DIR, "observations.csv")
PHOTO_DIR <- "www/photos"

dir.create(DATA_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create("www",     showWarnings = FALSE)
dir.create(PHOTO_DIR, showWarnings = FALSE)

`%||%` <- function(a, b) if (is.null(a) || is.na(a) || (is.character(a) && a == "")) b else a

# ---- load data ----
ukhab_h <- read_csv(HIER_FILE, show_col_types = FALSE) %>%
  mutate(across(everything(), ~ if (is.character(.)) str_trim(.) else .),
         l2_code = tolower(l2_code),
         l3_code = tolower(l3_code),
         l4_code = tolower(l4_code),
         l5_code = tolower(l5_code))

seccodes <- read_csv(SEC_FILE, show_col_types = FALSE) %>%
  mutate(essential = as.logical(essential),
         allowable_l2 = as.character(allowable_l2))

# definitions.csv should have these columns (extra columns are fine)
defs_cols <- c("code","title","definition","landscape_ecological_context",
               "synonyms","inclusions","exclusions","species","nvc_associations")
defs <- if (file.exists(DEFS_FILE)) {
  read_csv(DEFS_FILE, show_col_types = FALSE) %>%
    mutate(code = tolower(str_trim(as.character(code))))
} else {
  # Empty frame with expected columns to avoid errors
  as_tibble(setNames(replicate(length(defs_cols), character(), simplify = FALSE), defs_cols))
}

# L2 choices (unique, tidy, sorted by name)
l2_df <- ukhab_h %>%
  filter(!is.na(l2_code), l2_code != "", !is.na(l2_name), l2_name != "") %>%
  distinct(l2_code, l2_name) %>%
  arrange(l2_name)
l2_choices <- setNames(l2_df$l2_code, paste0(toupper(l2_df$l2_code), " — ", l2_df$l2_name))

# ---- helpers ----
most_specific_code <- function(l2,l3,l4,l5) {
  if (isTruthy(l5)) l5 else if (isTruthy(l4)) l4 else if (isTruthy(l3)) l3 else l2
}

# FIX: make sure we compare the *value* to the defs column (no NSE clash)
defs_row <- function(sel_code) {
  if (!isTruthy(sel_code) || !nrow(defs)) return(NULL)
  d <- defs %>% filter(.data[["code"]] == tolower(sel_code))
  if (!nrow(d)) return(NULL)
  d[1,]
}

# Build accordion from any available sections
accordion_sections <- function(d) {
  panels <- list()
  add_panel <- function(header, field) {
    val <- d[[field]] %||% ""
    if (isTRUE(nchar(val) > 0)) panels[[length(panels)+1]] <<- accordion_panel(header, div(val))
  }
  add_panel("Definition", "definition")
  add_panel("Landscape & ecological context", "landscape_ecological_context")
  add_panel("Synonyms", "synonyms")
  add_panel("Inclusions", "inclusions")
  add_panel("Exclusions", "exclusions")
  add_panel("Species", "species")
  add_panel("NVC associations", "nvc_associations")
  if (length(panels) == 0) return(helpText("No description available in definitions.csv for this code."))
  do.call(accordion, c(panels, list(id=NULL, multiple=TRUE)))
}

# Nice title for a code using the hierarchy names (fallback to defs$title)
title_for_code <- function(code) {
  if (!isTruthy(code)) return(code)
  nm <- ukhab_h %>%
    filter(l5_code==code | l4_code==code | l3_code==code | l2_code==code) %>%
    transmute(title = coalesce(l5_name, l4_name, l3_name, l2_name)) %>% pull(title) %>% .[1]
  if (!isTruthy(nm)) {
    d <- defs_row(code)
    nm <- if (!is.null(d) && isTruthy(d$title)) d$title else code
  }
  nm
}

# ---- UI ----
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  # TOP: two columns
  layout_columns(
    col_widths = c(4, 8),
    
    # LEFT: stepper + secondaries
    card(
      card_header(h5("Select habitat (primary)")),
      h6("Step 1 — Ecosystem (L2)"),
      radioButtons("l2", NULL, choices = l2_choices, inline = FALSE),
      
      h6("Step 2 — Broad habitat (L3)"),
      uiOutput("ui_l3"),
      
      h6("Step 3 — Habitat (L4)"),
      uiOutput("ui_l4"),
      
      h6("Step 4 — Sub-habitat (L5, optional)"),
      uiOutput("ui_l5"),
      
      hr(),
      card_header(h5("Secondary codes (filtered by L2)")),
      uiOutput("ui_sec_essential"),
      uiOutput("ui_sec_additional")
    ),
    
    # RIGHT: definitions for the *currently selected level*
    card(
      card_header(h5("Information & guidance")),
      uiOutput("ui_primary_header"),
      uiOutput("ui_primary_desc"),
      hr(),
      uiOutput("ui_secondary_desc")
    )
  ),
  
  # BOTTOM: full-width recording panel
  card(
    card_header(h5("Record site")),
    fluidRow(
      column(4, actionButton("btn_loc", "Get GPS")),
      column(4, textOutput("coord_txt")),
      column(4, textInput("accuracy", "GPS accuracy (m)", ""))
    ),
    fileInput("photo", "Photo (optional)", accept = c("image/*"),
              buttonLabel = "Choose / take photo"),
    textAreaInput("notes", "Notes", rows = 3, width = "100%"),
    hr(),
    strong("Primary code: "), textOutput("primary_code", inline = TRUE), br(),
    strong("Combined code: "), textOutput("combined_code", inline = TRUE), br(), br(),
    actionButton("save", "Save observation", class = "btn-primary"),
    span(id="save_status", style="margin-left:8px;"),
    br(), br(),
    downloadButton("dl_obs", "Download observations CSV")
  )
)

# ---- server ----
server <- function(input, output, session) {
  # Reset downstream selects when upstream changes
  observeEvent(input$l2, { updateSelectInput(session, "l3", selected = "") ; updateSelectInput(session, "l4", selected = "") ; updateSelectInput(session, "l5", selected = "") })
  observeEvent(input$l3, { updateSelectInput(session, "l4", selected = "") ; updateSelectInput(session, "l5", selected = "") })
  observeEvent(input$l4, { updateSelectInput(session, "l5", selected = "") })
  
  # L3 depends on L2
  l3_tbl <- reactive({
    req(input$l2)
    ukhab_h %>%
      filter(l2_code == input$l2) %>%
      filter(!is.na(l3_code), l3_code != "") %>%
      distinct(l3_code, l3_name) %>%
      arrange(l3_name)
  })
  output$ui_l3 <- renderUI({
    dat <- l3_tbl()
    validate(need(nrow(dat) > 0, "No L3 found for this L2."))
    selectInput("l3", NULL,
                choices = setNames(dat$l3_code, paste0(dat$l3_code, " — ", dat$l3_name)),
                width = "100%"
    )
  })
  
  # L4 depends on L3
  l4_tbl <- reactive({
    req(input$l2, input$l3)
    ukhab_h %>%
      filter(l2_code == input$l2, l3_code == input$l3) %>%
      filter(!is.na(l4_code), l4_code != "") %>%
      distinct(l4_code, l4_name) %>%
      arrange(l4_name)
  })
  output$ui_l4 <- renderUI({
    dat <- l4_tbl()
    if (nrow(dat) == 0) return(helpText("No Level 4 for this path."))
    selectInput("l4", NULL,
                choices = setNames(dat$l4_code, paste0(dat$l4_code, " — ", dat$l4_name)),
                width = "100%"
    )
  })
  
  # L5 depends on L4
  l5_tbl <- reactive({
    req(input$l2, input$l3)
    if (!isTruthy(input$l4))
      return(tibble(l5_code=character(), l5_name=character()))
    ukhab_h %>%
      filter(l2_code == input$l2, l3_code == input$l3, l4_code == input$l4) %>%
      filter(!is.na(l5_code), l5_code != "") %>%
      distinct(l5_code, l5_name) %>%
      arrange(l5_name)
  })
  output$ui_l5 <- renderUI({
    dat <- l5_tbl()
    if (nrow(dat) == 0) return(helpText("No Level 5 subtype for this habitat."))
    selectInput("l5", NULL,
                choices = setNames(dat$l5_code, paste0(dat$l5_code, " — ", dat$l5_name)),
                width = "100%"
    )
  })
  
  # Secondary codes allowed for chosen L2
  sec_for_l2 <- reactive({
    req(input$l2)
    pat <- paste0("(^|;)", stringr::fixed(input$l2), "~(;|$)")
    seccodes %>% filter(allowable_l2 == "all" | str_detect(allowable_l2, pat))
  })
  output$ui_sec_essential <- renderUI({
    dat <- sec_for_l2() %>% filter(essential) %>% arrange(code)
    if (!nrow(dat)) return(NULL)
    checkboxGroupInput("sec_ess", "Essential",
                       choices = setNames(dat$code, paste0(dat$code, " — ", dat$name)))
  })
  output$ui_sec_additional <- renderUI({
    dat <- sec_for_l2() %>% filter(!essential) %>% arrange(code)
    if (!nrow(dat)) return(NULL)
    checkboxGroupInput("sec_add", "Additional",
                       choices = setNames(dat$code, paste0(dat$code, " — ", dat$name)))
  })
  
  # Current primary selection (most specific of L2→L5)
  primary_code <- reactive({
    most_specific_code(input$l2, input$l3, input$l4, input$l5)
  })
  output$primary_code <- renderText(primary_code())
  
  # Combined code
  combined_code <- reactive({
    secs <- c(input$sec_ess, input$sec_add); secs <- secs[!is.na(secs) & secs != ""]
    if (length(secs)) paste0(primary_code(), ";", paste(secs, collapse=",")) else primary_code()
  })
  output$combined_code <- renderText(combined_code())
  
  # RIGHT PANE: Title + Definition for currently selected level (updates at each step)
  output$ui_primary_header <- renderUI({
    pc <- primary_code(); if (!isTruthy(pc)) return(NULL)
    tags$p(tags$b(paste0(pc, " — ", title_for_code(pc))))
  })
  output$ui_primary_desc <- renderUI({
    d <- defs_row(primary_code())
    if (is.null(d)) return(helpText("No description found in definitions.csv for this code."))
    accordion_sections(d)
  })
  
  # Secondary descriptions (optional, appear below)
  output$ui_secondary_desc <- renderUI({
    secs <- c(input$sec_ess, input$sec_add); secs <- secs[!is.na(secs) & secs != ""]
    if (!length(secs)) return(NULL)
    panels <- lapply(secs, function(sc) {
      d <- defs_row(sc)
      head <- paste0(sc, " — ", title_for_code(sc))
      body <- if (is.null(d)) div(em("No description found.")) else accordion_sections(d)
      accordion_panel(head, body)
    })
    do.call(accordion, c(panels, list(id=NULL, multiple=TRUE)))
  })
  
  # --- GPS & saving ---
  observeEvent(input$btn_loc, { session$sendCustomMessage("get_location", list()) })
  coords <- reactiveVal(NULL)
  observeEvent(input$geoloc, { coords(input$geoloc) })
  output$coord_txt <- renderText({
    c <- coords(); if (is.null(c)) return("lat, lon = ?")
    sprintf("%.6f, %.6f (±%sm)", c$lat, c$lon, c$acc %||% "?")
  })
  
  observeEvent(input$save, {
    photo_rel <- NA_character_
    if (!is.null(input$photo) && file.exists(input$photo$datapath)) {
      ext <- file_ext(input$photo$name) %||% "jpg"
      fname <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", primary_code(), ".", ext)
      dest  <- file.path(PHOTO_DIR, fname)
      file.copy(input$photo$datapath, dest, overwrite = FALSE)
      photo_rel <- file.path("photos", fname)
    }
    c <- coords()
    row <- tibble(
      datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      l2 = input$l2, l3 = input$l3 %||% NA_character_, l4 = input$l4 %||% NA_character_, l5 = input$l5 %||% NA_character_,
      primary = primary_code(),
      secondary = paste(c(input$sec_ess, input$sec_add), collapse = ","),
      combined = combined_code(),
      lat = if (!is.null(c)) c$lat else NA_real_,
      lon = if (!is.null(c)) c$lon else NA_real_,
      gps_accuracy_m = input$accuracy %||% NA_character_,
      notes = input$notes %||% "",
      photo = photo_rel
    )
    if (!file.exists(OBS_FILE)) write_csv(row, OBS_FILE) else {
      old <- read_csv(OBS_FILE, show_col_types = FALSE)
      write_csv(bind_rows(old, row), OBS_FILE)
    }
    showNotification("Saved ✔️", type = "message", duration = 2)
  })
  
  output$dl_obs <- downloadHandler(
    filename = function() paste0("ukhab_observations_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) if (file.exists(OBS_FILE)) file.copy(OBS_FILE, file) else write_csv(tibble(), file)
  )
}

# Browser geolocation
geo_js <- "
Shiny.addCustomMessageHandler('get_location', function(msg) {
  if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(function(pos){
      Shiny.setInputValue('geoloc', {lat: pos.coords.latitude, lon: pos.coords.longitude, acc: pos.coords.accuracy}, {priority: 'event'});
    }, function(err){
      Shiny.setInputValue('geoloc', null, {priority: 'event'});
    }, {enableHighAccuracy: true, timeout: 15000, maximumAge: 0});
  } else {
    Shiny.setInputValue('geoloc', null, {priority: 'event'});
  }
});
"

shinyApp(
  ui = tagList(tags$script(HTML(geo_js)), ui),
  server = server
)

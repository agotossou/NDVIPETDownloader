# ===========================
# Integrated Shiny App - Three Downloaders
# Tab 1: NDVI eVIIRS Pentadal
# Tab 2: NDVI Dekadal
# Tab 3: PET Daily
# ===========================
library(shiny)

# ===========================
# Helper Functions
# ===========================

# ---------- Pentad Functions ----------
next_pentad <- function(y, p) {
  if (p < 72) return(list(year=y, pentad=p+1))
  list(year=y+1, pentad=1)
}
pentad_leq <- function(y1,p1,y2,p2) {
  if(y1<y2) return(TRUE)
  if(y1>y2) return(FALSE)
  return(p1<=p2)
}
parse_pentad_filename <- function(fname) {
  m <- regexec("^wa(\\d{2})(\\d{2})\\.zip$", fname)
  mm <- regmatches(fname, m)[[1]]
  if(length(mm)==3) list(year=2000+as.integer(mm[2]), pentad=as.integer(mm[3])) else NULL
}
pentad_label <- function(y,p) sprintf("Pentad %02d, %d", p, y)

# ---------- Dekad Functions ----------
next_dekad <- function(y,d) {
  if (d < 36) return(list(year=y, dekad=d+1))
  list(year=y+1, dekad=1)
}
dekad_leq <- function(y1,d1,y2,d2) {
  if(y1<y2) return(TRUE)
  if(y1>y2) return(FALSE)
  return(d1<=d2)
}
parse_dekad_filename <- function(fname) {
  m <- regexec("^wa(\\d{4})(\\d{2})\\.zip$", fname)
  mm <- regmatches(fname, m)[[1]]
  if(length(mm)==3) list(year=as.integer(mm[2]), dekad=as.integer(mm[3])) else NULL
}
dekad_label <- function(y,d) sprintf("Dekad %02d, %d", d, y)

# ---------- PET Daily Functions ----------
next_day <- function(y,m,d) {
  dt <- as.Date(sprintf("%04d-%02d-%02d",y,m,d))
  ndt <- dt + 1
  list(year=as.integer(format(ndt,"%Y")),
       month=as.integer(format(ndt,"%m")),
       day=as.integer(format(ndt,"%d")))
}
is_before_or_equal <- function(y1,m1,d1,y2,m2,d2) {
  dt1 <- as.Date(sprintf("%04d-%02d-%02d",y1,m1,d1))
  dt2 <- as.Date(sprintf("%04d-%02d-%02d",y2,m2,d2))
  dt1 <= dt2
}
parse_pet_filename <- function(fname) {
  m <- regexec("^et(\\d{2})(\\d{2})(\\d{2})\\.tar\\.gz$", fname)
  mm <- regmatches(fname, m)[[1]]
  if(length(mm)==4) list(year=2000+as.integer(mm[2]), month=as.integer(mm[3]), day=as.integer(mm[4])) else NULL
}

# ---------------------------
# Generic Remote File Listing
# ---------------------------
list_remote_files_generic <- function(base_url, pattern) {
  tryCatch({
    txt <- readLines(base_url, warn=FALSE)
    t <- paste(txt, collapse="\n")
    matches <- gregexpr(pattern, t)
    if(matches[[1]][1]==-1) return(character(0))
    unique(regmatches(t,matches)[[1]])
  }, error=function(e) NULL)
}

# ===========================
# UI
# ===========================
ui <- fluidPage(
  titlePanel("NDVI & PET Downloaders"),
  tabsetPanel(
    id="tabs",

    # ---------- Tab 1: Pentadal ----------
    tabPanel("NDVI eVIIRS Pentadal",
             sidebarLayout(
               sidebarPanel(
                 tags$div(
                   style="border:1px solid #e0e0e0; border-radius:8px; padding:12px; margin-bottom:12px; box-shadow:0 1px 3px rgba(0,0,0,0.04);",
                   tags$h4("Dataset information", style="margin-top:0; margin-bottom:6px;"),
                   uiOutput("preview_text_pentad"),
                   actionButton("refresh_preview_pentad","Refresh Preview"),
                   actionButton("open_url_pentad","Open Dataset Website",style="margin-left:8px;")
                 ),
                 numericInput("start_year_pentad","Start Year:",2025,2012,2050),
                 numericInput("start_pentad","Start Pentad:",1,1,72),
                 numericInput("end_year_pentad","End Year:",2025,2012,2050),
                 numericInput("end_pentad","End Pentad:",1,1,72),
                 uiOutput("validation_error_ui_pentad"),
                 textInput("outdir_pentad","Output Directory:",value="C:/Users/gaded/Documents/fews_tools_WS/ProgramSettings/Data/Climate/"),
                 radioButtons("overwrite_pentad","If file exists:",choices=c("Skip","Overwrite"),selected="Overwrite"),
                 actionButton("download_pentad","Start Download")
               ),
               mainPanel(verbatimTextOutput("log_pentad"))
             )
    ),

    # ---------- Tab 2: Dekadal ----------
    tabPanel("NDVI eVMOD/eVIIRS Dekadal",
             sidebarLayout(
               sidebarPanel(
                 tags$div(
                   style="border:1px solid #e0e0e0; border-radius:8px; padding:12px; margin-bottom:12px; box-shadow:0 1px 3px rgba(0,0,0,0.04);",
                   tags$h4("Dataset information", style="margin-top:0; margin-bottom:6px;"),
                   uiOutput("preview_text_dekad"),
                   actionButton("refresh_preview_dekad","Refresh Preview"),
                   actionButton("open_url_dekad","Open Dataset Website",style="margin-left:8px;")
                 ),
                 numericInput("start_year_dekad","Start Year:",2025,2003,2050),
                 numericInput("start_dekad","Start Dekad:",1,1,36),
                 numericInput("end_year_dekad","End Year:",2025,2003,2050),
                 numericInput("end_dekad","End Dekad:",1,1,36),
                 uiOutput("validation_error_ui_dekad"),
                 textInput("outdir_dekad","Output Directory:",value="C:/Users/gaded/Documents/fews_tools_WS/ProgramSettings/Data/Climate/"),
                 radioButtons("overwrite_dekad","If file exists:",choices=c("Skip","Overwrite"),selected="Overwrite"),
                 actionButton("download_dekad","Start Download")
               ),
               mainPanel(verbatimTextOutput("log_dekad"))
             )
    ),

    # ---------- Tab 3: PET Daily ----------
    tabPanel("PET Daily",
             sidebarLayout(
               sidebarPanel(
                 tags$div(
                   style="border:1px solid #e0e0e0; border-radius:8px; padding:12px; margin-bottom:12px; box-shadow:0 1px 3px rgba(0,0,0,0.04);",
                   tags$h4("Dataset information", style="margin-top:0; margin-bottom:6px;"),
                   uiOutput("preview_text_pet"),
                   actionButton("refresh_preview_pet","Refresh Preview"),
                   actionButton("open_url_pet","Open Dataset Website",style="margin-left:8px;")
                 ),
                 numericInput("start_year_pet","Start Year:",2025,2001,2050),
                 numericInput("start_month_pet","Start Month:",1,1,12),
                 numericInput("start_day_pet","Start Day:",1,1,31),
                 numericInput("end_year_pet","End Year:",2025,2001,2050),
                 numericInput("end_month_pet","End Month:",1,1,12),
                 numericInput("end_day_pet","End Day:",1,1,31),
                 uiOutput("validation_error_ui_pet"),
                 textInput("outdir_pet","Output Directory:",value="C:/Users/gaded/Documents/fews_tools_WS/ProgramSettings/Data/Climate/"),
                 radioButtons("overwrite_pet","If file exists:",choices=c("Skip","Overwrite"),selected="Overwrite"),
                 actionButton("download_pet","Start Download")
               ),
               mainPanel(verbatimTextOutput("log_pet"))
             )
    )
  )
)

# ===========================
# SERVER
# ===========================
server <- function(input, output, session){

  # ---------- Pentad ----------
  base_url_pentad <- "https://example.com/pentad/"
  remote_files_pentad <- reactiveVal(NULL)
  avail_range_pentad <- reactiveVal(list(first=NULL,last=NULL,note=NULL))

  update_remote_listing_pentad <- function(){
    files <- list_remote_files_generic(base_url_pentad,"wa\\d{2}\\d{2}\\.zip")
    if(is.null(files)){remote_files_pentad(NULL); avail_range_pentad(list(first=NULL,last=NULL,note="Unable to reach server.")); return(NULL)}
    remote_files_pentad(files)
    parsed <- lapply(files,parse_pentad_filename)
    parsed <- Filter(Negate(is.null), parsed)
    if(length(parsed)==0){avail_range_pentad(list(first=NULL,last=NULL,note="No files found.")); return(NULL)}
    df <- do.call(rbind,lapply(parsed,function(x)c(year=x$year,pentad=x$pentad)))
    df <- as.data.frame(df); df$year<-as.integer(df$year); df$pentad<-as.integer(df$pentad)
    ord <- order(df$year,df$pentad)
    first_row <- df[ord[1],,drop=FALSE]; last_row <- df[ord[nrow(df)],,drop=FALSE]
    avail_range_pentad(list(first=list(year=first_row$year,pentad=first_row$pentad),
                            last=list(year=last_row$year,pentad=last_row$pentad),
                            note=NULL))
  }

  observe({ update_remote_listing_pentad() })
  observeEvent(input$refresh_preview_pentad,{ update_remote_listing_pentad() })
  observeEvent(input$open_url_pentad,{ tryCatch({browseURL(base_url_pentad)}, error=function(e){showNotification("Cannot open browser.",type="error")}) })
  output$preview_text_pentad <- renderUI({
    ar <- avail_range_pentad()
    if(!is.null(ar$note)){tags$div(tags$strong("Dataset: "),"NDVI eVIIRS Pentadal",tags$br(),tags$strong("Available: "),tags$span(style="color:#d35400;",ar$note))
    } else {tags$div(tags$strong("Dataset: "),"NDVI eVIIRS Pentadal",tags$br(),tags$strong("First available: "),pentad_label(ar$first$year,ar$first$pentad),tags$br(),tags$strong("Latest available: "),pentad_label(ar$last$year,ar$last$pentad))}
  })

  logtext_pentad <- reactiveVal("")
  appendLog_pentad <- function(txt){ logtext_pentad(paste0(logtext_pentad(), if(nzchar(logtext_pentad())) "\n" else "", txt)) }
  output$log_pentad <- renderText({ logtext_pentad() })

  validation_msg_pentad <- reactiveVal(NULL)
  output$validation_error_ui_pentad <- renderUI({msg <- validation_msg_pentad(); if(is.null(msg)) return(NULL); tags$div(style="color:white;background-color:#c0392b;padding:8px;border-radius:4px;margin-bottom:8px;", tags$strong("Error: "), tags$span(msg))})

  validate_inputs_pentad <- function(){
    # Build start and end dates for pentad (approximate as first day of pentad)
    sy <- input$start_year_pentad; sp <- input$start_pentad
    ey <- input$end_year_pentad; ep <- input$end_pentad
    cur_date <- Sys.Date()

    # Check numeric validity
    if(!is.numeric(sy) || !is.numeric(sp) || !is.numeric(ey) || !is.numeric(ep)) return("Year or pentad inputs must be numeric.")
    if(sp < 1 || sp > 72 || ep < 1 || ep > 72) return("Pentad must be between 1 and 72.")
    if(sy < 2012 || sy > 2050 || ey < 2012 || ey > 2050) return("Year must be between 2012 and 2050.")

    # Check order
    if(!pentad_leq(sy,sp,ey,ep)) return("Start date must be earlier than or equal to end date.")

    # Approximate start/end dates
    start_date <- as.Date(sprintf("%04d-01-01", sy)) + (sp-1)*5
    end_date <- as.Date(sprintf("%04d-01-01", ey)) + (ep-1)*5
    if(end_date > cur_date) return("Years cannot be in the future.")

    return(NULL)
  }

  observeEvent(input$download_pentad,{
    msg <- validate_inputs_pentad(); if(!is.null(msg)){validation_msg_pentad(msg); return()} else validation_msg_pentad(NULL)
    outdir <- input$outdir_pentad
    if(is.null(outdir)||outdir==""||!dir.exists(outdir)){validation_msg_pentad("Output directory does not exist. Please specify a valid folder before downloading."); return()} else validation_msg_pentad(NULL)
    overwrite_mode <- input$overwrite_pentad
    files <- list()
    cur <- list(year=input$start_year_pentad,pentad=input$start_pentad)
    end <- list(year=input$end_year_pentad,pentad=input$end_pentad)
    while(pentad_leq(cur$year,cur$pentad,end$year,end$pentad)){
      fname <- sprintf("wa%02d%02d.zip",cur$year%%100,cur$pentad)
      files[[fname]] <- paste0(base_url_pentad,fname)
      cur <- next_pentad(cur$year,cur$pentad)
    }
    downloaded <- 0; skipped <- 0; failed <- 0; n <- length(files)
    withProgress(message="Downloading Pentad files...", value=0,{
      if(n==0){appendLog_pentad("No files to download."); return()}
      for(i in seq_along(files)){
        fname <- names(files)[i]; url <- files[[fname]]; destfile <- file.path(outdir,fname)
        action <- if(file.exists(destfile) && overwrite_mode=="Skip") "skipped" else "download"
        if(action=="download"){
          success <- tryCatch({ download.file(url,destfile,mode="wb",quiet=TRUE); TRUE }, error=function(e){appendLog_pentad(paste("Failed:",fname,"-",conditionMessage(e))); FALSE})
          if(isTRUE(success)&&(!file.exists(destfile)||file.info(destfile)$size==0)){success<-FALSE; appendLog_pentad(paste("Failed (empty/missing):",fname))}
          if(success){appendLog_pentad(paste("Downloaded:",fname)); downloaded <- downloaded+1} else failed<-failed+1
        } else {appendLog_pentad(paste("Skipped:",fname)); skipped<-skipped+1}
        incProgress(1/n,detail=paste(action,":",fname)); Sys.sleep(0.01)
      }
    })
    appendLog_pentad(paste0("Job summary: ",downloaded," files downloaded, ",skipped," files skipped, ",failed," files failed."))
  })

  # ---------- Dekadal ----------
  base_url_dekad <- "https://example.com/dekad/"
  remote_files_dekad <- reactiveVal(NULL)
  avail_range_dekad <- reactiveVal(list(first=NULL,last=NULL,note=NULL))

  update_remote_listing_dekad <- function(){
    files <- list_remote_files_generic(base_url_dekad,"wa\\d{4}\\d{2}\\.zip")
    if(is.null(files)){remote_files_dekad(NULL); avail_range_dekad(list(first=NULL,last=NULL,note="Unable to reach server.")); return(NULL)}
    remote_files_dekad(files)
    parsed <- lapply(files,parse_dekad_filename)
    parsed <- Filter(Negate(is.null), parsed)
    if(length(parsed)==0){avail_range_dekad(list(first=NULL,last=NULL,note="No files found.")); return(NULL)}
    df <- do.call(rbind,lapply(parsed,function(x)c(year=x$year,dekad=x$dekad)))
    df <- as.data.frame(df); df$year<-as.integer(df$year); df$dekad<-as.integer(df$dekad)
    ord <- order(df$year,df$dekad)
    first_row <- df[ord[1],,drop=FALSE]; last_row <- df[ord[nrow(df)],,drop=FALSE]
    avail_range_dekad(list(first=list(year=first_row$year,dekad=first_row$dekad),
                           last=list(year=last_row$year,dekad=last_row$dekad),
                           note=NULL))
  }

  observe({ update_remote_listing_dekad() })
  observeEvent(input$refresh_preview_dekad,{ update_remote_listing_dekad() })
  observeEvent(input$open_url_dekad,{ tryCatch({browseURL(base_url_dekad)}, error=function(e){showNotification("Cannot open browser.",type="error")}) })
  output$preview_text_dekad <- renderUI({
    ar <- avail_range_dekad()
    if(!is.null(ar$note)){tags$div(tags$strong("Dataset: "),"NDVI Dekadal",tags$br(),tags$strong("Available: "),tags$span(style="color:#d35400;",ar$note))
    } else {tags$div(tags$strong("Dataset: "),"NDVI Dekadal",tags$br(),tags$strong("First available: "),dekad_label(ar$first$year,ar$first$dekad),tags$br(),tags$strong("Latest available: "),dekad_label(ar$last$year,ar$last$dekad))}
  })

  logtext_dekad <- reactiveVal("")
  appendLog_dekad <- function(txt){ logtext_dekad(paste0(logtext_dekad(), if(nzchar(logtext_dekad())) "\n" else "", txt)) }
  output$log_dekad <- renderText({ logtext_dekad() })

  validation_msg_dekad <- reactiveVal(NULL)
  output$validation_error_ui_dekad <- renderUI({msg <- validation_msg_dekad(); if(is.null(msg)) return(NULL); tags$div(style="color:white;background-color:#c0392b;padding:8px;border-radius:4px;margin-bottom:8px;", tags$strong("Error: "), tags$span(msg))})

  validate_inputs_dekad <- function(){
    sy <- input$start_year_dekad; sd <- input$start_dekad
    ey <- input$end_year_dekad; ed <- input$end_dekad
    cur_date <- Sys.Date()

    if(!is.numeric(sy) || !is.numeric(sd) || !is.numeric(ey) || !is.numeric(ed)) return("Year or dekad inputs must be numeric.")
    if(sd < 1 || sd > 36 || ed < 1 || ed > 36) return("Dekad must be between 1 and 36.")
    if(sy < 2003 || sy > 2050 || ey < 2003 || ey > 2050) return("Year must be between 2003 and 2050.")

    if(!dekad_leq(sy,sd,ey,ed)) return("Start date must be earlier than or equal to end date.")

    # Approximate start/end dates
    start_date <- as.Date(sprintf("%04d-01-01", sy)) + (sd-1)*10
    end_date <- as.Date(sprintf("%04d-01-01", ey)) + (ed-1)*10
    if(end_date > cur_date) return("Years cannot be in the future.")

    return(NULL)
  }

  observeEvent(input$download_dekad,{
    msg <- validate_inputs_dekad(); if(!is.null(msg)){validation_msg_dekad(msg); return()} else validation_msg_dekad(NULL)
    outdir <- input$outdir_dekad
    if(is.null(outdir)||outdir==""||!dir.exists(outdir)){validation_msg_dekad("Output directory does not exist. Please specify a valid folder before downloading."); return()} else validation_msg_dekad(NULL)
    overwrite_mode <- input$overwrite_dekad
    files <- list()
    cur <- list(year=input$start_year_dekad, dekad=input$start_dekad)
    end <- list(year=input$end_year_dekad, dekad=input$end_dekad)
    while(dekad_leq(cur$year,cur$dekad,end$year,end$dekad)){
      fname <- sprintf("wa%04d%02d.zip", cur$year, cur$dekad)
      files[[fname]] <- paste0(base_url_dekad,fname)
      cur <- next_dekad(cur$year,cur$dekad)
    }
    downloaded <- 0; skipped <- 0; failed <- 0; n <- length(files)
    withProgress(message="Downloading Dekad files...", value=0,{
      if(n==0){appendLog_dekad("No files to download."); return()}
      for(i in seq_along(files)){
        fname <- names(files)[i]; url <- files[[fname]]; destfile <- file.path(outdir,fname)
        action <- if(file.exists(destfile) && overwrite_mode=="Skip") "skipped" else "download"
        if(action=="download"){
          success <- tryCatch({ download.file(url,destfile,mode="wb",quiet=TRUE); TRUE }, error=function(e){appendLog_dekad(paste("Failed:",fname,"-",conditionMessage(e))); FALSE})
          if(isTRUE(success)&&(!file.exists(destfile)||file.info(destfile)$size==0)){success<-FALSE; appendLog_dekad(paste("Failed (empty/missing):",fname))}
          if(success){appendLog_dekad(paste("Downloaded:",fname)); downloaded <- downloaded+1} else failed<-failed+1
        } else {appendLog_dekad(paste("Skipped:",fname)); skipped<-skipped+1}
        incProgress(1/n,detail=paste(action,":",fname)); Sys.sleep(0.01)
      }
    })
    appendLog_dekad(paste0("Job summary: ",downloaded," files downloaded, ",skipped," files skipped, ",failed," files failed."))
  })

  # ---------- PET Daily ----------
  base_url_pet <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/global/daily/pet/downloads/daily/"
  remote_files_pet <- reactiveVal(NULL)
  avail_range_pet <- reactiveVal(list(first=NULL,last=NULL,note=NULL))

  update_remote_listing_pet <- function(){
    files <- list_remote_files_generic(base_url_pet,"et\\d{6}\\.tar\\.gz")
    if(is.null(files)){remote_files_pet(NULL); avail_range_pet(list(first=NULL,last=NULL,note="Unable to reach server.")); return(NULL)}
    remote_files_pet(files)
    parsed <- lapply(files,parse_pet_filename)
    parsed <- Filter(Negate(is.null), parsed)
    if(length(parsed)==0){avail_range_pet(list(first=NULL,last=NULL,note="No files found.")); return(NULL)}
    df <- do.call(rbind,lapply(parsed,function(x)c(year=x$year,month=x$month,day=x$day)))
    df <- as.data.frame(df); df$year<-as.integer(df$year); df$month<-as.integer(df$month); df$day<-as.integer(df$day)
    ord <- order(df$year,df$month,df$day)
    first_row <- df[ord[1],,drop=FALSE]; last_row <- df[ord[nrow(df)],,drop=FALSE]
    avail_range_pet(list(first=list(year=first_row$year,month=first_row$month,day=first_row$day),
                         last=list(year=last_row$year,month=last_row$month,day=last_row$day),
                         note=NULL))
  }

  observe({ update_remote_listing_pet() })
  observeEvent(input$refresh_preview_pet,{ update_remote_listing_pet() })
  observeEvent(input$open_url_pet,{ tryCatch({browseURL(base_url_pet)}, error=function(e){showNotification("Cannot open browser.",type="error")}) })

  output$preview_text_pet <- renderUI({
    ar <- avail_range_pet()
    if(!is.null(ar$note)){tags$div(tags$strong("Dataset: "),"PET Daily",tags$br(),tags$strong("Available: "),tags$span(style="color:#d35400;",ar$note))
    } else {tags$div(tags$strong("Dataset: "),"PET Daily",tags$br(),tags$strong("First available: "),sprintf("%04d-%02d-%02d",ar$first$year,ar$first$month,ar$first$day),tags$br(),tags$strong("Latest available: "),sprintf("%04d-%02d-%02d",ar$last$year,ar$last$month,ar$last$day))}
  })

  logtext_pet <- reactiveVal("")
  appendLog_pet <- function(txt){ logtext_pet(paste0(logtext_pet(), if(nzchar(logtext_pet())) "\n" else "", txt)) }
  output$log_pet <- renderText({ logtext_pet() })

  validation_msg_pet <- reactiveVal(NULL)
  output$validation_error_ui_pet <- renderUI({msg<-validation_msg_pet(); if(is.null(msg)) return(NULL); tags$div(style="color:white;background-color:#c0392b;padding:8px;border-radius:4px;margin-bottom:8px;", tags$strong("Error: "), tags$span(msg))})

  validate_inputs_pet <- function(){
    start_date <- tryCatch(as.Date(sprintf("%04d-%02d-%02d",input$start_year_pet,input$start_month_pet,input$start_day_pet)), error=function(e) NA)
    end_date <- tryCatch(as.Date(sprintf("%04d-%02d-%02d",input$end_year_pet,input$end_month_pet,input$end_day_pet)), error=function(e) NA)
    cur_date <- Sys.Date()
    if(is.na(start_date)) return("Start date is invalid.")
    if(is.na(end_date)) return("End date is invalid.")
    if(start_date>end_date) return("Start date must be earlier than or equal to end date.")
    if(end_date>cur_date) return("Years cannot be in the future.")
    return(NULL)
  }

  observeEvent(input$download_pet,{
    msg <- validate_inputs_pet(); if(!is.null(msg)){validation_msg_pet(msg); return()} else validation_msg_pet(NULL)
    outdir <- input$outdir_pet
    if(is.null(outdir)||outdir==""||!dir.exists(outdir)){validation_msg_pet("Output directory does not exist. Please specify a valid folder before downloading."); return()} else validation_msg_pet(NULL)
    overwrite_mode <- input$overwrite_pet
    files <- list()
    cur <- list(year=input$start_year_pet, month=input$start_month_pet, day=input$start_day_pet)
    end <- list(year=input$end_year_pet, month=input$end_month_pet, day=input$end_day_pet)
    while(is_before_or_equal(cur$year,cur$month,cur$day,end$year,end$month,end$day)){
      fname <- sprintf("et%02d%02d%02d.tar.gz", cur$year%%100, cur$month, cur$day)
      files[[fname]] <- paste0(base_url_pet,fname)
      cur <- next_day(cur$year,cur$month,cur$day)
    }
    downloaded <- 0; skipped <- 0; failed <- 0; n <- length(files)
    withProgress(message="Downloading PET Daily files...", value=0,{
      if(n==0){appendLog_pet("No files to download."); return()}
      for(i in seq_along(files)){
        fname <- names(files)[i]; url <- files[[fname]]; destfile <- file.path(outdir,fname)
        action <- if(file.exists(destfile) && overwrite_mode=="Skip") "skipped" else "download"
        if(action=="download"){
          success <- tryCatch({ download.file(url,destfile,mode="wb",quiet=TRUE); TRUE }, error=function(e){appendLog_pet(paste("Failed:",fname,"-",conditionMessage(e))); FALSE})
          if(isTRUE(success)&&(!file.exists(destfile)||file.info(destfile)$size==0)){success<-FALSE; appendLog_pet(paste("Failed (empty/missing):",fname))}
          if(success){appendLog_pet(paste("Downloaded:",fname)); downloaded<-downloaded+1} else failed<-failed+1
        } else {appendLog_pet(paste("Skipped:",fname)); skipped<-skipped+1}
        incProgress(1/n,detail=paste(action,":",fname)); Sys.sleep(0.01)
      }
    })
    appendLog_pet(paste0("Job summary: ",downloaded," files downloaded, ",skipped," files skipped, ",failed," files failed."))
  })
}

# ===========================
# Run the App
# ===========================
shinyApp(ui, server)

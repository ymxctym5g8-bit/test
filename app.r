# Shiny-Runner für R-Skripte
# Benötigt: install.packages(c("shiny", "callr"))
#
# Jedes Skript in ./prozesse/ läuft in einem Hintergrundprozess.
# Variante A (bestehendes Skript, unverändert):
#   Die Variable `stichtag` ist vorbelegt. Der Name der Tabelle, die das Skript
#   befüllt, wird links bei "Tabellen-Variable" eingetragen und live angezeigt.
# Variante B (optional): Skript definiert run(stichtag, emit) und ruft
#   emit(<data.frame>) für neue Zeilen auf. Dann Tabellen-Variable leer lassen.

library(shiny)
library(callr)

script_dir <- file.path(normalizePath(getwd()), "prozesse")
script_files <- list.files(script_dir, pattern = "\\.r$", ignore.case = TRUE, full.names = TRUE)
prozesse <- setNames(script_files, tools::file_path_sans_ext(basename(script_files)))

# Läuft im Hintergrundprozess (muss komplett eigenständig sein)
runner <- function(script, stichtag, out_file, var) {
  write_out <- function(df) {
    tmp <- paste0(out_file, ".tmp")   # erst temporär schreiben ...
    saveRDS(df, tmp)
    file.rename(tmp, out_file)        # ... dann atomar austauschen
  }

  # Variante B: Skript mit run()-Funktion und emit()
  collected <- NULL
  emit <- function(rows) {
    collected <<- rbind(collected, as.data.frame(rows))
    write_out(collected)
  }

  env <- new.env(parent = globalenv())
  env$stichtag <- stichtag            # im Skript direkt als Variable verfügbar

  
  # Variante A: bestehendes Skript unverändert. Jede Zuweisung an die
  # Tabellen-Variable (z. B. ergebnis <- rbind(ergebnis, ...)) wird live gespiegelt.
  if (nzchar(var)) {
    store <- NULL
    makeActiveBinding(var, function(v) {
      if (missing(v)) return(store)
      store <<- v
      if (is.data.frame(v)) write_out(v)
      invisible(v)
    }, env)
  }

  sys.source(script, envir = env)
  if (is.function(env$run)) env$run(stichtag = stichtag, emit = emit)
  invisible(TRUE)
}

ui <- fluidPage(
  titlePanel("Prozess-Runner"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("prozess", "Prozess", choices = names(prozesse)),
      dateInput("stichtag", "Stichtag", value = Sys.Date(),
                format = "dd.mm.yyyy", weekstart = 1, language = "de"),
      textInput("var", "Tabellen-Variable", value = "ergebnis"),
      fluidRow(
        column(6, actionButton("start", "Starten", class = "btn-primary", width = "100%")),
        column(6, actionButton("stop", "Abbrechen", width = "100%"))
      ),
      hr(),
      textOutput("status")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Ergebnis", br(), tableOutput("tabelle"))      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(proc = NULL, out_file = NULL, log_file = NULL, info = "Bereit.")

  kill_proc <- function() {
    if (!is.null(rv$proc) && rv$proc$is_alive()) rv$proc$kill()
  }

  observeEvent(input$start, {
    req(input$prozess, input$stichtag)
    kill_proc()

    rv$out_file <- tempfile(fileext = ".rds")
    rv$info <- sprintf("Läuft: %s (Stichtag %s)", input$prozess,
                       format(input$stichtag, "%d.%m.%Y"))

    rv$proc <- r_bg(
      runner,
      args = list(script = prozesse[[input$prozess]],
                  stichtag = input$stichtag,
                  out_file = rv$out_file,
                  var = trimws(input$var)),
      stdout = rv$log_file,
      stderr = "2>&1"
    )
  })

  observeEvent(input$stop, {
    if (!is.null(rv$proc) && rv$proc$is_alive()) {
      rv$proc$kill()
      rv$info <- "Abgebrochen."
    }
  })

  session$onSessionEnded(function() {
    isolate(kill_proc())
  })

  # Pollt alle 500 ms, solange der Prozess läuft (plus einmal nach Ende)
  daten <- reactive({
    req(rv$proc, rv$out_file)
    alive <- rv$proc$is_alive()
    if (alive) invalidateLater(500, session)

    df <- if (file.exists(rv$out_file)) {
      tryCatch(readRDS(rv$out_file), error = function(e) NULL)
    }

    if (!alive && startsWith(isolate(rv$info), "Läuft")) {
      status <- rv$proc$get_exit_status()
      rv$info <- if (identical(status, 0L)) {
        sprintf("Fertig. %s Zeilen.", if (is.null(df)) 0 else nrow(df))
      } else {
        "Fehler – siehe Tab „Log“."
      }
    }
    df
  })

  output$status <- renderText({
    daten()  # hält Status aktuell
    rv$info
  })

  output$tabelle <- renderTable({
    df <- daten()
    req(df)
    df[rev(seq_len(nrow(df))), , drop = FALSE]  # neueste Zeilen oben
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$log <- renderText({
    daten()
    req(rv$log_file)
    if (file.exists(rv$log_file)) paste(readLines(rv$log_file, warn = FALSE), collapse = "\n") else ""
  })
}

shinyApp(ui, server)

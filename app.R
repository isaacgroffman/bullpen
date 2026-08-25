# =============================================================================
# Bullpen Central — TrackMan practice bullpens with Edgertronic + AWRE video.
#
# Data: Supabase `pitches` (filled by trackman_practice_ingest.R), polled
# every minute. Clicking a pitch opens a modal with:
#   * Edgertronic clip(s)  — SAS URL minted at click time (trackman_api.R)
#   * AWRE angles          — fetched at click time from
#                            /team/{team}/clip/{sl_key}/angles, played via
#                            hls.js (AWRE serves HLS .m3u8 streams)
# Secrets (Connect Cloud Variables / ~/.Renviron): SB_DB_HOST, SB_DB_USER,
# SB_DB_PASS, TM_CLIENT_ID, TM_SECRET, AWRE_KEY.
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)
library(gt)
library(DT)
library(httr2)
library(xml2)
library(pool)
library(RPostgres)
library(DBI)

source("R/helpers.R", local = TRUE)
source("R/trackman_api.R", local = TRUE)   # edger_urls()
source("R/supabase.R", local = TRUE)

AWRE_TEAM <- "73715"
AWRE_API  <- "https://api.awresports.com/api/exchange/v2"

# Fetch AWRE camera angles for one pitch (sl_ key). Returns a named character
# vector: names = perspective labels, values = HLS urls. NULL on any failure.
awre_angles <- function(ppdk) {
  key <- Sys.getenv("AWRE_KEY")
  if (!nzchar(key) || is.na(ppdk) || !nzchar(ppdk)) return(NULL)
  resp <- request(paste0(AWRE_API, "/team/", AWRE_TEAM, "/clip/", ppdk, "/angles")) |>
    req_headers(Authorization = paste("Api-Key", key),
                Accept = "application/json") |>
    req_retry(max_tries = 2) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  if (resp_status(resp) >= 400) return(NULL)
  body <- tryCatch(resp_body_json(resp, simplifyVector = FALSE),
                   error = function(e) NULL)
  clips <- body$clips %||% list()
  clips <- keep(clips, ~ !identical(.x$perspective, "All"))   # composite stream: skip
  if (!length(clips)) return(NULL)
  stats::setNames(map_chr(clips, ~ .x$video_url %||% NA_character_),
                  map_chr(clips, ~ .x$perspective %||% "Angle"))
}

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "Bullpen Central",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#006F71"),
  header = tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/hls.js@1"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('play_hls', function(msg) {
        var tries = 0;
        function go() {
          var v = document.getElementById(msg.id);
          if (!v) { if (tries++ < 20) setTimeout(go, 100); return; }
          if (v._hls) { v._hls.destroy(); v._hls = null; }
          if (v.canPlayType('application/vnd.apple.mpegurl')) {
            v.src = msg.url; v.play();
          } else if (window.Hls && Hls.isSupported()) {
            var h = new Hls();
            h.loadSource(msg.url);
            h.attachMedia(v);
            v._hls = h;
            h.on(Hls.Events.MANIFEST_PARSED, function() { v.play(); });
          }
        }
        go();
      });
    "))
  ),
  sidebar = sidebar(
    width = 320,
    selectInput("pitcher", "Pitcher", choices = NULL),
    selectInput("bp_session", "Bullpen session", choices = NULL),
    hr(),
    p(class = "text-muted small",
      icon("database"),
      " Data syncs from TrackMan + AWRE automatically each night. ",
      "New bullpens appear here within a minute of ingest.")
  ),

  nav_panel(
    "Bullpen Report",
    card(
      card_header(h5(textOutput("bp_title", inline = TRUE), class = "m-0")),
      gt::gt_output("bp_summary_table")
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(full_screen = TRUE, card_header("Movement"),
           plotly::plotlyOutput("bp_movement_plot", height = "430px")),
      card(full_screen = TRUE, card_header("Release"),
           plotly::plotlyOutput("bp_release_plot", height = "430px")),
      card(full_screen = TRUE, card_header("Location"),
           plotly::plotlyOutput("bp_location_plot", height = "430px"))
    ),
    p(class = "text-muted small ps-2",
      icon("circle-play"), " Click any pitch in a chart to watch its video.")
  ),

  nav_panel(
    "Game Log",
    card(
      card_header("Pitch-by-pitch log"),
      DT::DTOutput("bp_log")
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  db_version <- reactivePoll(
    60 * 1000, session,
    checkFunc = pitches_version,
    valueFunc = function() Sys.time()
  )

  all_data <- reactive({
    db_version()
    d <- tryCatch(load_pitches(), error = function(e) {
      showNotification(paste("Database error:", conditionMessage(e)),
                       type = "error", duration = 10)
      NULL
    })
    if (is.null(d) || !nrow(d)) return(NULL)
    d
  })

  # -- Selectors --------------------------------------------------------------
  observeEvent(all_data(), {
    d <- all_data(); req(d)
    pitchers <- sort(unique(d$Pitcher[!is.na(d$Pitcher)]))
    sel <- isolate(input$pitcher)
    updateSelectInput(session, "pitcher", choices = pitchers,
                      selected = if (!is.null(sel) && sel %in% pitchers) sel
                                 else pitchers[1])
  })

  observeEvent(list(input$pitcher, all_data()), {
    d <- all_data(); req(d, input$pitcher, nzchar(input$pitcher))
    ses <- d |>
      dplyr::filter(Pitcher == input$pitcher) |>
      dplyr::group_by(SessionId) |>
      dplyr::summarise(date = max(date), n = dplyr::n(),
                       vids = sum(has_edger | has_awre, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(date))
    req(nrow(ses) > 0)
    labels <- sprintf("%s — %d pitches%s",
                      format(ses$date, "%b %d, %Y"), ses$n,
                      ifelse(ses$vids > 0,
                             sprintf("  (%d videos)", ses$vids), ""))
    sel <- isolate(input$bp_session)
    updateSelectInput(session, "bp_session",
                      choices = stats::setNames(ses$SessionId, labels),
                      selected = if (!is.null(sel) && sel %in% ses$SessionId) sel
                                 else ses$SessionId[1])
  })

  bp_session_df <- reactive({
    d <- all_data()
    req(d, input$pitcher, input$bp_session)
    d |>
      dplyr::filter(Pitcher == input$pitcher, SessionId == input$bp_session) |>
      dplyr::arrange(PitchNo)
  })

  output$bp_title <- renderText({
    df <- bp_session_df()
    validate(need(nrow(df) > 0, "Bullpen Report"))
    hand <- df$PitcherThrows[1]
    sprintf("%s%s — %s",
            df$Pitcher[1],
            if (!is.na(hand) && nzchar(hand))
              paste0(" (", substr(hand, 1, 1), "HP)") else "",
            format(df$date[1], "%B %d, %Y"))
  })

  # -- Summary table ----------------------------------------------------------
  output$bp_summary_table <- gt::render_gt({
    df <- bp_session_df()
    validate(need(!is.null(df) && nrow(df) > 0, "Select a bullpen session."))
    st <- calculate_bullpen_summary(df)
    validate(need(nrow(st) > 0, "No tagged pitches in this session."))
    pcol_fn <- function(x) vapply(as.character(x), function(p)
      if (!is.na(p) && p %in% names(pitch_colors)) pitch_colors[[p]] else "#eeeeee",
      character(1))
    st %>%
      gt::gt() %>%
      gt_theme_guardian() %>%
      gt::data_color(columns = Pitch, fn = pcol_fn) %>%
      gt::sub_missing(missing_text = "—") %>%
      gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(6)) %>%
      gt::cols_align(align = "center", columns = gt::everything())
  })

  # -- Movement ---------------------------------------------------------------
  output$bp_movement_plot <- plotly::renderPlotly({
    df <- bp_session_df() %>%
      dplyr::filter(!is.na(TaggedPitchType), TaggedPitchType != "Other",
                    TaggedPitchType != "Undefined",
                    !is.na(HorzBreak), !is.na(InducedVertBreak))
    validate(need(nrow(df) > 0, "No movement data for this session."))
    df <- df %>% dplyr::mutate(hover = paste0(
      "<b>", TaggedPitchType, "</b>",
      "<br>Velo: ", sprintf("%.1f", RelSpeed),
      "<br>IVB/HB: ", sprintf("%.1f", InducedVertBreak), " / ", sprintf("%.1f", HorzBreak),
      "<br>Spin: ", sprintf("%.0f", SpinRate),
      "<br>Eff: ", fmt_pct(SpinAxis3dSpinEfficiency)))
    centers <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(mean_velo = round(mean(RelSpeed, na.rm = TRUE)),
                       mean_hb = median(HorzBreak, na.rm = TRUE),
                       mean_ivb = median(InducedVertBreak, na.rm = TRUE), .groups = "drop")
    p <- ggplot(df, aes(HorzBreak, InducedVertBreak)) +
      geom_vline(xintercept = 0, color = "black", linewidth = 0.7) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
      suppressWarnings(geom_point(
        aes(fill = TaggedPitchType, text = hover, customdata = PlayID),
        alpha = 1, shape = 21,
        color = "black", stroke = 0.4, size = 4)) +
      geom_point(data = centers, aes(mean_hb, mean_ivb, fill = TaggedPitchType),
                 shape = 21, color = "black", stroke = 0.5, size = 9,
                 alpha = 0.95, inherit.aes = FALSE) +
      geom_text(data = centers, aes(mean_hb, mean_ivb, label = mean_velo),
                color = "black", size = 3.1, inherit.aes = FALSE) +
      scale_fill_manual(values = pitch_colors, name = NULL) +
      scale_x_continuous(breaks = seq(-20, 20, by = 10)) +
      scale_y_continuous(breaks = seq(-20, 20, by = 10)) +
      coord_fixed(ratio = 1, xlim = c(-27.5, 27.5), ylim = c(-27.5, 27.5)) +
      labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey88", linewidth = 0.3),
            plot.background = element_rect(fill = "white", color = NA),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
            axis.text = element_text(size = 11, color = "black"),
            axis.title = element_text(size = 12))
    plotly::ggplotly(p, tooltip = "text", source = "bp_move") %>%
      plotly::event_register("plotly_click")
  })

  # -- Release ----------------------------------------------------------------
  output$bp_release_plot <- plotly::renderPlotly({
    df <- bp_session_df() %>%
      dplyr::filter(!is.na(RelSide), !is.na(RelHeight),
                    !is.na(TaggedPitchType), TaggedPitchType != "Other",
                    TaggedPitchType != "Undefined")
    validate(need(nrow(df) > 0, "No release data for this session."))
    df <- df %>% dplyr::mutate(hover = paste0(
      "<b>", TaggedPitchType, "</b>",
      "<br>Velo: ", sprintf("%.1f", RelSpeed),
      "<br>Rel: ", sprintf("%.2f", RelSide), " / ", sprintf("%.2f", RelHeight),
      "<br>Ext: ", sprintf("%.2f", Extension),
      "<br>Eff: ", fmt_pct(SpinAxis3dSpinEfficiency)))
    avg_release <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(RelSide = mean(RelSide, na.rm = TRUE),
                       RelHeight = mean(RelHeight, na.rm = TRUE), .groups = "drop")
    p <- ggplot() +
      geom_rect(aes(xmin = -5, xmax = 5, ymin = 0, ymax = 0.83),
                fill = "#632b11", inherit.aes = FALSE) +
      geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = 0.8, ymax = 0.95),
                fill = "white", color = "black", linewidth = 0.4, inherit.aes = FALSE) +
      suppressWarnings(geom_point(
        data = df,
        aes(RelSide, RelHeight, fill = TaggedPitchType,
            text = hover, customdata = PlayID),
        size = 4.2, shape = 21, color = "black", alpha = 1, stroke = 0.4)) +
      geom_point(data = avg_release, aes(RelSide, RelHeight, fill = TaggedPitchType),
                 size = 7, shape = 21, color = "black", stroke = 0.6,
                 alpha = 0.95) +
      annotate("text", x = -3.5, y = 7.5, label = "← 1B", size = 3.8, hjust = 0) +
      annotate("text", x = 3.5, y = 7.5, label = "3B →", size = 3.8, hjust = 1) +
      scale_fill_manual(values = pitch_colors, name = NULL) +
      coord_fixed(ratio = 1, xlim = c(-4, 4), ylim = c(0, 8)) +
      labs(x = "Release Side (ft)", y = "Release Height (ft)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey88", linewidth = 0.3),
            plot.background = element_rect(fill = "white", color = NA),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
            axis.text = element_text(size = 11, color = "black"),
            axis.title = element_text(size = 12))
    plotly::ggplotly(p, tooltip = "text", source = "bp_rel") %>%
      plotly::event_register("plotly_click")
  })

  # -- Location ---------------------------------------------------------------
  output$bp_location_plot <- plotly::renderPlotly({
    df <- bp_session_df() %>%
      dplyr::filter(!is.na(TaggedPitchType), TaggedPitchType != "",
                    TaggedPitchType != "Undefined", TaggedPitchType != "Other",
                    !is.na(PlateLocSide), !is.na(PlateLocHeight))
    validate(need(nrow(df) > 0, "No location data for this session."))
    df <- df %>% dplyr::mutate(hover = paste0(
      "<b>", TaggedPitchType, "</b>",
      "<br>Velo: ", sprintf("%.1f", RelSpeed),
      "<br>IVB/HB: ", sprintf("%.1f", InducedVertBreak), " / ", sprintf("%.1f", HorzBreak),
      "<br>Eff: ", fmt_pct(SpinAxis3dSpinEfficiency)))
    zl <- -0.8333; zr <- 0.8333; zb <- 1.5; zt <- 3.5
    p <- ggplot(df, aes(PlateLocSide, PlateLocHeight)) +
      annotate("segment", x = -0.85, xend = 0.85, y = 0.15, yend = 0.15,
               color = "black", linewidth = 0.6) +
      annotate("segment", x = -0.85, xend = -0.85, y = 0.15, yend = 0.3,
               color = "black", linewidth = 0.6) +
      annotate("segment", x = 0.85, xend = 0.85, y = 0.15, yend = 0.3,
               color = "black", linewidth = 0.6) +
      annotate("segment", x = -0.85, xend = 0, y = 0.3, yend = 0.45,
               color = "black", linewidth = 0.6) +
      annotate("segment", x = 0.85, xend = 0, y = 0.3, yend = 0.45,
               color = "black", linewidth = 0.6) +
      annotate("rect", xmin = -1.1, xmax = 1.1, ymin = 1.2, ymax = 3.8,
               fill = NA, color = "gray55", linetype = "dashed", linewidth = 0.5) +
      annotate("rect", xmin = zl, xmax = zr, ymin = zb, ymax = zt,
               fill = NA, color = "black", linewidth = 0.9) +
      suppressWarnings(geom_point(
        aes(fill = TaggedPitchType, text = hover, customdata = PlayID),
        size = 4.4, shape = 21,
        color = "black", stroke = 0.4, alpha = 1)) +
      scale_fill_manual(values = pitch_colors, name = NULL) +
      coord_fixed(xlim = c(-2.2, 2.2), ylim = c(0, 4.2)) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", color = NA))
    plotly::ggplotly(p, tooltip = "text", source = "bp_loc") %>%
      plotly::event_register("plotly_click")
  })

  # -- Game log ---------------------------------------------------------------
  output$bp_log <- DT::renderDT({
    df <- bp_session_df()
    validate(need(nrow(df) > 0, "Select a bullpen session."))
    log_df <- df |>
      dplyr::transmute(
        `#`    = PitchNo,
        Time   = substr(Time, 12, 19),
        Pitch  = TaggedPitchType,
        Velo   = round(RelSpeed, 1),
        Spin   = round(SpinRate),
        `Eff %` = round(100 * SpinAxis3dSpinEfficiency),
        Tilt,
        IVB    = round(InducedVertBreak, 1),
        HB     = round(HorzBreak, 1),
        `Rel Ht` = round(RelHeight, 2),
        `Rel Side` = round(RelSide, 2),
        Ext    = round(Extension, 2),
        VAA    = round(VertApprAngle, 1),
        Zone   = ifelse(in_zone(PlateLocSide, PlateLocHeight), "In", "Out"),
        Video  = dplyr::case_when(
          (has_edger | has_awre) & !is.na(PlayID) ~ sprintf(paste0(
            '<button class="btn btn-sm btn-primary" ',
            'onclick="Shiny.setInputValue(\'watch_play\', \'%s\', ',
            '{priority: \'event\'})">&#9658; Watch</button>'), PlayID),
          TRUE ~ '<span class="text-muted">—</span>')
      )
    DT::datatable(
      log_df, escape = FALSE, rownames = FALSE,
      selection = "none",
      options = list(pageLength = 50, dom = "ft", scrollX = TRUE,
                     order = list(list(0, "asc"))),
      class = "compact stripe hover"
    )
  })

  # -- Video modal: Edgertronic + AWRE ----------------------------------------
  awre_current <- reactiveVal(NULL)   # named vector: perspective -> HLS url

  show_video <- function(play_id) {
    d <- all_data(); req(d)
    row <- d |> dplyr::filter(PlayID == play_id) |> dplyr::slice(1)
    if (!nrow(row)) return(invisible(NULL))

    has_e <- isTRUE(row$has_edger) && !is.na(row$edger_blob)
    has_a <- isTRUE(row$has_awre)  && !is.na(row$awre_ppdk)
    if (!has_e && !has_a) {
      showNotification("No video for this pitch.", type = "warning")
      return(invisible(NULL))
    }

    # Edgertronic: mint SAS urls now
    edger_tags <- NULL
    if (has_e) {
      blobs <- strsplit(
        if (!is.na(row$edger_all_blobs) && nzchar(row$edger_all_blobs))
          row$edger_all_blobs else row$edger_blob,
        "|", fixed = TRUE)[[1]]
      urls <- tryCatch(edger_urls(row$SessionId, blobs), error = function(e) NULL)
      if (!is.null(urls) && length(urls) && !all(is.na(urls))) {
        edger_tags <- tagList(
          h6(class = "mt-1", icon("bolt"), " Edgertronic",
             if (!is.na(row$framerate))
               span(class = "text-muted small",
                    sprintf("  %s fps", format(row$framerate, big.mark = ",")))),
          lapply(urls[!is.na(urls)], function(u)
            tags$video(src = u, controls = NA, autoplay = has_e && !has_a,
                       muted = NA, playsinline = NA,
                       style = "width:100%; margin-bottom:8px; border-radius:6px;"))
        )
      }
    }

    # AWRE: fetch angle list now, play via hls.js — loud about every failure
    awre_tags <- NULL
    awre_current(NULL)
    if (has_a) {
      if (!nzchar(Sys.getenv("AWRE_KEY"))) {
        showNotification(
          "AWRE_KEY is not set in this R session — add AWRE_KEY=... to ~/.Renviron, restart R, relaunch the app.",
          type = "warning", duration = 10)
      } else {
        angles <- tryCatch(awre_angles(row$awre_ppdk), error = function(e) {
          showNotification(paste("AWRE request error:", conditionMessage(e)),
                           type = "error", duration = 10); NULL })
        if (is.null(angles) || !length(angles)) {
          showNotification(
            paste0("AWRE returned no clips for ", row$awre_ppdk,
                   " (key rejected, or clips missing)."),
            type = "warning", duration = 10)
        } else {
          awre_current(angles)
          default <- if ("Bullpen" %in% names(angles)) "Bullpen" else names(angles)[1]
          awre_tags <- tagList(
            h6(class = "mt-2", icon("video"), " AWRE"),
            radioButtons("awre_angle", NULL, choices = names(angles),
                         selected = default, inline = TRUE),
            tags$video(id = "awreVid", controls = NA, muted = NA, playsinline = NA,
                       style = "width:100%; border-radius:6px; background:#000;")
          )
        }
      }
    }

    meta_bits <- c(
      if (!is.na(row$SpinRate))
        sprintf("%.0f rpm · %s eff", row$SpinRate,
                fmt_pct(row$SpinAxis3dSpinEfficiency)))
    meta <- if (length(meta_bits)) paste(meta_bits, collapse = "  ·  ") else NULL

    showModal(modalDialog(
      title = sprintf("%s — %s, %s mph (Pitch %s)",
                      row$Pitcher, row$TaggedPitchType,
                      ifelse(is.na(row$RelSpeed), "—",
                             sprintf("%.1f", row$RelSpeed)),
                      ifelse(is.na(row$PitchNo), "—", row$PitchNo)),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      if (!is.null(meta)) p(class = "text-muted small mb-2", meta),
      edger_tags,
      awre_tags
    ))

    # kick off the default AWRE angle once the modal DOM exists
    a <- awre_current()
    if (!is.null(a)) {
      default <- if ("Bullpen" %in% names(a)) "Bullpen" else names(a)[1]
      session$sendCustomMessage("play_hls", list(id = "awreVid", url = a[[default]]))
    }
  }

  observeEvent(input$awre_angle, {
    a <- awre_current()
    if (is.null(a) || !(input$awre_angle %in% names(a))) return()
    session$sendCustomMessage("play_hls",
                              list(id = "awreVid", url = a[[input$awre_angle]]))
  }, ignoreInit = TRUE)

  safe_show_video <- function(pid) {
    tryCatch(show_video(pid), error = function(e)
      showNotification(paste("Video error:", conditionMessage(e)),
                       type = "error", duration = 10))
  }

  lapply(c("bp_move", "bp_rel", "bp_loc"), function(src) {
    observeEvent(plotly::event_data("plotly_click", source = src), {
      ed <- plotly::event_data("plotly_click", source = src)
      pid <- ed$customdata
      if (!is.null(pid) && length(pid) && !is.na(pid[1])) safe_show_video(pid[1])
    })
  })

  observeEvent(input$watch_play, safe_show_video(input$watch_play))
}

shinyApp(ui, server)

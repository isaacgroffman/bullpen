# =============================================================================
# trackman_api.R — TrackMan Data API client + practice-session ingest.
# Ported from trackman_practice_parquet.R; everything here is function-only so
# sourcing it never hits the network. The Shiny app calls ingest_range() to
# pull new sessions and edger_urls() to mint fresh SAS links at click time.
#
# API shape reminders:
#   * discovery is POST, 30-consecutive-date cap  -> chunked
#   * Edgertronic exists ONLY under /media/practice/videotokens
#   * blob layout: Plays/{PlayId}/{VideoType}/{UnitSystem}/<file>
#   * each /balls record carries a top-level playId matching /plays playID —
#     ball metrics join on it directly. Do NOT align by pitch order.
# =============================================================================

# ---- Config -----------------------------------------------------------------
# Credentials come from the environment ONLY — never hardcode them here: this
# code lives in a public GitHub repo for Connect Cloud deployment.
#   local:         ~/.Renviron
#   Connect Cloud: content settings -> Variables (TM_CLIENT_SECRET as secret)
TM_CLIENT_ID <- Sys.getenv("TM_CLIENT_ID", "CoastalCarolina-Palace")
TM_SECRET    <- Sys.getenv("TM_CLIENT_SECRET")

TM_TOKEN_URL <- "https://login.trackmanbaseball.com/connect/token"
TM_API       <- "https://dataapi.trackmanbaseball.com/api/v1"

# Where the arrow dataset lives: BP_PARQUET_ROOT override, else the local
# store built by the ingest script, else the snapshot bundled with the app
# (the path a Connect Cloud deployment sees).
PARQUET_ROOT <- local({
  env <- Sys.getenv("BP_PARQUET_ROOT")
  if (nzchar(env)) return(path.expand(env))
  home <- path.expand("~/data/trackman_practice")
  if (dir.exists(home)) home else "data/trackman_practice"
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
nz <- function(x, type = NA_real_) if (is.null(x) || length(x) == 0) type else x[[1]]

# ---- Auth -------------------------------------------------------------------
tm_token <- local({
  cache <- NULL
  function(force = FALSE) {
    if (!nzchar(TM_SECRET)) {
      stop("TM_CLIENT_SECRET is not set. Add it to ~/.Renviron locally, or ",
           "as a secret environment variable in Connect Cloud content settings.",
           call. = FALSE)
    }
    if (!force && !is.null(cache) && cache$expires_at > Sys.time() + 120) {
      return(cache$access_token)
    }
    resp <- httr2::request(TM_TOKEN_URL) |>
      httr2::req_body_form(client_id = TM_CLIENT_ID,
                           client_secret = TM_SECRET,
                           grant_type = "client_credentials") |>
      httr2::req_perform() |> httr2::resp_body_json()
    cache <<- list(access_token = resp$access_token,
                   expires_at = Sys.time() + as.numeric(resp$expires_in %||% 3600))
    cache$access_token
  }
})

tm_get <- function(path) {
  httr2::request(paste0(TM_API, path)) |>
    httr2::req_auth_bearer_token(tm_token()) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |> httr2::resp_body_json(simplifyVector = FALSE)
}

tm_post <- function(path, body) {
  httr2::request(paste0(TM_API, path)) |>
    httr2::req_auth_bearer_token(tm_token()) |>
    httr2::req_headers(`Content-Type` = "application/json-patch+json",
                       accept = "text/plain") |>
    httr2::req_body_json(body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |> httr2::resp_body_json(simplifyVector = FALSE)
}

# ---- Session discovery (chunked to <=30 dates) ------------------------------
date_chunks <- function(from, to, days = 28) {
  starts <- seq(from, to, by = paste(days, "days"))
  purrr::map2(starts, pmin(starts + days - 1, to), ~ list(from = .x, to = .y))
}

discover_practice <- function(from, to, session_type = "All") {
  purrr::map_dfr(date_chunks(from, to), function(ch) {
    message(sprintf("discover %s -> %s", ch$from, ch$to))
    body <- list(
      sessionType = session_type,
      utcDateFrom = format(as.POSIXct(ch$from, tz = "UTC"), "%Y-%m-%dT00:00:00.000Z"),
      utcDateTo   = format(as.POSIXct(ch$to,   tz = "UTC"), "%Y-%m-%dT23:59:59.999Z")
    )
    res <- tryCatch(tm_post("/discovery/practice/sessions", body),
                    error = function(e) { message("  ", conditionMessage(e)); list() })
    purrr::map_dfr(res, ~ tibble::tibble(
      sessionId         = .x$sessionId %||% NA_character_,
      externalSessionId = as.character(.x$externalSessionId %||% NA),
      sessionType       = .x$sessionType %||% NA_character_,
      gameDateUtc       = .x$gameDateUtc %||% NA_character_,
      gameDateLocal     = .x$gameDateLocal %||% NA_character_
    ))
  })
}

# ---- Flatteners -------------------------------------------------------------
flat_pitch <- function(b) {
  p <- b$pitch; r <- p$release; tr <- p$trajectory; lo <- p$location; px <- p$pfxData
  tibble::tibble(
    PlayID           = b$playId %||% NA_character_,
    PitchUID         = p$pitchUID %||% NA_character_,
    RelSpeed         = nz(r$relSpeed),
    VertRelAngle     = nz(r$vertRelAngle),
    HorzRelAngle     = nz(r$horzRelAngle),
    SpinRate         = nz(r$spinRate),
    SpinAxis         = nz(r$spinAxis),
    Tilt             = nz(r$tilt, NA_character_),
    RelHeight        = nz(r$relHeight),
    RelSide          = nz(r$relSide),
    Extension        = nz(r$extension),
    SpinAxis3dTilt              = nz(r$spinAxis3dTilt, NA_character_),
    SpinAxis3dTransverseAngle   = nz(r$spinAxis3dTransverseAngle),
    SpinAxis3dLongitudinalAngle = nz(r$spinAxis3dLongitudinalAngle),
    SpinAxis3dActiveSpinRate    = nz(r$spinAxis3dActiveSpinRate),
    SpinAxis3dSpinEfficiency    = nz(r$spinAxis3dSpinEfficiency),
    VertBreak        = nz(tr$vertBreak),
    InducedVertBreak = nz(tr$inducedVertBreak),
    HorzBreak        = nz(tr$horzBreak),
    PlateLocHeight   = nz(lo$plateLocHeight),
    PlateLocSide     = nz(lo$plateLocSide),
    ZoneSpeed        = nz(lo$zoneSpeed),
    VertApprAngle    = nz(lo$vertApprAngle),
    HorzApprAngle    = nz(lo$horzApprAngle),
    ZoneTime         = nz(lo$zoneTime),
    pfxx = nz(px$pfxx), pfxz = nz(px$pfxz),
    x0 = nz(px$x0$x), y0 = nz(px$x0$y), z0 = nz(px$x0$z),
    vx0 = nz(px$v0$x), vy0 = nz(px$v0$y), vz0 = nz(px$v0$z),
    ax0 = nz(px$a0$x), ay0 = nz(px$a0$y), az0 = nz(px$a0$z),
    EffVelocity      = nz(p$effVelocity)
  )
}

flat_hit <- function(b) {
  h <- b$hit; l <- h$launch; ld <- h$landing
  tibble::tibble(
    PlayID       = b$playId %||% NA_character_,
    HitUID       = h$hitUID %||% NA_character_,
    ExitSpeed    = nz(l$exitSpeed),
    Angle        = nz(l$angle),
    Direction    = nz(l$direction),
    HitSpinRate  = nz(l$spinRate),
    ContactPositionX = nz(l$contactPosition$x),
    ContactPositionY = nz(l$contactPosition$y),
    ContactPositionZ = nz(l$contactPosition$z),
    LastTrackedDistance = nz(h$lastTrackedDistance),
    HangTime     = nz(ld$hangTime),
    Distance     = nz(ld$distance),
    Bearing      = nz(ld$bearing)
  )
}

flat_play <- function(p) {
  tibble::tibble(
    PlayID          = p$playID %||% NA_character_,
    CalibrationId   = p$calibrationId %||% NA_character_,
    Date            = p$date %||% NA_character_,
    Time            = p$time %||% NA_character_,
    Pitcher         = p$pitcher$pitcher %||% NA_character_,
    PitcherId       = as.character(p$pitcher$pitcherId %||% NA),
    PitcherThrows   = p$pitcher$pitcherThrows %||% NA_character_,
    Batter          = p$batter$batter %||% NA_character_,
    BatterId        = as.character(p$batter$batterId %||% NA),
    BatterSide      = p$batter$batterSide %||% NA_character_,
    TaggedPitchType = p$pitchTag$taggedPitchType %||% NA_character_,
    PitchNo         = as.integer(p$taggerBehavior$pitchNo %||% NA),
    PitchSession    = p$taggerBehavior$pitchSession %||% NA_character_
  )
}

as_list_of <- function(x, key) {   # API returns bare object when there's 1 record
  if (!length(x)) return(list())
  if (!is.null(x[[key]])) list(x) else x
}

# ---- Edgertronic blob index -------------------------------------------------
list_blobs <- function(base, sas) {
  out <- character(0); marker <- NULL
  repeat {
    url <- paste0(base, "/", sas, "&comp=list&restype=container")
    if (!is.null(marker)) url <- paste0(url, "&marker=", utils::URLencode(marker, TRUE))
    xml <- httr2::request(url) |> httr2::req_retry(max_tries = 3) |>
      httr2::req_perform() |> httr2::resp_body_xml()
    out <- c(out, xml2::xml_text(xml2::xml_find_all(xml, ".//Blobs/Blob/Name")))
    marker <- xml2::xml_text(xml2::xml_find_first(xml, "./NextMarker"))
    if (is.na(marker) || !nzchar(marker)) break
  }
  out
}

edger_index <- function(sid) {
  toks  <- tryCatch(tm_get(paste0("/media/practice/videotokens/", sid)),
                    error = function(e) list())
  edger <- purrr::keep(toks, ~ identical(.x$type, "EdgertronicVideos"))
  if (!length(edger)) return(tibble::tibble())
  e <- edger[[1]]
  base <- sprintf("https://%s.blob.core.windows.net/%s", e$entityPath, e$endpoint)

  blobs <- tryCatch(list_blobs(base, e$token), error = function(err) {
    message("  blob list failed: ", conditionMessage(err)); character(0)
  })
  if (!length(blobs)) return(tibble::tibble())

  tibble::tibble(blob = blobs) |>
    dplyr::mutate(parts = strsplit(blob, "/", fixed = TRUE),
      PlayID      = purrr::map_chr(parts, ~ if (length(.x) >= 2) .x[2] else NA_character_),
      video_type  = purrr::map_chr(parts, ~ if (length(.x) >= 3) .x[3] else NA_character_),
      unit_system = purrr::map_chr(parts, ~ if (length(.x) >= 5) .x[4] else NA_character_)) |>
    dplyr::select(-parts) |>
    dplyr::filter(is.na(video_type) | grepl("edger", video_type, ignore.case = TRUE)) |>
    dplyr::group_by(PlayID) |>
    dplyr::summarise(
      edger_blob      = dplyr::first(blob),
      edger_n_clips   = dplyr::n(),
      edger_all_blobs = paste(blob, collapse = "|"),
      .groups = "drop"
    ) |>
    dplyr::mutate(edger_account = e$entityPath, edger_container = e$endpoint)
}

edger_meta <- function(sid) {
  res <- tryCatch(tm_get(paste0("/media/practice/videometadata/", sid)),
                  error = function(e) list())
  res <- as_list_of(res, "videoClipId")
  if (!length(res)) return(tibble::tibble())
  purrr::map_dfr(res, ~ tibble::tibble(
    PlayID            = .x$playId %||% NA_character_,
    camera_type       = .x$cameraType %||% NA_character_,
    camera_target     = .x$cameraTarget %||% NA_character_,
    framerate         = nz(.x$framerate),
    shutter_speed     = nz(.x$shutterSpeed),
    release_frame     = nz(.x$releasePointFrame),
    release_frame_adj = nz(.x$adjustedReleasePointFrame),
    clip_seconds      = nz(.x$videoDurationInSeconds)
  )) |>
    dplyr::filter(grepl("edger", camera_type %||% "", ignore.case = TRUE) |
                    is.na(camera_type)) |>
    dplyr::distinct(PlayID, .keep_all = TRUE)
}

# ---- Build one session ------------------------------------------------------
build_session <- function(sid, sdate, ext_id) {
  message(sprintf("session %s (%s)", substr(sid, 1, 8), sdate))

  balls <- as_list_of(tryCatch(tm_get(paste0("/data/practice/balls/", sid)),
                               error = function(e) list()), "trackType")
  plays <- as_list_of(tryCatch(tm_get(paste0("/data/practice/plays/", sid)),
                               error = function(e) list()), "playID")
  if (!length(plays)) { message("  no plays"); return(tibble::tibble()) }

  play_df <- purrr::map_dfr(plays, flat_play)

  # JOIN NOTE: /balls records join deterministically on PlayID; /balls is NOT
  # in play order — never align by sequence.
  pitch_df <- balls |>
    purrr::keep(~ identical(.x$trackType, "Pitch")) |>
    purrr::map_dfr(flat_pitch)
  hit_df <- balls |>
    purrr::keep(~ identical(.x$trackType, "Hit")) |>
    purrr::map_dfr(flat_hit)
  if (nrow(pitch_df)) pitch_df <- dplyr::distinct(pitch_df, PlayID, .keep_all = TRUE)
  if (nrow(hit_df))   hit_df   <- dplyr::distinct(hit_df,   PlayID, .keep_all = TRUE)

  out <- play_df
  if (nrow(pitch_df)) out <- dplyr::left_join(out, pitch_df, by = "PlayID")
  if (nrow(hit_df))   out <- dplyr::left_join(out, hit_df,   by = "PlayID")
  out <- dplyr::mutate(out, join_method = "playId")

  idx <- edger_index(sid)
  emd <- edger_meta(sid)
  if (nrow(idx)) out <- dplyr::left_join(out, idx, by = "PlayID")
  if (nrow(emd)) out <- dplyr::left_join(out, emd, by = "PlayID")

  for (col in c("edger_blob", "edger_all_blobs", "edger_account", "edger_container",
                "camera_type", "camera_target")) {
    if (!col %in% names(out)) out[[col]] <- NA_character_
  }
  for (col in c("edger_n_clips", "framerate", "shutter_speed",
                "release_frame", "release_frame_adj", "clip_seconds")) {
    if (!col %in% names(out)) out[[col]] <- NA_real_
  }

  out |>
    dplyr::mutate(SessionId = sid,
                  ExternalSessionId = ext_id,
                  date = sdate,
                  has_edger = !is.na(edger_blob),
                  ingested_at = Sys.time(),
                  .before = 1)
}

# ---- Ingest a date range into the parquet store -----------------------------
# progress: optional function(frac, msg) for Shiny withProgress reporting.
# Returns the number of newly-written pitch rows.
ingest_range <- function(from, to, session_type = "Pitching",
                         parquet_root = PARQUET_ROOT, resume = TRUE,
                         progress = NULL) {
  dir.create(parquet_root, recursive = TRUE, showWarnings = FALSE)
  note <- function(frac, msg) if (is.function(progress)) progress(frac, msg)

  note(0.02, "Discovering sessions…")
  sessions <- discover_practice(as.Date(from), as.Date(to), session_type) |>
    dplyr::distinct(sessionId, .keep_all = TRUE) |>
    # gameDateLocal is MM/DD/YYYY, NOT ISO — naive as.Date() NA's out any
    # day-of-month > 12. Fall back to the ISO UTC stamp.
    dplyr::mutate(date = dplyr::coalesce(
      as.Date(gameDateLocal, format = "%m/%d/%Y"),
      as.Date(substr(gameDateUtc, 1, 10)))) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::arrange(date)

  if (resume &&
      length(list.files(parquet_root, recursive = TRUE, pattern = "\\.parquet$"))) {
    done <- arrow::open_dataset(parquet_root) |>
      dplyr::distinct(SessionId) |> dplyr::collect() |> dplyr::pull(SessionId)
    sessions <- dplyr::filter(sessions, !sessionId %in% done)
  }
  if (!nrow(sessions)) { note(1, "Nothing new."); return(0L) }

  n <- nrow(sessions)
  ds <- purrr::pmap_dfr(
    list(sessions$sessionId, sessions$date, sessions$externalSessionId,
         seq_len(n)),
    function(sid, d, x, i) {
      note(0.05 + 0.9 * (i - 1) / n, sprintf("Session %d of %d (%s)…", i, n, d))
      tryCatch(build_session(sid, d, x),
               error = function(e) {
                 message("  FAILED: ", conditionMessage(e)); tibble::tibble()
               })
    })

  if (nrow(ds)) {
    note(0.97, "Writing parquet…")
    arrow::write_dataset(ds, parquet_root,
                         format = "parquet",
                         partitioning = "date",
                         basename_template = paste0(
                           "ingest-", format(Sys.time(), "%Y%m%d%H%M%S"),
                           "-{i}.parquet"),
                         existing_data_behavior = "overwrite")
  }
  nrow(ds)
}

# ---- Read side: mint live URLs from stored blob paths -----------------------
# SAS tokens expire within hours, so URLs are minted fresh on every request.
# One token fetch covers all blobs for a session.
edger_urls <- function(session_id, blobs) {
  toks  <- tm_get(paste0("/media/practice/videotokens/", session_id))
  edger <- purrr::keep(toks, ~ identical(.x$type, "EdgertronicVideos"))
  if (!length(edger)) return(rep(NA_character_, length(blobs)))
  e <- edger[[1]]
  vapply(blobs, function(blob) {
    enc <- paste(vapply(strsplit(blob, "/", fixed = TRUE)[[1]],
                        utils::URLencode, character(1), reserved = TRUE),
                 collapse = "/")
    sprintf("https://%s.blob.core.windows.net/%s/%s%s",
            e$entityPath, e$endpoint, enc, e$token)
  }, character(1), USE.NAMES = FALSE)
}

edger_url <- function(session_id, blob) edger_urls(session_id, blob)[1]

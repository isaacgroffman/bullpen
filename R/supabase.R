# =============================================================================
# R/supabase.R — Bullpen Central's read-only connection to Supabase.
#
# Uses the `bullpen_reader` Postgres role (SELECT-only) through Supabase's
# transaction pooler. All credentials arrive as environment variables:
# on Posit Connect Cloud set them in the content's Variables/Secrets panel;
# locally put the same names in ~/.Renviron.
#
#   SB_DB_HOST  e.g. aws-1-us-east-1.pooler.supabase.com   (from Connect dialog)
#   SB_DB_USER  e.g. bullpen_reader.ryqzkosdbksawrcrdqrc   (role.projectref)
#   SB_DB_PASS  the bullpen_reader password you invented in schema.sql
# =============================================================================

library(pool)
library(RPostgres)
library(DBI)

sb_pool <- local({
  p <- NULL
  function() {
    if (!is.null(p) && pool::dbIsValid(p)) return(p)
    host <- Sys.getenv("SB_DB_HOST")
    user <- Sys.getenv("SB_DB_USER")
    pass <- Sys.getenv("SB_DB_PASS")
    if (!nzchar(host) || !nzchar(user) || !nzchar(pass)) {
      stop("Supabase credentials missing: set SB_DB_HOST, SB_DB_USER, SB_DB_PASS")
    }
    p <<- pool::dbPool(
      RPostgres::Postgres(),
      host     = host,
      port     = 6543,          # Supabase transaction pooler (NOT 5432)
      dbname   = "postgres",
      user     = user,
      password = pass,
      sslmode  = "require",
      minSize  = 1, maxSize = 3
    )
    p
  }
})

# One query, aliased straight to the camelCase names the app already uses,
# so every plot / table / video handler downstream stays untouched.
# edger_all_blobs (a Postgres text[]) is collapsed to the pipe-delimited
# string the video modal splits on.
load_pitches <- function() {
  sql <- '
    select
      play_id                                   as "PlayID",
      session_id                                as "SessionId",
      pitcher                                   as "Pitcher",
      pitcher_throws                            as "PitcherThrows",
      session_date                              as "date",
      pitch_no                                  as "PitchNo",
      time::text                                as "Time",
      tagged_pitch_type                         as "TaggedPitchType",
      rel_speed                                 as "RelSpeed",
      spin_rate                                 as "SpinRate",
      spin_axis                                 as "SpinAxis",
      tilt                                      as "Tilt",
      induced_vert_break                        as "InducedVertBreak",
      horz_break                                as "HorzBreak",
      vert_appr_angle                           as "VertApprAngle",
      rel_height                                as "RelHeight",
      rel_side                                  as "RelSide",
      extension                                 as "Extension",
      plate_loc_side                            as "PlateLocSide",
      plate_loc_height                          as "PlateLocHeight",
      spin_efficiency                           as "SpinAxis3dSpinEfficiency",
      coalesce(has_edger, false)                as "has_edger",
      edger_blob,
      array_to_string(edger_all_blobs, \'|\')   as "edger_all_blobs",
      framerate,
      clip_seconds
    from pitches
    order by session_date, pitch_no
  '
  d <- DBI::dbGetQuery(sb_pool(), sql)
  d$date <- as.Date(d$date)
  if (!"SpinAxis3dActiveSpinRate" %in% names(d)) d$SpinAxis3dActiveSpinRate <- NA_real_
  d
}

# Cheap freshness probe for reactivePoll: changes whenever the ingest job
# writes anything new.
pitches_version <- function() {
  tryCatch(
    DBI::dbGetQuery(sb_pool(),
      "select coalesce(max(updated_at)::text, '') || '-' || count(*)::text as v
       from pitches")$v,
    error = function(e) NA_character_
  )
}

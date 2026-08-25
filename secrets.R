# =============================================================================
# R/secrets.R — LOCAL MACHINE ONLY. Never commit this file.
#
# Add this line to the app repo's .gitignore BEFORE your next commit:
#   R/secrets.R
#
# This file sets the same environment variables Connect Cloud's Variables
# panel provides in deployment, so the rest of the app is identical in both
# places. Fill in every value.
# =============================================================================

Sys.setenv(
  SB_DB_HOST   = "aws-0-us-east-1.pooler.supabase.com",
  SB_DB_USER   = "bullpen_reader.ryqzkosdbksawrcrdqrc",
  SB_DB_PASS   = "PUT-BULLPEN-READER-PASSWORD-HERE",
  TM_CLIENT_ID = "CoastalCarolina-Palace",
  TM_SECRET    = "ZQ9=2v*rZdqprz]eRU[j(OADdBO0EGOx",
  AWRE_KEY     = "3ITVOIpl.BZVdd3qpVYtMdXMH1HdlqgZvkVe3Y3VJ"
)

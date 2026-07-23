#!/bin/bash
################################################################################
# setup_cron.sh — Install CRON jobs for Economic Snapshot data pulls
#
# Run once to register all scheduled data pulls:
#   bash "~/Desktop/econ_indicator_dashboard/Economic Snapshot/setup_cron.sh"
#
# To verify installs:  crontab -l
# To remove all jobs:  crontab -r   (removes ALL cron jobs — use with care)
################################################################################

RSCRIPT="/usr/local/bin/Rscript"
REPO_DIR="$HOME/Desktop/econ_indicator_dashboard/Economic Snapshot"
CODE_DIR="$REPO_DIR/R"
LOG_DIR="$REPO_DIR/data/logs"

# Create log directory if it doesn't exist
mkdir -p "$LOG_DIR"

# ---- Define jobs ----
# Format: "cron_schedule|script_name|description"
JOBS=(
  "0 3 * * 0|run_all.R|Weekly full data pull (FRED + BLS + ITA)"
  "0 4 * * 0|scrape_tsa_encounters.R|Weekly TSA checkpoint volumes"
  "0 5 * * 0|scrape_measles_cdc.R|Weekly CDC measles cases"
)

# ---- Install jobs idempotently ----
# Export current crontab to a temp file
TMPFILE=$(mktemp)
crontab -l 2>/dev/null > "$TMPFILE"

ADDED=0

for JOB in "${JOBS[@]}"; do
  SCHEDULE=$(echo "$JOB" | cut -d'|' -f1)
  SCRIPT=$(echo "$JOB" | cut -d'|' -f2)
  DESC=$(echo "$JOB" | cut -d'|' -f3)

  # cd into the repo first so here::here() (used by config.R) finds the
  # project root via the .here file.
  FULL_CMD="$SCHEDULE cd \"$REPO_DIR\" && $RSCRIPT \"$CODE_DIR/$SCRIPT\" >> \"$LOG_DIR/cron_output.log\" 2>&1"

  # Only add if this exact script is not already in crontab
  if grep -qF "$SCRIPT" "$TMPFILE"; then
    echo "⏭  Already installed: $DESC"
  else
    echo "$FULL_CMD" >> "$TMPFILE"
    echo "✅ Added: $DESC  [$SCHEDULE]"
    ADDED=$((ADDED + 1))
  fi
done

# Install updated crontab
crontab "$TMPFILE"
rm "$TMPFILE"

echo ""
echo "Done. $ADDED new job(s) installed."
echo ""
echo "Current crontab:"
crontab -l
echo ""
echo "NOTE: WRDS/Revelio data (run_wrds.R) requires manual 2FA and is NOT scheduled."
echo "      Run it manually from the repo root: Rscript R/run_wrds.R"

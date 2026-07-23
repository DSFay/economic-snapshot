#!/usr/bin/env bash
#
# update_tsa.sh — local half of the TSA pipeline.
#
# tsa.gov blocks the GitHub cloud runner, so this runs on a personal Mac (which
# isn't blocked) on a daily schedule (see the launchd job that calls it). It
# scrapes the latest TSA passenger volumes and, if the numbers changed, commits
# the one tracked TSA file and pushes it. The push triggers the cloud rebuild,
# which reads this committed file instead of scraping TSA itself.
#
# Failsafe: re-running when nothing changed does nothing. Safe to run by hand.

set -uo pipefail

# Resolve the repo root from this script's location (scripts/ is one level
# below the root) - the job works wherever the repo lives.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO" || { echo "Cannot cd to repo root"; exit 1; }

# launchd runs with a bare PATH; add the usual spots for R and git.
export PATH="/opt/homebrew/bin:/usr/local/bin:/Library/Frameworks/R.framework/Resources/bin:/usr/bin:/bin"

TSA_FILE="data/tsa/tsa_daily_passenger_volumes.csv"
log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"; }

log "=== TSA update starting in $REPO ==="

# 1. Scrape TSA. The scraper writes $TSA_FILE on success and nothing on failure,
#    so a bad scrape simply leaves the file unchanged (handled in step 2). A
#    non-zero exit here means a hard R error (e.g. packages not restored).
if ! Rscript R/scrape_tsa_encounters.R; then
  log "Rscript failed (is the R environment restored? run: Rscript -e 'renv::restore()'). Aborting."
  exit 1
fi

# 2. Stage the TSA file. If there's nothing new to commit AND nothing waiting to
#    be pushed from a previous run, we're done.
git add "$TSA_FILE"
if git diff --cached --quiet && [ -z "$(git log origin/main..HEAD --oneline 2>/dev/null)" ]; then
  log "No TSA change and nothing pending — done."
  exit 0
fi

# 3. Commit if there are staged changes (skipped when only pushing a stuck commit).
if ! git diff --cached --quiet; then
  git commit -m "Update TSA passenger volumes ($(date +%F))" || { log "commit failed"; exit 1; }
  log "Committed updated TSA data."
fi

# 4. Rebase onto the remote (in case you pushed edits from GitHub Desktop) and push.
if ! git pull --rebase origin main; then
  log "Rebase failed — resolve manually in $REPO. Nothing pushed."
  exit 1
fi
if git push origin main; then
  log "Pushed. The cloud rebuild will pick up the new TSA data."
else
  log "Push failed (check GitHub sign-in). Commit is saved locally; will retry next run."
  exit 1
fi

log "=== TSA update done ==="

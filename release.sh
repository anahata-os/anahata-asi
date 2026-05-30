#!/bin/bash
# ==============================================================================
#  🔵🔴 ANAHATA ASI - ULTIMATE DEVOPS RELEASE COORDINATION SCRIPT 🔴🔵
# ==============================================================================
# This script automates the complete local preparation, symmetrical version bumping,
# Javadoc generation & git preservation, and tagging for a V2 platform release.
#
# Usage:
#   chmod +x release.sh
#   ./release.sh
# ==============================================================================
set -e

# --- Colors for Professional Console Output ---
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${BLUE}==============================================================================${NC}"
echo -e "${CYAN}        🔵🔴 ANAHATA ASI - TRANSACTIONAL RELEASE COORDINATOR 🔴🔵${NC}"
echo -e "${BLUE}==============================================================================${NC}"

# --- Step 1: Pre-flight Git Status Verification ---
echo -e "\n${YELLOW}[1/5] Running pre-flight workspace verification...${NC}"
if [ -n "$(git status --porcelain)" ]; then
  echo -e "${RED}[ERROR] Your local git workspace has uncommitted changes. Please commit or stash them before releasing!${NC}"
  git status -s
  exit 1
fi
echo -e "${GREEN}[SUCCESS] Local workspace is pristine and clean.${NC}"

# --- Step 2: Interactive or Programmatic Version Capture ---
CURRENT_VERSION=$(mvn help:evaluate -Dexpression=project.version -q -DforceStdout)
echo -e "\n${CYAN}Current Repository Version : ${YELLOW}${CURRENT_VERSION}${NC}"

# If parameters are passed as arguments, use them; otherwise, prompt interactively
if [ -n "$1" ] && [ -n "$2" ]; then
  RELEASE_VERSION="$1"
  NEXT_SNAPSHOT="$2"
  echo -e "Using command-line arguments:"
  echo -e "   Target Release Version    : ${GREEN}${RELEASE_VERSION}${NC}"
  echo -e "   Next Development Snapshot : ${GREEN}${NEXT_SNAPSHOT}${NC}"
else
  read -p "Enter the TARGET RELEASE version (e.g., 1.0.0)      : " RELEASE_VERSION
  read -p "Enter the NEXT DEVELOPMENT snapshot (e.g., 1.1.0-SNAPSHOT): " NEXT_SNAPSHOT
fi

if [ -z "$RELEASE_VERSION" ] || [ -z "$NEXT_SNAPSHOT" ]; then
    echo -e "${RED}[ERROR] Release version and Next Snapshot version are mandatory!${NC}"
    exit 1
fi

# --- Step 3: Local Pre-flight Compilation Check ---
echo -e "\n${YELLOW}[2/5] Running local pre-flight compilation check...${NC}"
mvn clean install -DskipTests -ntp
echo -e "${GREEN}[SUCCESS] Local compile succeeded flawlessly.${NC}"

# --- Step 4: Symmetrical Version Bumping ---
echo -e "\n${YELLOW}[3/5] Promoting POM coordinates to release [${RELEASE_VERSION}]...${NC}"
mvn versions:set -DnewVersion=${RELEASE_VERSION} -DgenerateBackupPoms=false -ntp
echo -e "${GREEN}[SUCCESS] Symmetrical version bump completed across parent and all modules.${NC}"

# --- Step 5: Commit and Tag the Release ---
echo -e "\n${YELLOW}[4/5] Committing release modifications and tagging...${NC}"
git commit -am "chore: release V2 platform v${RELEASE_VERSION}"
git tag -a v${RELEASE_VERSION} -m "Anahata ASI v${RELEASE_VERSION} Stable GA Release"
echo -e "${GREEN}[SUCCESS] Release commit and tag [v${RELEASE_VERSION}] created successfully.${NC}"

# --- Step 6: Post-Release Snapshot Transition ---
echo -e "\n${YELLOW}[5/5] Advancing branch version to next snapshot [${NEXT_SNAPSHOT}]...${NC}"
mvn versions:set -DnewVersion=${NEXT_SNAPSHOT} -DgenerateBackupPoms=false -ntp
git commit -am "chore: next development iteration ${NEXT_SNAPSHOT}"
echo -e "${GREEN}[SUCCESS] Repository advanced cleanly to development cycle ${NEXT_SNAPSHOT}.${NC}"

echo -e "${BLUE}==============================================================================${NC}"
echo -e "${GREEN}      🎉 PREPARATION COMPLETE! RELEASE v${RELEASE_VERSION} IS FULLY STAGED! 🎉${NC}"
echo -e "${BLUE}==============================================================================${NC}"
echo -e "To complete the release and trigger the parallel GitHub Actions builders, run:"
echo -e "   ${CYAN}git push origin main --tags${NC}\n"

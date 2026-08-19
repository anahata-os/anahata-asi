@echo off
setlocal enabledelayedexpansion

echo ================================================================
echo    Anahata ASI Platform - Cross-Platform Release Engine (Windows)
echo ================================================================

:: --- Step 1: Git Status Pre-Flight Check ---
echo.
echo [1/6] Verifying git workspace cleanliness...
for /f "tokens=*" %%i in ('git status --porcelain') do (
    echo [ERROR] Your local git workspace has uncommitted changes.
    echo Please commit or stash them before releasing!
    git status -s
    exit /b 1
)
echo [SUCCESS] Local workspace is clean and ready.

:: --- Step 2: Version Parameter Resolution ---
set RELEASE_VERSION=%1
set NEXT_SNAPSHOT=%2

if "%RELEASE_VERSION%"=="" (
    set /p RELEASE_VERSION="Enter the TARGET RELEASE version (e.g., 1.1.0): "
)
if "%NEXT_SNAPSHOT%"=="" (
    set /p NEXT_SNAPSHOT="Enter the NEXT DEVELOPMENT snapshot (e.g., 1.2.0-SNAPSHOT): "
)

if "%RELEASE_VERSION%"=="" (
    echo [ERROR] Target release version is mandatory!
    exit /b 1
)
if "%NEXT_SNAPSHOT%"=="" (
    echo [ERROR] Next snapshot version is mandatory!
    exit /b 1
)

echo.
echo Target Release Version    : %RELEASE_VERSION%
echo Next Development Snapshot : %NEXT_SNAPSHOT%
echo.

:: --- Step 3: Pre-flight Compilation & Test Check ---
echo [2/6] Running local pre-flight compilation check...
call mvn clean install -DskipTests -ntp
if %ERRORLEVEL% neq 0 (
    echo [ERROR] Pre-flight Maven build failed! Aborting release.
    exit /b 1
)
echo [SUCCESS] Local build passed cleanly.

:: --- Step 4: Promote POMs to Release Version ---
echo.
echo [3/6] Promoting POMs to release version [%RELEASE_VERSION%]...
call mvn versions:set -DnewVersion=%RELEASE_VERSION% -DgenerateBackupPoms=false -ntp
if %ERRORLEVEL% neq 0 (
    echo [ERROR] Failed to set release version in POMs!
    exit /b 1
)
echo [SUCCESS] POM version updated to %RELEASE_VERSION%.

:: --- Step 5: Commit and Tag Release ---
echo.
echo [4/6] Committing release modifications and tagging v%RELEASE_VERSION%...
git commit -am "chore(release): prepare v%RELEASE_VERSION%"
if %ERRORLEVEL% neq 0 (
    echo [ERROR] Failed to commit release changes!
    exit /b 1
)

git tag -a v%RELEASE_VERSION% -m "Anahata ASI v%RELEASE_VERSION% Stable GA Release"
if %ERRORLEVEL% neq 0 (
    echo [ERROR] Failed to create git tag v%RELEASE_VERSION%!
    exit /b 1
)
echo [SUCCESS] Release commit and tag created successfully.

:: --- Step 6: Roll Over to Next Snapshot ---
echo.
echo [5/6] Rolling over to next development snapshot [%NEXT_SNAPSHOT%]...
call mvn versions:set -DnewVersion=%NEXT_SNAPSHOT% -DgenerateBackupPoms=false -ntp
if %ERRORLEVEL% neq 0 (
    echo [ERROR] Failed to set next snapshot version!
    exit /b 1
)

git commit -am "chore: open %NEXT_SNAPSHOT% development cycle"
if %ERRORLEVEL% neq 0 (
    echo [ERROR] Failed to commit next snapshot cycle!
    exit /b 1
)
echo [SUCCESS] Rolled over to %NEXT_SNAPSHOT%.

:: --- Step 7: Push Everything to GitHub ---
echo.
echo [6/6] Pushing commits and tag v%RELEASE_VERSION% to remote...
git push origin main
git push origin v%RELEASE_VERSION%
if %ERRORLEVEL% neq 0 (
    echo [WARNING] Git push encountered an issue. You can manually run: git push origin main ^&^& git push origin v%RELEASE_VERSION%
) else (
    echo [SUCCESS] Successfully pushed commits and tag v%RELEASE_VERSION% to GitHub!
)

echo.
echo ================================================================
echo    Release v%RELEASE_VERSION% is LIVE!
echo    GitHub Actions is now compiling, packaging, and publishing.
echo ================================================================

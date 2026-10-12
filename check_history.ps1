# check_history.ps1 -- Study A: safety check before the repository is made public
# (Blueprint, "Publishing a repository"; Week 4)
#
# RESULT: PASS only if every one of these returns nothing:
#   git log --all --oneline -- data/raw/original/      (the original document; W1-02)
#   git log --all --oneline -- data/raw/proprietary/   (the Wind exports)
#   git log --all --oneline -- .env                    (API keys)
# plus the private files of Week 1 (W1-08, W1-09, W1-10; handover Week 1, s.5):
#   docs/readmes/   docs/decisions.md   code/python/make_week_summary.py
# plus two stricter checks added in Week 4 (decision W4-07):
#   git log --all --oneline -- data/interim/           (Wind- and original-derived values, W2-08)
#   git ls-files -- <each path above>                  (tracked now, even if not yet committed)
# "--all" covers every branch, tag, remote-tracking branch and the stash, so a file committed
# on any branch and deleted later is still found.
#
# Writes outputs/logs/check_history_<timestamp>.log (same line format as the other scripts).
# Exit code: 0 = PASS, 1 = FAIL, 2 = could not run (not a git repository, git missing).
#
# Usage (Command Prompt, project root):
#   powershell -ExecutionPolicy Bypass -File check_history.ps1
# This file is plain ASCII on purpose: Windows PowerShell 5.1 reads BOM-less files as ANSI.

$ErrorActionPreference = "Continue"
$root = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location -LiteralPath $root
$stamp = Get-Date -Format "yyyyMMdd-HHmmss"
$logDir = Join-Path $root "outputs\logs"
New-Item -ItemType Directory -Force -Path $logDir | Out-Null
$logPath = Join-Path $logDir ("check_history_" + $stamp + ".log")
$utf8 = New-Object System.Text.UTF8Encoding($false)
$script:nWarn = 0
$script:nErr = 0

function Write-Log([string]$level, [string]$msg) {
    $line = "{0} {1,-5} {2}" -f (Get-Date -Format "yyyy-MM-dd HH:mm:ss"), $level, $msg
    Write-Host $line
    [System.IO.File]::AppendAllText($logPath, $line + "`n", $utf8)
    if ($level -eq "WARN") { $script:nWarn++ }
    if ($level -eq "ERROR") { $script:nErr++ }
}

function Finish([int]$code) {
    $status = if ($script:nErr -gt 0) { "FAIL" } elseif ($script:nWarn -gt 0) { "WARN" } else { "OK" }
    Write-Log "INFO" ("STATUS: {0} (warnings={1}, errors={2})" -f $status, $script:nWarn, $script:nErr)
    Write-Host ("Run log: " + $logPath)
    exit $code
}

# Runs git and returns its output lines; $null plus an ERROR line if git itself fails
function Invoke-Git([string[]]$gitArgs) {
    $out = & git @gitArgs 2>&1
    if ($LASTEXITCODE -ne 0) {
        Write-Log "ERROR" ("git " + ($gitArgs -join " ") + " failed: " + (($out | ForEach-Object { "$_" }) -join " | "))
        return $null
    }
    return @($out | ForEach-Object { "$_" } | Where-Object { $_ -ne "" })
}

Write-Log "INFO" ("PowerShell " + $PSVersionTable.PSVersion.ToString() + " on " + [System.Environment]::OSVersion.VersionString)
$gitVersion = & git --version 2>&1
if ($LASTEXITCODE -ne 0) { Write-Log "ERROR" "git not found on PATH (install Git for Windows)"; Finish 2 }
if (-not (Test-Path -LiteralPath (Join-Path $root ".git"))) {
    Write-Log "ERROR" ("not a git repository: " + $root + " (run from the project root)")
    Finish 2
}
Write-Log "INFO" ($gitVersion + "; repository " + $root)
$branch = Invoke-Git @("rev-parse", "--abbrev-ref", "HEAD")
$head = Invoke-Git @("rev-parse", "--short", "HEAD")
$remote = Invoke-Git @("remote", "-v")
Write-Log "INFO" ("branch " + ($branch -join "") + " at " + ($head -join ""))
foreach ($r in @($remote)) { if ($r) { Write-Log "INFO" ("remote: " + $r) } }

$checks = @(
    @{ Path = "data/raw/original/";                Why = "original document (W1-02)" },
    @{ Path = "data/raw/proprietary/";             Why = "Wind exports (Blueprint 0.3)" },
    @{ Path = ".env";                              Why = "API keys (Blueprint 0.3)" },
    @{ Path = "data/interim/";                     Why = "Wind- and original-derived values (W2-08; Week 4 check)" },
    @{ Path = "docs/readmes/";                     Why = "weekly READMEs (W1-08)" },
    @{ Path = "docs/decisions.md";                 Why = "decisions log (W1-09)" },
    @{ Path = "code/python/make_week_summary.py";  Why = "week-summary script (W1-10)" }
)
$failed = 0
foreach ($c in $checks) {
    $hist = @(Invoke-Git @("log", "--all", "--oneline", "--", $c.Path))
    $tracked = @(Invoke-Git @("ls-files", "--", $c.Path))
    $nh = @($hist | Where-Object { $_ }).Count
    $nt = @($tracked | Where-Object { $_ }).Count
    if ($nh -eq 0 -and $nt -eq 0) {
        Write-Log "INFO" ("CHECK PASS  {0,-34} never committed on any ref, not tracked  [{1}]" -f $c.Path, $c.Why)
    } else {
        $failed++
        Write-Log "ERROR" ("CHECK FAIL  {0,-34} {1} commit(s) in the history, {2} file(s) tracked now  [{3}]" -f $c.Path, $nh, $nt, $c.Why)
        foreach ($l in ($hist | Where-Object { $_ } | Select-Object -First 10)) { Write-Log "INFO" ("    commit  " + $l) }
        foreach ($l in ($tracked | Where-Object { $_ } | Select-Object -First 10)) { Write-Log "INFO" ("    tracked " + $l) }
    }
}

# .gitignore should list every protected path (information: a missing line is how leaks start)
$gi = Join-Path $root ".gitignore"
if (Test-Path -LiteralPath $gi) {
    $lines = Get-Content -LiteralPath $gi | ForEach-Object { $_.Trim().TrimEnd("/") }
    foreach ($c in $checks) {
        if ($lines -notcontains $c.Path.TrimEnd("/")) { Write-Log "WARN" (".gitignore has no line " + $c.Path) }
    }
} else {
    Write-Log "WARN" ".gitignore not found"
}

if ($failed -eq 0 -and $script:nErr -eq 0) {
    Write-Log "INFO" "RESULT: PASS  nothing protected is, or ever was, in the git history. The repository may be made public."
    Finish 0
} else {
    Write-Log "ERROR" "RESULT: FAIL  do NOT make the repository public. Remove the files from the history first (README_week4, Step 9)."
    Finish 1
}

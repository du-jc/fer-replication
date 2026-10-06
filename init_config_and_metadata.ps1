# init_config_and_metadata.ps1 -- Study A, Week 1 (Blueprint 0.2, 0.3, Week 1 setup)
#
# Run from the PROJECT ROOT (fer-replication\), after create_skeleton.ps1:
#
#   powershell -ExecutionPolicy Bypass -File init_config_and_metadata.ps1
#
# Creates, only if they do not exist yet (NEVER overwrites):
#   .gitignore                            (if it exists: appends only missing REQUIRED lines)
#                                          data/raw/original/ (W1-02), docs/readmes/ (W1-08) and
#                                          docs/decisions.md (W1-09) and
#                                          code/python/make_week_summary.py (W1-10) are required too
#   .env.example
#   data\metadata\source_ledger.csv        header only
#   data\metadata\transformation_log.csv   header only
#   docs\decisions.md                      header and entry template only
# Writes a run log to outputs\logs\init_config_and_metadata_<timestamp>.log

$ErrorActionPreference = 'Stop'
$Stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
$Lines = New-Object System.Collections.Generic.List[string]
$script:Warnings = 0
$script:Errors = 0
$Utf8NoBom = New-Object System.Text.UTF8Encoding($false)

function Write-Log([string]$Level, [string]$Message) {
    $line = '{0} {1,-5} {2}' -f (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), $Level, $Message
    Write-Host $line
    $Lines.Add($line)
    if ($Level -eq 'WARN') { $script:Warnings++ }
    if ($Level -eq 'ERROR') { $script:Errors++ }
}

function New-FileIfAbsent([string]$RelPath, [string]$Content) {
    $p = Join-Path (Get-Location).Path $RelPath
    if (Test-Path $p) {
        Write-Log 'INFO' "exists, not overwritten: $RelPath"
        return
    }
    $dir = Split-Path -Parent $p
    if (-not (Test-Path $dir)) { New-Item -ItemType Directory -Path $dir -Force | Out-Null }
    $text = ($Content -replace "`r`n", "`n").TrimEnd("`n") + "`n"
    [System.IO.File]::WriteAllText($p, $text, $Utf8NoBom)
    Write-Log 'INFO' "created: $RelPath"
}

# ---- contents (headers and templates only; no data) -------------------------------------
$RequiredIgnore = @(
    '.env',
    '.venv/',
    'renv/library/',
    '.Rhistory',
    '.RData',
    'data/raw/proprietary/',
    'data/interim/',
    'data/raw/original/',
    'docs/readmes/',
    'docs/decisions.md',
    'code/python/make_week_summary.py'
)
$ExtraIgnore = @(
    '# Word/Excel lock files, Python and R caches, OS files',
    '~$*',
    '__pycache__/',
    '*.pyc',
    '.Rproj.user/',
    'renv/staging/',
    'Thumbs.db',
    'desktop.ini'
)
$GitIgnore = (@('# Blueprint 0.2 / Week 1 setup: required entries (+ data/raw/original/ W1-02, docs/readmes/ W1-08, docs/decisions.md W1-09, make_week_summary.py W1-10)') + $RequiredIgnore + @('') + $ExtraIgnore) -join "`n"

$EnvExample = @'
# Copy this file to .env and fill in values. NEVER commit .env (it is in .gitignore).
# Study A needs no API keys. The line below is kept only as an example of the format.
# EXAMPLE_API_KEY=
'@

$LedgerHeader = 'series_name,provider,original_file_name,download_date,source_url_or_access_note,series_key_or_code,frequency,unit,currency,seasonal_adjustment_status,coverage_start,coverage_end,acquisition_method,layer,license_or_access_restriction,transformation_script,notes'

$TransformHeader = 'timestamp_utc,script,step,input_file,input_sha256,output_file,description,n_affected,decision_ref'

$Decisions = @'
# Decisions log -- fer-replication (Study A)

Append-only. One entry per decision, numbered W<week>-<nn>. Do not edit an old entry;
add a new one that supersedes it. Code comments cite these numbers.

Template (copy below the last entry):

    ## W<week>-<nn> Short title
    - **Date**: YYYY-MM-DD
    - **Decision**:
    - **Options considered**:
    - **Evidence**:
    - **Status**: proposed | decided | superseded by W?-??
    - **Would change if**:

---
'@

# ---- run ----------------------------------------------------------------------------------
try {
    Write-Log 'INFO' "PowerShell $($PSVersionTable.PSVersion); running in $((Get-Location).Path)"
    foreach ($need in @('code\python', 'data\metadata', 'outputs\logs', 'docs')) {
        if (-not (Test-Path $need)) {
            throw "Folder '$need' not found. Run this from the fer-replication root, after create_skeleton.ps1."
        }
    }

    if (Test-Path '.gitignore') {
        $have = Get-Content '.gitignore' | ForEach-Object { $_.Trim() }
        $missing = $RequiredIgnore | Where-Object { $have -notcontains $_ }
        if ($missing) {
            $add = "`n# added by init_config_and_metadata.ps1 (required, Blueprint Week 1)`n" + ($missing -join "`n") + "`n"
            [System.IO.File]::AppendAllText((Join-Path (Get-Location).Path '.gitignore'), $add, $Utf8NoBom)
            Write-Log 'WARN' (".gitignore existed without required lines; appended: " + ($missing -join ', '))
        } else {
            Write-Log 'INFO' 'exists, not overwritten: .gitignore (all required lines present)'
        }
    } else {
        New-FileIfAbsent '.gitignore' $GitIgnore
    }
    New-FileIfAbsent '.env.example' $EnvExample
    New-FileIfAbsent 'data\metadata\source_ledger.csv' $LedgerHeader
    New-FileIfAbsent 'data\metadata\transformation_log.csv' $TransformHeader
    New-FileIfAbsent 'docs\decisions.md' $Decisions

    # final check of the safety-critical lines
    $have = Get-Content '.gitignore' | ForEach-Object { $_.Trim() }
    foreach ($r in $RequiredIgnore) {
        if ($have -contains $r) { Write-Log 'INFO' "CHECK PASS  .gitignore contains $r" }
        else { Write-Log 'ERROR' "CHECK FAIL  .gitignore lacks $r" }
    }
} catch {
    Write-Log 'ERROR' $_.Exception.Message
}

$Status = if ($script:Errors -gt 0) { 'FAIL' } elseif ($script:Warnings -gt 0) { 'WARN' } else { 'OK' }
Write-Log 'INFO' ("STATUS: {0} (warnings={1}, errors={2})" -f $Status, $script:Warnings, $script:Errors)

$LogDir = Join-Path (Get-Location).Path 'outputs\logs'
if (-not (Test-Path $LogDir)) { $LogDir = (Get-Location).Path }
$LogPath = Join-Path $LogDir ("init_config_and_metadata_{0}.log" -f $Stamp)
[System.IO.File]::WriteAllText($LogPath, (($Lines -join "`n") + "`n"), $Utf8NoBom)
Write-Host "Run log: $LogPath"
if ($script:Errors -gt 0) { exit 1 } else { exit 0 }

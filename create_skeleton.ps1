# create_skeleton.ps1 -- Study A, Week 1 (Blueprint 0.2 and A.1)
#
# Creates the folder tree of fer-replication\ (folders only; no files except its run log).
# Run it from the PARENT folder, i.e. the folder that will CONTAIN fer-replication:
#
#   powershell -ExecutionPolicy Bypass -File create_skeleton.ps1
#
# Safe to rerun: existing folders are reported and left alone; nothing is deleted.
# Writes a run log to fer-replication\outputs\logs\create_skeleton_<timestamp>.log

$ErrorActionPreference = 'Stop'
$ProjectName = 'fer-replication'
$Stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
$Lines = New-Object System.Collections.Generic.List[string]
$Warnings = 0
$Errors = 0

function Write-Log([string]$Level, [string]$Message) {
    $line = '{0} {1,-5} {2}' -f (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), $Level, $Message
    Write-Host $line
    $Lines.Add($line)
}

$Folders = @(
    'config',
    'data\raw\original', 'data\raw\safe', 'data\raw\proprietary',
    'data\interim', 'data\processed', 'data\metadata',
    'code\python', 'code\R',
    'outputs\tables', 'outputs\figures', 'outputs\logs',
    'docs\readmes',
    'tests'
)

$Here = (Get-Location).Path
$Root = Join-Path $Here $ProjectName
try {
    Write-Log 'INFO' "PowerShell $($PSVersionTable.PSVersion); running in $Here"
    if ((Split-Path -Leaf $Here) -eq $ProjectName) {
        throw "You are INSIDE $ProjectName. cd .. to the parent folder and run again."
    }
    if (Test-Path $Root) {
        Write-Log 'INFO' "$ProjectName already exists; only missing folders will be created"
    }
    foreach ($f in $Folders) {
        $p = Join-Path $Root $f
        if (Test-Path $p) {
            Write-Log 'INFO' ("exists   {0}\{1}" -f $ProjectName, $f)
        } else {
            New-Item -ItemType Directory -Path $p -Force | Out-Null
            Write-Log 'INFO' ("created  {0}\{1}" -f $ProjectName, $f)
        }
    }
    Write-Log 'INFO' "Next: extract the week-1 files into $Root, open it in VS Code"
} catch {
    $Errors++
    Write-Log 'ERROR' $_.Exception.Message
}

$Status = if ($Errors -gt 0) { 'FAIL' } elseif ($Warnings -gt 0) { 'WARN' } else { 'OK' }
Write-Log 'INFO' ("STATUS: {0} (warnings={1}, errors={2})" -f $Status, $Warnings, $Errors)

# Write the run log (UTF-8 without BOM). If the project folder could not be made, log here.
$LogDir = Join-Path $Root 'outputs\logs'
if (-not (Test-Path $LogDir)) { $LogDir = $Here }
$LogPath = Join-Path $LogDir ("create_skeleton_{0}.log" -f $Stamp)
$Utf8NoBom = New-Object System.Text.UTF8Encoding($false)
[System.IO.File]::WriteAllText($LogPath, (($Lines -join "`n") + "`n"), $Utf8NoBom)
Write-Host "Run log: $LogPath"
if ($Errors -gt 0) { exit 1 } else { exit 0 }

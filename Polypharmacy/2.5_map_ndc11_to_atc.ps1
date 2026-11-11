# Project: Komodo_generalizability - shareable index-based polypharmacy
# Author: Nemo Zhou
# Date started: 2026-11-09
# Date last updated: 2026-11-09
# Purpose: Run the bundled n2c-based mapper for one explicitly named mapping
# version and relationship-source policy (default ATCPROD-only). Uses Python
# 3.9+ standard library only; installs no packages.

[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)][string]$MappingVersion,
  [string]$StudyRoot = (Get-Location).Path,
  [ValidateSet("atcprod_only", "n2c_combined")][string]$RelationPolicy = "atcprod_only",
  [string]$Python = "python"
)
$ErrorActionPreference = "Stop"
$root = (Resolve-Path -LiteralPath $StudyRoot).Path
$outputRoot = Join-Path $root "Outputs\4_shareable_polypharmacy"
$inputFile = Join-Path $outputRoot "unique_ndc11.txt"
if (-not (Test-Path -LiteralPath $inputFile)) {
  throw "Run 2_prepare_polypharmacy_inputs.R first. Missing: $inputFile"
}
if ($MappingVersion -notmatch '^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$') {
  throw "Use a simple MappingVersion label of at most 128 characters."
}
$mappingDir = Join-Path (Join-Path $outputRoot "mapping") $MappingVersion
& $Python (Join-Path $PSScriptRoot "n2c_mapper.py") --input $inputFile --output-dir $mappingDir --mapping-version $MappingVersion --relation-policy $RelationPolicy
if ($LASTEXITCODE -ne 0) { throw "Mapping failed. The cache can be reused when this command is retried." }
Write-Host "Next run 3_build_polypharmacy_exposures.R, then 4_calculate_and_validate_polypharmacy.R."

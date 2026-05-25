param(
  [string]$ImageName = "jeffreybbrown/skg:latest"
)

$ErrorActionPreference = "Stop"

Write-Host "Checking Docker..."
docker version | Out-Host

Write-Host "Pulling $ImageName ..."
docker pull $ImageName

Write-Host ""
Write-Host "Installed Docker image: $ImageName"
Write-Host "Run .\run.ps1 when you are ready to start Skg."

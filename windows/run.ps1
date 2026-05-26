param(
  [string]$ImageName = "jeffreybbrown/skg:latest",
  [string]$ContainerName = "skg-windows"
)

$ErrorActionPreference = "Stop"

$ScriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$SettingsPath = Join-Path $ScriptRoot ".skg-windows-settings.json"
$ContainerDataRoot = "/data"

function Read-Settings {
  if (Test-Path $SettingsPath) {
    return Get-Content $SettingsPath -Raw | ConvertFrom-Json
  }
  return [pscustomobject]@{}
}

function Save-Settings($Settings) {
  $Settings | ConvertTo-Json | Set-Content -Path $SettingsPath -Encoding UTF8
}

function Prompt-With-Default($Prompt, $Default) {
  $suffix = if ($Default) { " [$Default]" } else { "" }
  $answer = Read-Host "$Prompt$suffix"
  if ([string]::IsNullOrWhiteSpace($answer)) {
    return $Default
  }
  return $answer
}

function Resolve-DataRoot($Settings) {
  $default = $Settings.DataRoot
  if ([string]::IsNullOrWhiteSpace($default)) {
    $default = Join-Path $HOME "skg-data"
  }
  $chosen = Prompt-With-Default "Data folder" $default
  return [System.IO.Path]::GetFullPath(
    [Environment]::ExpandEnvironmentVariables($chosen))
}

function Is-Directory-Empty-Or-GitOnly($Path) {
  if (!(Test-Path $Path)) {
    return $true
  }
  $items = @(Get-ChildItem -Force -LiteralPath $Path)
  if ($items.Count -eq 0) {
    return $true
  }
  if ($items.Count -eq 1 -and $items[0].Name -eq ".git") {
    return $true
  }
  return $false
}

function Get-SkgConfigSources($ConfigPath) {
  $sources = @()
  $current = $null
  foreach ($line in Get-Content -LiteralPath $ConfigPath) {
    if ($line -match '^\s*\[\[sources\]\]') {
      if ($current) { $sources += $current }
      $current = [pscustomobject]@{ Name = $null; Path = $null }
    } elseif ($current -and $line -match '^\s*name\s*=\s*"([^"]+)"') {
      $current.Name = $Matches[1]
    } elseif ($current -and $line -match '^\s*path\s*=\s*"([^"]+)"') {
      $current.Path = $Matches[1]
    }
  }
  if ($current) { $sources += $current }
  return $sources
}

function Get-SkgConfigPort($ConfigPath) {
  foreach ($line in Get-Content -LiteralPath $ConfigPath) {
    if ($line -match '^\s*port\s*=\s*([0-9]+)') {
      return [int]$Matches[1]
    }
  }
  return 1731
}

function Validate-Existing-DataRoot($DataRoot) {
  $config = Join-Path $DataRoot "skgconfig.toml"
  if (!(Test-Path $config)) {
    return $false
  }
  $sources = @(Get-SkgConfigSources $config)
  if ($sources.Count -eq 0) {
    throw "Found $config, but it contains no [[sources]] entries."
  }
  foreach ($source in $sources) {
    if ([string]::IsNullOrWhiteSpace($source.Path)) {
      throw "Source '$($source.Name)' in $config has no path."
    }
    if ([System.IO.Path]::IsPathRooted($source.Path)) {
      $repoPath = $source.Path
    } else {
      $repoPath = Join-Path $DataRoot $source.Path
    }
    if (!(Test-Path $repoPath -PathType Container)) {
      throw "Source '$($source.Name)' points to missing folder: $repoPath"
    }
    if (!(Test-Path (Join-Path $repoPath ".git") -PathType Container)) {
      throw "Source '$($source.Name)' exists but is not a git repository: $repoPath"
    }
  }
  Write-Host "Using existing data root: $DataRoot"
  return $true
}

function Initialize-DataRoot($DataRoot) {
  $public = Join-Path $DataRoot "public"
  $private = Join-Path $DataRoot "private"
  foreach ($repo in @($public, $private)) {
    if (!(Is-Directory-Empty-Or-GitOnly $repo)) {
      throw "Refusing to initialize because this folder is nonempty: $repo"
    }
  }
  New-Item -ItemType Directory -Force -Path $public, $private | Out-Null
  foreach ($repo in @($public, $private)) {
    if (!(Test-Path (Join-Path $repo ".git"))) {
      git -C $repo init | Out-Host
    }
  }
  @"
db_name = "skg"
tantivy_folder = ".index.tantivy"
port = 1731
beep_when_server_becomes_available = false

[[sources]]
name = "public"
path = "public"
user_owns_it = true

[[sources]]
name = "private"
path = "private"
user_owns_it = true
"@ | Set-Content -Path (Join-Path $DataRoot "skgconfig.toml") -Encoding UTF8
  @"
public
private
"@ | Set-Content -Path (Join-Path $DataRoot "list-of-repositories.txt") -Encoding UTF8
  $bash = Join-Path $DataRoot "bash"
  New-Item -ItemType Directory -Force -Path $bash | Out-Null
  Copy-Item -Force -Path (Join-Path $ScriptRoot "starter-data/bash/*.sh") -Destination $bash
  Write-Host "Initialized data root: $DataRoot"
}

function Find-EmacsCandidates {
  $candidates = @()
  $cmd = Get-Command "emacsclientw.exe" -ErrorAction SilentlyContinue
  if ($cmd) { $candidates += $cmd.Source }
  $roots = @(
    "$env:ProgramFiles\Emacs",
    "${env:ProgramFiles(x86)}\Emacs",
    "$HOME\scoop\apps\emacs",
    "C:\tools\emacs",
    "C:\emacs"
  ) | Where-Object { $_ -and (Test-Path $_) }
  foreach ($root in $roots) {
    $candidates += Get-ChildItem -Path $root -Recurse -Filter "emacsclientw.exe" -ErrorAction SilentlyContinue | Select-Object -ExpandProperty FullName
  }
  return @($candidates | Sort-Object -Unique)
}

function Choose-Emacs($Settings) {
  if ($Settings.EmacsClient -and (Test-Path $Settings.EmacsClient) -and
      $Settings.RunEmacs -and (Test-Path $Settings.RunEmacs)) {
    return [pscustomobject]@{
      Client = $Settings.EmacsClient
      RunEmacs = $Settings.RunEmacs
    }
  }
  $candidates = @(Find-EmacsCandidates)
  Write-Host ""
  Write-Host "Emacs candidates:"
  for ($i = 0; $i -lt $candidates.Count; $i++) {
    Write-Host "  $($i + 1). $($candidates[$i])"
  }
  Write-Host "  0. Type another path"
  $choice = Prompt-With-Default "Choose emacsclientw.exe" "1"
  if ($choice -eq "0" -or $candidates.Count -eq 0) {
    $client = Read-Host "Path to emacsclientw.exe"
  } else {
    $client = $candidates[[int]$choice - 1]
  }
  $runEmacs = Join-Path (Split-Path -Parent $client) "runemacs.exe"
  if (!(Test-Path $runEmacs)) {
    $runEmacs = Read-Host "Path to runemacs.exe"
  }
  return [pscustomobject]@{
    Client = $client
    RunEmacs = $runEmacs
  }
}

function Get-ElispRoot {
  $packaged = Join-Path $ScriptRoot "elisp/skg-reload.el"
  if (Test-Path $packaged) {
    return (Join-Path $ScriptRoot "elisp")
  }
  $repo = Join-Path $ScriptRoot "../elisp/skg-reload.el"
  if (Test-Path $repo) {
    return (Resolve-Path (Join-Path $ScriptRoot "../elisp")).Path
  }
  throw "Could not find elisp/skg-reload.el beside windows/ or beside the package scripts."
}

function Convert-To-ElispPath($Path) {
  return ([System.IO.Path]::GetFullPath($Path)).Replace("\", "/")
}

function Start-Or-Use-Container($DataRoot, $Port) {
  docker version | Out-Host
  $existingImage = docker inspect -f '{{.Config.Image}}' $ContainerName 2>$null
  if ($LASTEXITCODE -eq 0) {
    if ($existingImage -ne $ImageName) {
      throw "Container '$ContainerName' already exists for image '$existingImage', not '$ImageName'. Choose another -ContainerName or remove the existing container."
    }
    $mountsJson = docker inspect -f '{{json .Mounts}}' $ContainerName
    $mounts = $mountsJson | ConvertFrom-Json
    $dataMount = @($mounts | Where-Object { $_.Destination -eq $ContainerDataRoot }) | Select-Object -First 1
    if (!$dataMount) {
      throw "Container '$ContainerName' has no mount at $ContainerDataRoot. Remove it or choose another -ContainerName."
    }
    $mountedSource = [System.IO.Path]::GetFullPath($dataMount.Source)
    $requestedSource = [System.IO.Path]::GetFullPath($DataRoot)
    if ($mountedSource -ne $requestedSource) {
      throw "Container '$ContainerName' mounts '$mountedSource', but this run requested '$requestedSource'. Remove it or choose another -ContainerName."
    }
    $running = docker inspect -f '{{.State.Running}}' $ContainerName
    if ($running -ne "true") {
      docker start $ContainerName | Out-Host
    }
  } else {
    docker run --name $ContainerName -d `
      --platform linux/amd64 `
      --ulimit nofile=524288:524288 `
      -p "${Port}:${Port}" `
      -v "${DataRoot}:${ContainerDataRoot}" `
      -w /opt/skg `
      $ImageName sleep infinity | Out-Host
  }
  docker exec `
    -e "SKG_CONFIG=${ContainerDataRoot}/skgconfig.toml" `
    $ContainerName `
    /opt/skg/windows/container/start-servers.sh
}

function Wait-For-Port($Port) {
  Write-Host "Waiting for Skg to listen on 127.0.0.1:$Port ..."
  for ($i = 0; $i -lt 60; $i++) {
    $client = New-Object System.Net.Sockets.TcpClient
    try {
      $async = $client.BeginConnect("127.0.0.1", $Port, $null, $null)
      if ($async.AsyncWaitHandle.WaitOne(1000)) {
        $client.EndConnect($async)
        $client.Close()
        return
      }
    } catch {
    } finally {
      $client.Close()
    }
    Start-Sleep -Seconds 1
  }
  throw "Skg did not listen on 127.0.0.1:$Port within 60 seconds. Check logs under the data folder."
}

function Invoke-Emacs($Emacs, $ElispRoot, $ConfigPath) {
  $reload = Convert-To-ElispPath (Join-Path $ElispRoot "skg-reload.el")
  $config = Convert-To-ElispPath $ConfigPath
  $expr = "(progn (load-file `"$reload`") (skg-reload) (skg-client-init `"$config`"))"
  & $Emacs.Client --eval "(emacs-pid)" *> $null
  if ($LASTEXITCODE -ne 0) {
    Write-Host "Starting Emacs and its server..."
    & $Emacs.RunEmacs --eval "(progn (require 'server) (unless (server-running-p) (server-start)))"
    Start-Sleep -Seconds 3
  }
  Write-Host "Loading Skg into Emacs..."
  & $Emacs.Client --eval $expr | Out-Host
}

$settings = Read-Settings
$dataRoot = Resolve-DataRoot $settings
New-Item -ItemType Directory -Force -Path $dataRoot | Out-Null
if (!(Validate-Existing-DataRoot $dataRoot)) {
  Initialize-DataRoot $dataRoot
}
$configPath = Join-Path $dataRoot "skgconfig.toml"
$port = Get-SkgConfigPort $configPath
$emacs = Choose-Emacs $settings
$elispRoot = Get-ElispRoot

Start-Or-Use-Container $dataRoot $port
Wait-For-Port $port
Invoke-Emacs $emacs $elispRoot $configPath

$settings | Add-Member -Force -NotePropertyName DataRoot -NotePropertyValue $dataRoot
$settings | Add-Member -Force -NotePropertyName EmacsClient -NotePropertyValue $emacs.Client
$settings | Add-Member -Force -NotePropertyName RunEmacs -NotePropertyValue $emacs.RunEmacs
Save-Settings $settings

Write-Host ""
Write-Host "Skg is starting. If Emacs reports that the server is initializing, wait a moment and retry the Skg command."
Write-Host "Data logs are under: $(Join-Path $dataRoot 'logs')"

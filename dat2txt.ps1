param([string]$DatFile, [int]$RecordLength)

$inputPath = Resolve-Path $DatFile -ErrorAction Stop | Select-Object -ExpandProperty Path
$txtFile = [System.IO.Path]::ChangeExtension($inputPath, ".txt")

$bytes = [System.IO.File]::ReadAllBytes($inputPath)
$recordCount = [Math]::Floor($bytes.Length / $RecordLength)

$outputLines = @()
for ($i = 0; $i -lt $recordCount; $i++) {
    $start = $i * $RecordLength
    $recordBytes = $bytes[$start..($start + $RecordLength - 1)]
    $recordStr = [System.Text.Encoding]::ASCII.GetString($recordBytes)
    $outputLines += $recordStr.TrimEnd()
}

[System.IO.File]::WriteAllLines($txtFile, $outputLines)
Write-Host "Saved: $txtFile"
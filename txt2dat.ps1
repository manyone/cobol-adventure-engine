param([string]$TxtFile, [int]$RecordLength)

$inputPath = Resolve-Path $TxtFile -ErrorAction Stop | Select-Object -ExpandProperty Path
$datFile = [System.IO.Path]::ChangeExtension($inputPath, ".dat")

$lines = Get-Content $inputPath | Where-Object { $_ -ne '' }

$outputBytes = New-Object System.Collections.Generic.List[Byte]
foreach ($line in $lines) {
    if ($line.Length -gt $RecordLength) { $line = $line.Substring(0, $RecordLength) }
    $line = $line.PadRight($RecordLength, ' ')
    $outputBytes.AddRange([System.Text.Encoding]::ASCII.GetBytes($line))
}

[System.IO.File]::WriteAllBytes($datFile, $outputBytes.ToArray())
Write-Host "Saved: $datFile"
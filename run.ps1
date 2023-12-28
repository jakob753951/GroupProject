py -m mkdocs build
Copy-Item -Recurse -Verbose ".\exports\" -Destination ".\site\resources"
Write-Host "Done!"
Write-Output $CR_PAT | docker login ghcr.io -u $GH_USER --password-stdin

docker build --build-arg="GITHUB_AUTH=$GH_PAT" -t ghcr.io/wacl-york/eddy4r.york:0.1 .

docker push ghcr.io/wacl-york/eddy4r.york:0.1
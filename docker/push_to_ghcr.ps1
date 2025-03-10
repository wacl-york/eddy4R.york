Write-Output $CR_PAT | docker login ghcr.io -u $GH_USER --password-stdin

docker build --no-cache -t ghcr.io/wacl-york/eddy4r.york:0.1.1 .

docker push ghcr.io/wacl-york/eddy4r.york:0.1.1
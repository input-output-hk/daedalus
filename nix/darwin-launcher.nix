{ runCommand, go }:

runCommand "darwin-launcher" { buildInputs = [ go ]; } ''
  export HOME=$NIX_BUILD_TOP
  mkdir -pv $out/bin/
  cp -vi ${./darwin-launcher.go} darwin-launcher.go
  CGO_ENABLED=0 go build -a -o $out/bin/darwin-launcher
''

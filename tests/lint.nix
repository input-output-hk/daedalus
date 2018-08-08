{runCommand, source, rawapp, nodejs-8_x }:
runCommand "daedalus-lint-ci" { buildInputs = [ nodejs-8_x ]; } ''
  export NO_UPDATE_NOTIFIER=1
  ln -s ${rawapp.node_modules} node_modules
  cp -a ${source}/. .
  npm run lint
  EXIT_CODE=$?
  if [ $EXIT_CODE == 0 ]
  then
    echo $EXIT_CODE > $out
    exit 0
  fi
''

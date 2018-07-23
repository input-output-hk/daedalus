{runCommand, frontend, src }:
runCommand "daedalus-flow-ci" { } ''
  # flow command fails to ignore the files if node_modules is symlink
  # so we have to copy the whole directory to run the test
  cp -a ${src}/. .
  chmod -R u+w ./
  cp -a ${frontend.node_modules} node_modules
  node_modules/.bin/flow --quiet
  if [ $? == 0 ] || [ $? == 2 ]
  then
    echo $? > $out
  fi
  EXIT_CODE=$?
  if [ $EXIT_CODE == 0 ] || [ $EXIT_CODE == 2 ]
  then
    echo $EXIT_CODE > $out
    exit 0
  fi
''

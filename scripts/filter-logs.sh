#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

myDir=$(dirname "$0")

# --- Where are we?

uname=$(uname)
is_linux=
is_darwin=
if [ "$uname" = "Linux"  ] ; then is_linux=1  ; fi
if [ "$uname" = "Darwin" ] ; then is_darwin=1 ; fi

# --- Find `default_logfile`

default_logfile=
if [ -n "$is_darwin" ] ; then
  default_logfile="$HOME/Library/Application Support/Daedalus Mainnet/Logs/pub/Daedalus.json"
  :
  if [ ! -e "$default_logfile" ] ; then
    candidate=$(find "$HOME/Library/Application Support" -maxdepth 1 -mindepth 1 -type d -name 'Daedalus*' | head -n 1)
    if [ -n "$candidate" ] ; then
      default_logfile="$candidate"/Logs/pub/Daedalus.json
    fi
  fi
elif [ -n "$is_linux" ] ; then
  default_logfile="$HOME"/.local/share/Daedalus/mainnet/Logs/pub/Daedalus.json
  if [ ! -e "$default_logfile" ] ; then
    if [ -e "$HOME"/.local/share/Daedalus/ ] ; then
       candidate=$(find "$HOME"/.local/share/Daedalus/ -mindepth 1 -maxdepth 1 -type d | head -n 1)
       if [ -n "$candidate" ] ; then
         default_logfile="$candidate"/Logs/pub/Daedalus.json
       fi
    fi
  fi
fi

# --- Print usage

print_usage() {
  echo "Usage: $0 [OPTION]..."
  echo
  echo "  -h, --help                       Display help."
  echo "  -f, --follow                     Continue processing as the log file grows."
  echo "  -c, --compact                    Output compact instead of pretty-printed JSON."
  echo "  -i, --input <file>               Deadalus.json log location (default: $default_logfile)."
  echo "  -m, --msg-contains <substring>   Filter down to entries ‘.msg’ of which contains a <substring>."
  echo "      --app-contains <string>      Filter down to entries ‘.app’ of which contains a <string> element."
  echo "  -d, --data-key <key>             Filter down to entries ‘.data’ of which contains a <key> key."
  echo "      --data-kv <key> <value>      Filter down to entries ‘.data’ of which contains a <key> with value <value>."
}

# --- Parse arguments

follow_mode=
compact_mode=
input="$default_logfile"
filter_msg_contains=
filter_app_contains=
filter_data_key=
filter_data_kv_key=
filter_data_kv_value=

while [ $# -gt 0 ] ; do
  case "$1" in
    --help         | -h ) print_usage ; exit 0 ;;
    --follow       | -f ) follow_mode=1 ;;
    --compact      | -c ) compact_mode=1 ;;
    --input        | -i ) shift ; input="$1" ;;
    --msg-contains | -m ) shift ; filter_msg_contains="$1" ;;
    --app-contains      ) shift ; filter_app_contains="$1" ;;
    --data-key     | -d ) shift ; filter_data_key="$1" ;;
    --data-kv           ) shift ; filter_data_kv_key="$1" ; shift ; filter_data_kv_value="$1" ;;
    *              ) echo >&2 'fatal: unknown arguments:' "$@" ; print_usage ; exit 1 ;;
  esac
  shift
done

if [ -n "${DEBUG:-}" ] ; then
  echo 2>&1 "; follow_mode          = $follow_mode"
  echo 2>&1 "; compact_mode         = $compact_mode"
  echo 2>&1 "; input                = $input"
  echo 2>&1 "; filter_msg_contains  = $filter_msg_contains"
  echo 2>&1 "; filter_app_contains  = $filter_app_contains"
  echo 2>&1 "; filter_data_key      = $filter_data_key"
  echo 2>&1 "; filter_data_kv_key   = $filter_data_kv_key"
  echo 2>&1 "; filter_data_kv_value = $filter_data_kv_value"
fi

# --- Find jq

if type jq >/dev/null 2>&1 ; then
  :
else
  echo >&2 "warn: no ‘jq’ found, will use one from Nixpkgs"
  default_nix="$myDir"/../default.nix
  if [ -e "$default_nix" ] ; then
    jq_package=$(nix-build "$default_nix" -A pkgs.jq)
  else
    jq_package=$(nix-build '<nixpkgs>' -A pkgs.jq)
  fi
  export PATH="$jq_package/bin:$PATH"
fi

# --- Filters

read_source() {
  if [ -n "$follow_mode" ] ; then
    # 10¹⁰ ≈ ∞
    tail -n 999999999 -F "$input"
  else
    cat "$input"
  fi
}

filter_dev_log_tags() {
  sed -r 's/^\[.*\{/\{/'
}

pretty_print() {
  if [ -n "$compact_mode" ] ; then
    jq --unbuffered  -c .
  else
    # could be just `cat`, but we want colors!
    jq --unbuffered  .
  fi
}

f_msg_contains() {
  if [ -n "$filter_msg_contains" ] ; then
    jq --unbuffered --arg arg1 "$filter_msg_contains" -c 'select(.msg | contains($arg1))'
  else
    cat
  fi
}

f_app_contains() {
  if [ -n "$filter_app_contains" ] ; then
    jq --unbuffered --arg arg1 "$filter_app_contains" -c 'select(.app[] | contains($arg1))'
  else
    cat
  fi
}

f_data_key() {
  if [ -n "$filter_data_key" ] ; then
    jq --unbuffered --arg key1 "$filter_data_key" -c 'select(.data | .[$key1])'
  else
    cat
  fi
}

f_data_kv() {
  if [ -n "$filter_data_kv_key" ] ; then
    jq --unbuffered --arg key1 "$filter_data_kv_key" --arg value1 "$filter_data_kv_value" -c 'select(.data | .[$key1] == $value1)'
  else
    cat
  fi
}

read_source | filter_dev_log_tags | f_msg_contains | f_app_contains | f_data_key | f_data_kv | pretty_print

#!/usr/bin/env bash
set -euo pipefail

if [[ -t 1 ]]; then
  color_reset=$'\033[0m'
  color_bold=$'\033[1m'
  color_red=$'\033[31m'
  color_green=$'\033[32m'
  color_yellow=$'\033[33m'
  color_blue=$'\033[34m'
  color_magenta=$'\033[35m'
  color_cyan=$'\033[36m'
else
  color_reset=''
  color_bold=''
  color_red=''
  color_green=''
  color_yellow=''
  color_blue=''
  color_magenta=''
  color_cyan=''
fi

usage() {
  printf '%sUsage:%s %s --tasks=<file> --prompt=<file> [--max-iterations=<n>]\n' \
    "$color_bold" "$color_reset" "$0"
  printf '\n'
  printf '%sOptions:%s\n' "$color_bold" "$color_reset"
  printf '  %s--tasks=<file>%s           Path to the tasks JSON file\n' "$color_cyan" "$color_reset"
  printf '  %s--prompt=<file>%s          Path to the prompt file passed to opencode\n' "$color_cyan" "$color_reset"
  printf '  %s--max-iterations=<n>%s     Maximum opencode runs before giving up (default: 10)\n' "$color_cyan" "$color_reset"
  printf '  %s--help, -h%s               Show this help message\n' "$color_cyan" "$color_reset"
  printf '\n'
  printf '%sExamples:%s\n' "$color_bold" "$color_reset"
  printf '  %s%s --tasks=.agent/plans/agentic/knowledge-base-platform-tasks.json --prompt=.ralph/prompt.txt%s\n' \
    "$color_magenta" "$0" "$color_reset"
  printf '  %s%s --tasks=tasks.json --prompt=prompt.txt --max-iterations=5%s\n' \
    "$color_magenta" "$0" "$color_reset"
}

error() {
  printf '%s[ERROR]%s %s\n' "$color_red" "$color_reset" "$1" >&2
}

section() {
  printf '\n%s== %s ==%s\n' "$color_bold$color_blue" "$1" "$color_reset"
}

info() {
  printf '%s[INFO]%s %s\n' "$color_cyan" "$color_reset" "$1"
}

success() {
  printf '%s[OK]%s %s\n' "$color_green" "$color_reset" "$1"
}

warn() {
  printf '%s[WARN]%s %s\n' "$color_yellow" "$color_reset" "$1"
}

tasks_file=''
prompt_file=''
max_iterations=10
iterations_completed=0
start_head=''
stop_reason_prefix='RALPH_STOP_REASON='
resume_hint=''

for arg in "$@"; do
  case "$arg" in
    --tasks=*)
      tasks_file="${arg#*=}"
      ;;
    --prompt=*)
      prompt_file="${arg#*=}"
      ;;
    --max-iterations=*)
      max_iterations="${arg#*=}"
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      error "Unknown argument: $arg"
      usage >&2
      exit 1
      ;;
  esac
done

if [[ -z "$tasks_file" || -z "$prompt_file" ]]; then
  error '--tasks=<file> and --prompt=<file> are required'
  usage >&2
  exit 1
fi

if [[ ! -r "$tasks_file" ]]; then
  error "Tasks file is not readable: $tasks_file"
  exit 1
fi

if [[ ! -r "$prompt_file" ]]; then
  error "Prompt file is not readable: $prompt_file"
  exit 1
fi

if [[ ! "$max_iterations" =~ ^[1-9][0-9]*$ ]]; then
  error "--max-iterations must be a positive integer: $max_iterations"
  exit 1
fi

for command_name in jq git opencode; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    error "Required command not found in PATH: $command_name"
    exit 1
  fi
done

if start_head=$(git rev-parse --verify HEAD 2>/dev/null); then
  :
else
  start_head=''
fi

print_commit_summary() {
  local commits

  section 'Git Commits'

  if [[ -z "$start_head" ]]; then
    if ! commits=$(git log --oneline 2>/dev/null); then
      commits=''
    fi
  else
    if ! commits=$(git log --oneline "${start_head}..HEAD" 2>/dev/null); then
      commits=''
    fi
  fi

  if [[ -n "$commits" ]]; then
    printf '%s\n' "$commits"
  else
    printf '%snone%s\n' "$color_yellow" "$color_reset"
  fi
}

exit_with_summary() {
  local exit_code="$1"
  local outcome_message="$2"

  section 'Run Summary'
  if [[ "$exit_code" -eq 0 ]]; then
    success "$outcome_message"
  else
    warn "$outcome_message"
  fi
  printf '%sIterations completed:%s %s/%s\n' "$color_bold" "$color_reset" "$iterations_completed" "$max_iterations"
  printf '%sTasks file:%s %s%s%s\n' "$color_bold" "$color_reset" "$color_magenta" "$tasks_file" "$color_reset"
  printf '%sPrompt file:%s %s%s%s\n' "$color_bold" "$color_reset" "$color_magenta" "$prompt_file" "$color_reset"
  if [[ -n "$resume_hint" ]]; then
    printf '%sResume:%s %s%s%s\n' "$color_bold" "$color_reset" "$color_magenta" "$resume_hint" "$color_reset"
  fi
  print_commit_summary
  exit "$exit_code"
}

get_task_counts() {
  jq -r '
    [ .phases[]?.tasks[]? ] as $tasks
    | [
        ($tasks | length),
        ([ $tasks[] | select((.status // "") != "completed") ] | length)
      ]
    | @tsv
  ' "$tasks_file"
}

for ((iteration = 1; iteration <= max_iterations; iteration++)); do
  if ! counts=$(get_task_counts); then
    exit_with_summary 1 "Failed to parse tasks file with jq: $tasks_file"
  fi

  IFS=$'\t' read -r total_tasks remaining_tasks <<<"$counts"

  if [[ "$remaining_tasks" == '0' ]]; then
    if [[ "$total_tasks" == '0' ]]; then
      exit_with_summary 0 'All tasks are already completed; the tasks file contains zero tasks.'
    fi

    exit_with_summary 0 "All tasks are completed after ${iterations_completed} iteration(s)."
  fi

  section "Iteration ${iteration}/${max_iterations}"
  info "Tasks remaining: ${remaining_tasks}/${total_tasks}"
  info "Launching: opencode run < ${prompt_file}"

  opencode_output_file=$(mktemp)

  set +e
  opencode run < "$prompt_file" 2>&1 | tee "$opencode_output_file"
  opencode_status=${PIPESTATUS[0]}
  set -e

  iterations_completed="$iteration"

  stop_reason=''
  while IFS= read -r output_line; do
    if [[ "$output_line" == ${stop_reason_prefix}* ]]; then
      stop_reason="${output_line#${stop_reason_prefix}}"
    fi
  done < "$opencode_output_file"

  rm -f "$opencode_output_file"

  if [[ -n "$stop_reason" ]]; then
    resume_hint='opencode --continue'
    exit_with_summary 0 "opencode paused for ${stop_reason//_/ } after iteration ${iteration}."
  fi

  if [[ "$opencode_status" -ne 0 ]]; then
    exit_with_summary "$opencode_status" "opencode exited with status ${opencode_status} on iteration ${iteration}."
  fi

  success "opencode completed iteration ${iteration}."
done

if ! counts=$(get_task_counts); then
  exit_with_summary 1 "Failed to parse tasks file with jq: $tasks_file"
fi

IFS=$'\t' read -r total_tasks remaining_tasks <<<"$counts"
exit_with_summary 1 "Reached max iterations (${max_iterations}) with ${remaining_tasks} of ${total_tasks} task(s) still not completed."

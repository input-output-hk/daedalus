let
  ciInputName = "GitHub event";
  repository = "input-output-hk/daedalus";
in
rec {

  # XXX: We cross-build for Windows on Linux, but want a separate
  # GitHub Check for both Linux, and Windows targets. (Separate Cicero
  # tasks still run in the same Nomad container, so that wouldn’t be
  # useful.)
  #
  # Since Darwin builds are also being “built” inside a Linux container by
  # using the remote builders feature, we want to run Darwin in parallel
  # to Linux+Windows, running the latter two sequentially, to get it
  # faster.
  #
  # Therefore, we don’t use `preset.github.status.lib.reportBulk` with
  # all of them inside a single `reportBulk` – that would build all
  # systems sequentially.
  #
  # Moreover, we don’t want to fail the any of those sub-builds if any
  # other fails.
  #
  # Inside a build for a particular target system, we want to build
  # for target clusters (mainnet, preprod, etc.) sequentially.
  #
  # Further more, we want to fail the top-level `ci` GitHub
  # Check. That’s tricky, since `preset.github.status.lib.reportBulk`
  # doesn't carry over non-0 exit codes.

  tasks.ci = { config, lib, pkgs, ... }: {
    preset = {
      nix.enable = true;
      github.ci = {
        # Tullia tasks can run locally or on Cicero.
        # When no facts are present we know that we are running locally and vice versa.
        # When running locally, the current directory is already bind-mounted
        # into the container, so we don't need to fetch the source from GitHub
        # and we don't want to report a GitHub status.
        enable = config.actionRun.facts != { };
        inherit repository;
        revision = config.preset.github.lib.readRevision ciInputName null;
      };
    };

    command.text = let

      makeAndReportSequentiallyFor = { targetSystems }: pkgs.writeShellScript "build-and-report-seq-${lib.concatStringsSep "-" targetSystems}" ''
        set -x

        # Mark all as pending:
        ${lib.concatMapStringsSep "\n" (targetSystem: ''
          ${config.preset.github.status.lib.report} -c ${lib.escapeShellArg " (${targetSystem})"} -s pending -d Queued
        '') targetSystems}

        # Run all sequentially, reporting status:
        ${lib.concatMapStringsSep "\n" (targetSystem: ''
          ${config.preset.github.status.lib.report} -c ${lib.escapeShellArg " (${targetSystem})"} -- ${makeOneFor { inherit targetSystem; }}
        '') targetSystems}
      '';

      makeOneFor = { targetSystem }: pkgs.writeShellScript "build-one-${targetSystem}" ''
        set -o errexit
        set -o nounset
        set -o pipefail
        set -x
        export PATH="${lib.makeBinPath (with pkgs; [ curl jq gnused gnugrep ])}:$PATH"

        tr ' ' '\n' <apps/desktop/installer-clusters.cfg | while IFS= read -r cluster ; do
          echo >&2 "Building an ‘"${lib.escapeShellArg targetSystem}"’ installer for cluster ‘$cluster’…"

          outLink=./result-${lib.escapeShellArg targetSystem}

          echo >&2 "self.rev is: $(nix eval .#packages.x86_64-linux.internal.mainnet.buildRev)"

          ${checkGitStatus}

          nix build --no-accept-flake-config --out-link "$outLink" --cores 1 --max-jobs 1 -L .#packages.${lib.escapeShellArg targetSystem}.installer."$cluster" || {
            ec=$?
            echo "$ec" >$BUILD_FAILED_MARKER_FILE
            exit "$ec"
          }

          echo '{}' \
          | jq \
            --arg system ${lib.escapeShellArg targetSystem} \
            --arg cluster "$cluster" \
            --arg url "$(realpath "$outLink"/*${lib.escapeShellArg targetSystem}* | sed  -r 's,^/nix/store/,https://nar-proxy.ci.iog.io/dl/,')" \
            '.[$system][$cluster] = $url' \
          | curl "$CICERO_WEB_URL"/api/run/"$NOMAD_JOB_ID"/fact \
            --output /dev/null --fail \
            --no-progress-meter \
            --data-binary @-
        done
      '';

      runInParallel = commands: ''
        ${pkgs.parallel}/bin/parallel -j ${toString (__length commands)} -k ::: ${lib.escapeShellArgs commands}
      '';

      checkGitStatus = ''
        gitStatus=$(${pkgs.gitMinimal}/bin/git status --porcelain)
        if [ -n "$gitStatus" ] ; then
          echo >&2 "git-status is non-empty, i.e. repository is dirty, and inputs.self.sourceInfo is not available:"
          echo >&2 "$gitStatus"
          exit 1
        fi
      '';

    in ''
      ${checkGitStatus}

      BUILD_FAILED_MARKER_FILE="$(mktemp)"
      export BUILD_FAILED_MARKER_FILE

      echo >&2 "Cicero API URL: $CICERO_WEB_URL"
      echo >&2 "Nomad job ID: $NOMAD_JOB_ID"

      ${runInParallel [
        (makeAndReportSequentiallyFor { targetSystems = [ "x86_64-darwin" ]; })
        (makeAndReportSequentiallyFor { targetSystems = [ "x86_64-windows" "x86_64-linux" ]; })
      ]}

      # Propagate the last ‘nix build’ error exit code:
      maybeExitCode=$(cat "$BUILD_FAILED_MARKER_FILE")
      if [ -n "$maybeExitCode" ] ; then
        exit "$maybeExitCode"
      fi
    '';

    memory = 1024 * 32;
    nomad.resources.cpu = 10000;
    nomad.templates = [
      {
        destination = "${config.env.HOME}/.netrc";
        data = ''
          machine cicero.ci.iog.io
          login cicero
          password {{with secret "kv/data/cicero/api"}}{{.Data.data.basic}}{{end}}
        '';
      }
    ];
  };

  actions."daedalus/ci" = {
    task = "ci";  # Refer to the aggregated one.
    io = ''
      // This is a CUE expression that defines what events trigger a new run of this action.
      // There is no documentation for this yet. Ask SRE if you have trouble changing this.
      let github = {
        #input: "${ciInputName}"
        #repo: "${repository}"
      }
      #lib.merge
      #ios: [
        #lib.io.github_push & github,
        { #lib.io.github_pr, github, #target_default: false },
      ]
    '';
  };
}

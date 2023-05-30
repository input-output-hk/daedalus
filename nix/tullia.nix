let
  ciInputName = "GitHub event";
  repository = "input-output-hk/daedalus";
  githubCheckName = targetSystem: "Cicero (${targetSystem})";

  allJobs = {
    x86_64-windows = [mkInstaller];
    x86_64-darwin = [mkInstaller mkDevshell];
    x86_64-linux = [mkInstaller mkDevshell];
    aarch64-darwin = [mkInstaller mkDevshell];
  };

  # XXX: We cross-build for Windows on Linux, and want separate
  # action+task per each platform, so that they are built in parallel
  # (like Buildkite), but logs are easy to view separately, and they
  # don’t consume each other’s resources. Darwin is built remotely. See also
  # <https://github.com/input-output-hk/cicero/discussions/75>.

  actions = __listToAttrs (map (targetSystem: {
    name = "daedalus/ci/${targetSystem}";
    value = {
      task = githubCheckName targetSystem;
      io = ''
        // This is a CUE expression that defines what events trigger a new run of this action.
        // There is no documentation for this yet. Ask SRE if you have trouble changing this.
        let github = {
          #input: "${ciInputName}"
          #repo: "${repository}"
        }
        #lib.merge
        #ios: [
          { #lib.io.github_push, github, #default_branch: true },
          { #lib.io.github_pr,   github, #target_default: false },
        ]
      '';
    };
  }) (__attrNames allJobs));

  tasks = __listToAttrs (map (targetSystem: {
    name = githubCheckName targetSystem;
    value = {
      config,
      lib,
      pkgs,
      ...
    }: {
      preset = {
        nix.enable = true;
        github.ci = {
          # Tullia tasks can run locally or on Cicero.
          # When no facts are present we know that we are running locally and vice versa.
          # When running locally, the current directory is already bind-mounted
          # into the container, so we don't need to fetch the source from GitHub
          # and we don't want to report a GitHub status.
          enable = config.actionRun.facts != {};
          inherit repository;
          remote = config.preset.github.lib.readRepository ciInputName null;
          revision = config.preset.github.lib.readRevision ciInputName null;
        };
      };
      # Linux & Windows builds require 32 GB, but Darwin happens on a remote builder that we don’t control:
      memory =
        if lib.hasInfix "darwin" targetSystem
        then 8 * 1024
        else 32 * 1024;
      nomad.resources.cpu = 10000;
      nomad.driver = "exec"; # Robin Stumm’s suggestion (more reliable), not building inside a container
      nomad.templates = [
        {
          destination = "secrets/cicero-token";
          data = ''{{(secret "kv/data/cicero/sessions/lace").Data.data.token}}'';
        }
      ];

      command.text = lib.concatMapStringsSep "\n" (job:
        pkgs.writeShellScript "job-${targetSystem}-${job.name}" ''
          set -o errexit
          set -o nounset
          set -o pipefail
          echo '~~~ Starting job: '${pkgs.lib.escapeShellArg job.name}
          set -x
          ${job.command}
        '') (map (j: j {inherit pkgs targetSystem;}) (allJobs.${targetSystem}));
    };
  }) (__attrNames allJobs));

  mkInstaller = {
    pkgs,
    targetSystem,
  }: {
    name = "installer";
    command = let
      outLink = "./result-${targetSystem}";
    in ''
      retry() {
        local total_tries=$1
        shift
        local current_try=1
        while [ "$current_try" -le "$total_tries" ] ; do
          local context="‘$*’ (try: $current_try of $total_tries)"
          if "$@" ; then
            echo >&2 "info: success running $context"
            return 0
          elif [ "$current_try" == "$total_tries" ] ; then
            echo >&2 "fatal: failed to run $context"
            return 1
          else
            echo >&2 "error: failed to run $context"
            sleep 5
          fi
          current_try=$((current_try + 1))
        done
      }

      for cluster in $(cat ./installer-clusters.cfg) ; do

        installable=${pkgs.lib.escapeShellArg "packages.${targetSystem}.installer"}."$cluster"

        ${
          # XXX: this is nasty, but on Darwin, we often trigger auto-gc, which has races
          # (<https://github.com/NixOS/nix/issues/6757>, <https://github.com/NixOS/nix/issues/1970>,
          # <https://input-output-rnd.slack.com/archives/C02H2Q4L54Y/p1677172044575869>),
          # and builds fail. Furthermore, you can’t trigger a GC remotely. We also can’t control
          # `--builders` with `driver=exec`, and `driver=podman` fails randomly and often with
          # “image not found”. Let’s then trigger auto-gc in first try/tries, and then retry the build.
          # That usually works.
          if pkgs.lib.hasInfix "darwin" targetSystem
          then "retry 4"
          else ""
        } nix build --out-link ${pkgs.lib.escapeShellArg outLink} --cores 1 --max-jobs 1 -L ".#$installable"

        # XXX: create a link to the artifact:
        export PATH="${pkgs.lib.makeBinPath (with pkgs; [curl jq gnused])}:$PATH"
        jq --null-input \
          --arg system ${pkgs.lib.escapeShellArg targetSystem} \
          --arg url "$(realpath ${pkgs.lib.escapeShellArg outLink}/*${pkgs.lib.escapeShellArg targetSystem}* | sed  -r 's,^/nix/store/,https://nar-proxy.ci.iog.io/dl/,')" \
          '.[$system] = $url' \
        | curl "$CICERO_WEB_URL"/api/run/"$NOMAD_JOB_ID"/fact \
          --header @<(
            # cannot use --oauth2-bearer as that leaks the token in the CLI args
            echo -n 'Authorization: Bearer '
            cat /secrets/cicero-token
          ) \
          --output /dev/null --fail \
          --no-progress-meter \
          --data-binary @-

      done
    '';
  };

  mkDevshell = {
    pkgs,
    targetSystem,
  }: {
    name = "devshell";
    command = ''
      nix build --cores 1 --max-jobs 1 -L .#devShell.${pkgs.lib.escapeShellArg targetSystem}
    '';
  };
in {inherit actions tasks;}

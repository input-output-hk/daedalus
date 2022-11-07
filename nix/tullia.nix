self: system:

let
  inherit (self.inputs.tullia.inputs.nixpkgs) lib;

  ciInputName = "GitHub event";
in
rec {
  tasks =
    let
      inherit (self.inputs.tullia) flakeOutputTasks taskSequence;

      common =
        { config
        , ...
        }: {
          preset = {
            # needed on top-level task to set runtime options
            nix.enable = true;

            github-ci = {
              # Tullia tasks can run locally or on Cicero.
              # When no facts are present we know that we are running locally and vice versa.
              # When running locally, the current directory is already bind-mounted
              # into the container, so we don't need to fetch the source from GitHub
              # and we don't want to report a GitHub status.
              enable = config.actionRun.facts != { };
              repo = "input-output-hk/daedalus";
              sha = config.preset.github-ci.lib.getRevision ciInputName null;
            };
          };
        };

      ciTasks = __mapAttrs
        (_: flakeOutputTask: { ... }: {
          imports = [ common flakeOutputTask ];

          memory = 1024 * 8;
          nomad.resources.cpu = 10000;
        })
        (flakeOutputTasks [ "hydraJobs" system ] self);

      ciTasksSeq = taskSequence "ci/" ciTasks (__attrNames ciTasks);
    in
    ciTasks // # for running tasks separately
    ciTasksSeq // # for running in an arbitrary sequence
    {
      "ci" = { lib, ... }: {
        imports = [ common ];
        after = __attrNames ciTasksSeq;
      };
    };

  actions = {
    "daedalus/ci" = {
      task = "ci";
      io = ''
        // This is a CUE expression that defines what events trigger a new run of this action.
        // There is no documentation for this yet. Ask SRE if you have trouble changing this.
        let github = {
          #input: "${ciInputName}"
          #repo: "input-output-hk/daedalus"
        }
        #lib.merge
        #ios: [
          #lib.io.github_push & github,
          { #lib.io.github_pr, github, #target_default: false },
        ]
      '';
    };
  };
}

{ system, crossSystem, sources, pkgs, launcherConfig }:

let

  marlowe-cli = (import sources.marlowe-cardano {
    inherit system;
    crossSystem = null; # TODO: Windows is not possible yet.
  }).marlowe-cli;

  unixWelcomeMessage = ''
    Hello!

    • We set up this terminal to allow easy running of ‘marlowe-cli’.
    • It will work as long as you don’t close Daedalus.
    • If this is not your terminal of choice, please set the ‘$TERMINAL’
      environment variable before starting Daedalus.

    Try typing in ‘marlowe-cli --help’:
  '';

  unixExports = ''
    export CARDANO_NODE_SOCKET_PATH="${launcherConfig.stateDir + "/cardano-node.socket"}"
    export CARDANO_TESTNET_MAGIC=${toString (builtins.fromJSON (builtins.unsafeDiscardStringContext (
      builtins.readFile launcherConfig.nodeConfig.network.genesisFile
    ))).protocolConsts.protocolMagic}
  '';

in {

  inherit marlowe-cli;

  # It’s monstrous, I’m sorry…
  open-marlowe-term = rec {

    x86_64-windows = abort "Windows not available yet.";

    aarch64-darwin = x86_64-darwin; # Just a bash script.

    # XXX: it’s easiest to keep this in a single file for packaging, hence the `mktemp` shenaningans.
    x86_64-darwin = pkgs.writeScriptBin "open-marlowe-term" ''
      #! /bin/sh

      setup_script=$(mktemp)
      chmod +x $setup_script
      echo >>$setup_script ${pkgs.lib.escapeShellArg ''
        #! /bin/sh
        ${unixExports}
        # clear the terminal for nicer UX:
        reset
        echo
        echo ${pkgs.lib.escapeShellArg unixWelcomeMessage} | sed -r 's/^/  /'
      ''}
      echo >>$setup_script "
        rm $setup_script
        export PATH=\"$PATH\"
      "
      echo >>$setup_script ${pkgs.lib.escapeShellArg ''
        exec $SHELL -i
      ''}

      osascript -e "tell app \"Terminal\" to do script \"exec $setup_script\""
    '';

    x86_64-linux = let
      welcomeScript = pkgs.writeScript "welcomeScript" ''
        #! /bin/sh
        echo
        echo ${pkgs.lib.escapeShellArg unixWelcomeMessage} | sed -r 's/^/  /'
        exec $SHELL -i
      '';
    in pkgs.writeScriptBin "open-marlowe-term" ''
      #! ${pkgs.stdenv.shell}

      # Set environment here to be inherited, in case an exotic
      # $TERMINAL doesn’t run our welcome script.

      export PATH=${marlowe-cli}/bin:$PATH
      export XDG_DATA_HOME=''${XDG_DATA_HOME:-''${HOME}/.local/share}
      ${unixExports}

      cd $HOME

      # Terminal emulator heuristic by Han Boetes <han@mijncomputer.nl> taken from
      # <https://github.com/i3/i3/blob/9db03797da3cea5dc6898adc79a68ba4523a409c/i3-sensible-terminal>:

      for terminal in "$TERMINAL" x-terminal-emulator mate-terminal gnome-terminal terminator xfce4-terminal urxvt rxvt termit Eterm aterm uxterm xterm roxterm termite lxterminal terminology st qterminal lilyterm tilix terminix konsole kitty guake tilda alacritty hyper; do
        if command -v "$terminal" > /dev/null 2>&1; then
          nohup $terminal -e ${welcomeScript} </dev/null >/dev/null 2>&1 &
          exit 0
        fi
      done

      echo >&2 'No terminal emulator found, set ‘$TERMINAL’!'
      exit 1
    '';

  };

}

{ system, crossSystem, sources, pkgs, launcherConfig, daedalus-bridge }:

let

  inherit (pkgs.lib) escapeShellArg;

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
      echo >>$setup_script ${escapeShellArg ''
        #! /bin/sh
        ${unixExports}
        # clear the terminal for nicer UX:
        reset
        echo
        echo ${escapeShellArg unixWelcomeMessage} | sed -r 's/^/  /'
      ''}
      echo >>$setup_script "
        rm $setup_script
        export PATH=\"$PATH\"
      "
      echo >>$setup_script ${escapeShellArg ''
        exec $SHELL -i
      ''}

      # If the Terminal app is running, `activate` will not add a new window,
      # but just move the app to the foreground instead. So we need to add
      # a new window.
      #
      # If the app is not running, we activate it, which also brings it to the
      # foreground, and creates a new window. So to prevent having 2 windows,
      # we run the script in front window.

      osascript <<END
        if application "Terminal" is running then
          tell application "Terminal"
            activate
            do script "exec $setup_script"
          end tell
        else
          tell application "Terminal"
            activate
            do script "exec $setup_script" in front window
          end tell
        end if
      END
    '';

    # TODO: perhaps instead of wrapping `marlowe-cli` to be run inside
    # the nix-user-chroot, we should export `LD_LIBRARY_PATH` to point
    # to various ~/.daedalus/nix/store/… directories, and run it in
    # the host context…? – @michalrus

    x86_64-linux = let
      linuxExports = ''
        export XDG_DATA_HOME=''${XDG_DATA_HOME:-''${HOME}/.local/share}
        ${unixExports}
      '';
      welcomeScript = pkgs.writeScript "welcomeScript" ''
        #! /bin/sh

        # It is possible that the terminal emulator did not inherit the env of its
        # invocation, but instead of the Window Manager (that would happen if $TERMINAL
        # was `i3-msg exec …`). Let’s re-export here:

        ${linuxExports}

        echo
        echo ${escapeShellArg unixWelcomeMessage} | sed -r 's/^/  /'
        exec $SHELL -i
      '';
      runTerminalEmulator = initialScript: ''
        # Terminal emulator heuristic by Han Boetes <han@mijncomputer.nl> taken from
        # <https://github.com/i3/i3/blob/9db03797da3cea5dc6898adc79a68ba4523a409c/i3-sensible-terminal>:

        for terminal in "$TERMINAL" x-terminal-emulator mate-terminal gnome-terminal terminator xfce4-terminal urxvt rxvt termit Eterm aterm uxterm xterm roxterm termite lxterminal terminology st qterminal lilyterm tilix terminix konsole kitty guake tilda alacritty hyper; do
          if command -v "$terminal" > /dev/null 2>&1; then
            nohup $terminal -e ${initialScript} </dev/null >/dev/null 2>&1 &
            exit 0
          fi
        done

        echo >&2 'No terminal emulator found, set ‘$TERMINAL’!'
        exit 1
      '';
      daedalusPrefix = "\${HOME}/.daedalus";
      mkNixChrootWrapper = wrapperName: storePath: pkgs.writeScriptBin wrapperName ''
        #!/bin/sh

        export XDG_DATA_HOME=''${XDG_DATA_HOME:-''${HOME}/.local/share}
        export NAMESPACE_HELPER_RUN_ARGV=1

        # namespaceHelper outputs a bunch of debug messages, let’s hide them with file descriptor redirections
        # also, passing $PWD is a bit awkward
        exec 3>&1 4>&2 1>/dev/null 2>/dev/null ${launcherConfig.stateDir}/namespaceHelper ${pkgs.writeScript "${wrapperName}-wrapper" ''
          #! ${pkgs.stdenv.shell}
          cd "$1" ; shift
          ${linuxExports}
          exec 1>&3 2>&4 ${storePath} "$@"
        ''} "$PWD" "$@"
      '';
      addNixChrootWrapper = wrapperName: storePath: ''
        export PATH="${daedalusPrefix}${mkNixChrootWrapper wrapperName storePath}/bin:$PATH"
      '';
    in pkgs.writeScriptBin "open-marlowe-term" ''
      #! ${pkgs.stdenv.shell}

      if [ -e /escape-hatch ] ; then
        # We’re inside the installed nix-user-chroot. There are no
        # terminal emulators in here, so we have to escape. The commands
        # will be accessible from the terminal emulator running *outside*
        # of our nix-user-chroot – but each will be a thin wrapper that will
        # enter the chroot just for this particular invocation.

        echo >/escape-hatch ${daedalusPrefix}${pkgs.writeScript "open-marlowe-term-escaped" ''
          #!/bin/sh
          ${addNixChrootWrapper "marlowe-cli"     "${marlowe-cli}/bin/marlowe-cli"}
          ${addNixChrootWrapper "cardano-cli"     "${daedalus-bridge}/bin/cardano-cli"}
          ${addNixChrootWrapper "cardano-wallet"  "${daedalus-bridge}/bin/cardano-wallet"}
          ${addNixChrootWrapper "cardano-address" "${daedalus-bridge}/bin/cardano-address"}
          ${addNixChrootWrapper "jq"              "${pkgs.jq}/bin/jq"}
          ${addNixChrootWrapper "basenc"          "${pkgs.coreutils}/bin/basenc"}
          ${linuxExports}
          cd $HOME
          ${runTerminalEmulator "$HOME/.daedalus${welcomeScript}"}
        ''}
      else
        # We’re inside nix-shell or on NixOS. Let’s set environment here to be inherited,
        # in case an exotic $TERMINAL doesn’t run our welcome script.

        export PATH=${marlowe-cli}/bin:${pkgs.jq}/bin:$PATH
        ${linuxExports}
        cd $HOME
        ${runTerminalEmulator welcomeScript}
      fi
    '';

  };

}

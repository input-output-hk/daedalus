{inputs, ...}: {
  perSystem = {
    pkgs,
    lib,
    system,
    config,
    common,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      clusters = common.sourceLib.installerClusters;

      # cloud-init user-data: __SSH_KEY__ is substituted at script runtime.
      # The Daedalus .desktop autostart path matches the native package install root.
      mkUserData = {
        cluster,
        packages, # list of distro package names
        installCmd, # shell command for first install
        reinstallCmd, # shell command for upgrade/reinstall on subsequent boots
      }:
        pkgs.writeText "user-data-${cluster}" ''
          #cloud-config
          hostname: daedalus-vm
          manage_etc_hosts: true
          users:
            - name: tester
              groups: wheel
              sudo: ALL=(ALL) NOPASSWD:ALL
              shell: /bin/bash
              lock_passwd: false
              plain_text_passwd: tester
              ssh_authorized_keys:
                - __SSH_KEY__
          packages:
          ${lib.concatMapStrings (p: "  - ${p}\n") packages}
          write_files:
            - path: /etc/lightdm/lightdm.conf.d/50-autologin.conf
              content: |
                [Seat:*]
                autologin-user=tester
                autologin-session=xfce
            - path: /etc/xdg/autostart/daedalus.desktop
              content: |
                [Desktop Entry]
                Type=Application
                Name=Daedalus (${cluster})
                Exec=env LIBGL_ALWAYS_SOFTWARE=1 /opt/daedalus/${cluster}/bin/daedalus
                X-GNOME-Autostart-enabled=true
            - path: /usr/local/bin/daedalus-reinstall
              permissions: '0755'
              content: |
                #!/bin/bash
                set -euo pipefail
                mount /mnt/daedalus-pkg 2>/dev/null || true
                ${reinstallCmd}
            - path: /etc/systemd/system/daedalus-reinstall.service
              content: |
                [Unit]
                Description=Reinstall Daedalus from host package share
                After=local-fs.target
                Before=lightdm.service display-manager.service

                [Service]
                Type=oneshot
                ExecStart=/usr/local/bin/daedalus-reinstall
                RemainAfterExit=yes

                [Install]
                WantedBy=graphical.target
          runcmd:
            - groupmod -g 62000 ubuntu 2>/dev/null || true
            - groupmod -g 62001 wheel 2>/dev/null || true
            - groupmod -g 1000 tester 2>/dev/null || true
            - usermod -g tester tester 2>/dev/null || true
            - modprobe 9p 9pnet 9pnet_virtio
            - mkdir -p /mnt/daedalus-pkg
            - mount -t 9p -o trans=virtio,version=9p2000.L,ro daedalus-pkg /mnt/daedalus-pkg
            - ${installCmd}
            - mkdir -p /home/tester/.local/share/Daedalus/${cluster}
            - chown -R tester:tester /home/tester/.local/share /home/tester/.config
            - printf 'daedalus-pkg\t/mnt/daedalus-pkg\t9p\ttrans=virtio,version=9p2000.L,ro,_netdev\t0\t0\n' >> /etc/fstab
            - printf 'daedalus-state\t/home/tester/.local/share/Daedalus/${cluster}\tvirtiofs\tdefaults,_netdev\t0\t0\n' >> /etc/fstab
            - mount /home/tester/.local/share/Daedalus/${cluster} || true
            - mount --make-rslave /
            - systemctl enable daedalus-reinstall.service
            - systemctl set-default graphical.target
            - systemctl enable lightdm
            - systemctl start lightdm || true
        '';

      debUserData = cluster:
        mkUserData {
          inherit cluster;
          packages = ["xfce4" "xfce4-goodies" "thunar" "chromium-browser" "spice-vdagent" "lightdm" "lightdm-gtk-greeter" "libgl1"];
          installCmd = "dpkg -i /mnt/daedalus-pkg/*.deb || apt-get install -f -y";
          reinstallCmd = "dpkg -r daedalus-${cluster} 2>/dev/null || true; dpkg -i /mnt/daedalus-pkg/*.deb || apt-get install -f -y";
        };

      rpmUserData = cluster:
        mkUserData {
          inherit cluster;
          packages = ["xfce4-session" "xfce4-panel" "xfwm4" "xfce4-terminal" "thunar" "chromium" "spice-vdagentd" "lightdm" "lightdm-gtk-greeter" "mesa-libGL"];
          installCmd = "rpm -i /mnt/daedalus-pkg/*.rpm || dnf install -f -y";
          reinstallCmd = "rpm -e daedalus-${cluster} 2>/dev/null || true; rpm -i /mnt/daedalus-pkg/*.rpm || dnf install -f -y";
        };

      archUserData = cluster:
        mkUserData {
          inherit cluster;
          packages = ["xfce4" "xfce4-goodies" "thunar" "chromium" "spice-vdagent" "lightdm" "lightdm-gtk-greeter" "mesa" "libglvnd"];
          installCmd = "pacman -U --noconfirm /mnt/daedalus-pkg/*.pkg.tar.zst";
          reinstallCmd = "pacman -U --noconfirm /mnt/daedalus-pkg/*.pkg.tar.zst";
        };

      mkTestVm = {
        distro, # "deb", "rpm", "arch"
        cluster,
        imageUrl,
        imageFormat, # "qcow2" (or "raw" for legacy images)
        vncPort,
        sshPort,
        userDataFile,
        packageDistro ? distro, # override to use a different package variant (e.g. "arch-dev")
        useEfi ? false, # UEFI boot via OVMF (required for Ubuntu Noble and later)
      }: let
        vmName = "daedalus-${distro}-${cluster}";
        package = config.packages."${packageDistro}-installer-${cluster}";
      in
        pkgs.writeShellApplication {
          name = "test-vm-${distro}-${cluster}";
          runtimeInputs = with pkgs; [curl qemu virtiofsd cloud-utils gnused coreutils];
          text = ''
            VM_NAME="${vmName}"
            IMAGE_CACHE="''${XDG_CACHE_HOME:-$HOME/.cache}/daedalus-test-vms/$(basename "${imageUrl}")"
            VM_DISK="''${XDG_CACHE_HOME:-$HOME/.cache}/daedalus-test-vms/${vmName}.qcow2"
            PACKAGE_PATH="${package}"
            VNC_PORT="''${VM_VNC_PORT:-${toString vncPort}}"
            SSH_PORT="''${VM_SSH_PORT:-${toString sshPort}}"
            VM_DAEDALUS_DIR="''${VM_DAEDALUS_DIR:-''${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus/${cluster}}"
            VNC_DISPLAY=$((VNC_PORT - 5900))

            mkdir -p "$(dirname "$IMAGE_CACHE")"
            mkdir -p "$VM_DAEDALUS_DIR"

            # Find SSH public key for passwordless access
            SSH_KEY=""
            for f in "$HOME/.ssh/id_ed25519.pub" "$HOME/.ssh/id_rsa.pub" "$HOME/.ssh/id_ecdsa.pub"; do
              if [ -f "$f" ]; then SSH_KEY=$(cat "$f"); break; fi
            done
            if [ -z "$SSH_KEY" ]; then
              echo "Warning: no SSH public key found; SSH will require password 'tester'"
            fi

            # Download cloud image once, reuse across clusters
            if [ ! -f "$IMAGE_CACHE" ]; then
              echo "Downloading cloud image (cached at $IMAGE_CACHE)..."
              curl -L --progress-bar -o "$IMAGE_CACHE.tmp" "${imageUrl}"
              mv "$IMAGE_CACHE.tmp" "$IMAGE_CACHE"
            fi

            WORKDIR=$(mktemp -d)

            cleanup() {
              echo ""
              echo "Stopping $VM_NAME..."
              kill "''${QEMU_PID:-0}" 2>/dev/null || true
              wait "''${QEMU_PID:-0}" 2>/dev/null || true
              kill "''${VIRTIOFSD_PID:-0}" 2>/dev/null || true
              wait "''${VIRTIOFSD_PID:-0}" 2>/dev/null || true
              if [ -s "$WORKDIR/qemu-errors.log" ]; then
                echo "QEMU errors:"; cat "$WORKDIR/qemu-errors.log"
              fi
              if [ -s "$WORKDIR/virtiofsd.log" ]; then
                echo "virtiofsd errors:"; cat "$WORKDIR/virtiofsd.log"
              fi
              rm -rf "$WORKDIR"
              echo "VM disk preserved at $VM_DISK (delete it manually for a clean slate)."
            }
            trap cleanup EXIT

            # On first run create the overlay disk; on subsequent runs reuse it so
            # boot skips cloud-init. The daedalus-reinstall service handles package
            # updates on every boot. Delete the qcow2 manually to get a clean slate.
            if [ ! -f "$VM_DISK" ]; then
              echo "Creating VM disk for $VM_NAME (first run)..."
              qemu-img create -f qcow2 -b "$IMAGE_CACHE" -F "${imageFormat}" "$VM_DISK" 20G
            else
              echo "Reusing existing VM disk for $VM_NAME (daedalus-reinstall will update the package)..."
            fi

            sed "s|__SSH_KEY__|$SSH_KEY|g" "${userDataFile}" > "$WORKDIR/user-data"
            cloud-localds "$WORKDIR/cloud-init.iso" "$WORKDIR/user-data"

            # virtiofsd for the Daedalus state dir: avoids 9P SQLite locking issues.
            # --sandbox=none: the default --sandbox=namespace restricts the virtiofsd
            # user namespace to a single UID/GID mapping (1000→host); any mkdir that
            # virtiofsd then tries to chown to an unmapped UID returns EINVAL, which
            # surfaces as mithril's "failed to create staging directory" warning.
            # With --sandbox=none virtiofsd runs as the host user directly and all
            # filesystem ops work. The UID translation below keeps ownership aligned.
            virtiofsd \
              --socket-path="$WORKDIR/virtiofsd.sock" \
              --shared-dir="$VM_DAEDALUS_DIR" \
              --cache=auto \
              --sandbox=none \
              --translate-uid "map:1000:$(id -u):1" \
              --translate-gid "map:1000:$(id -g):1" \
              2>"$WORKDIR/virtiofsd.log" &
            VIRTIOFSD_PID=$!
            # Wait for the socket to appear before handing it to QEMU.
            for _i in $(seq 1 50); do
              [ -S "$WORKDIR/virtiofsd.sock" ] && break
              sleep 0.1
            done

            # Run QEMU directly: no libvirt, no bridge, user-mode NAT via SLIRP.
            # Ubuntu Noble (24.04+) cloud images are UEFI-only (GPT, no hybrid MBR),
            # so those VMs use q35 + OVMF. Fedora and Arch images support SeaBIOS/pc.
            # memory-backend-memfd + -numa are required for vhost-user-fs (virtiofsd);
            # the memfd size must match -m exactly.
            ${lib.optionalString useEfi ''
              cp "${pkgs.OVMF.fd}/FV/OVMF_VARS.fd" "$WORKDIR/OVMF_VARS.fd"
              chmod 644 "$WORKDIR/OVMF_VARS.fd"
              efi_flags="-drive if=pflash,format=raw,readonly=on,unit=0,file=${pkgs.OVMF.fd}/FV/OVMF_CODE.fd -drive if=pflash,format=raw,unit=1,file=$WORKDIR/OVMF_VARS.fd"
            ''}
            # shellcheck disable=SC2086
            qemu-system-x86_64 \
              -enable-kvm \
              -cpu host \
              -machine ${
              if useEfi
              then "q35"
              else "pc"
            } \
              -m 8192 \
              -object "memory-backend-memfd,id=mem,size=8192M,share=on" \
              -numa "node,memdev=mem" \
              -smp 4 \
              ''${efi_flags:-} \
              -drive "if=ide,file=$VM_DISK,format=qcow2,cache=writeback" \
              -drive "file=$WORKDIR/cloud-init.iso,media=cdrom,readonly=on" \
              -netdev "user,id=net0,hostfwd=tcp:127.0.0.1:''${SSH_PORT}-:22" \
              -device "virtio-net-pci,netdev=net0" \
              -virtfs "local,path=$PACKAGE_PATH,mount_tag=daedalus-pkg,security_model=mapped-xattr,readonly=on" \
              -chardev "socket,id=char0,path=$WORKDIR/virtiofsd.sock" \
              -device "vhost-user-fs-pci,chardev=char0,tag=daedalus-state" \
              -vnc "127.0.0.1:''${VNC_DISPLAY}" \
              -serial "file:$WORKDIR/console.log" \
              2>"$WORKDIR/qemu-errors.log" &
            QEMU_PID=$!

            echo ""
            echo "Cloud-init is installing desktop + Daedalus (~2-5 min)."
            echo "Watch console: tail -f $WORKDIR/console.log"

            echo ""
            echo "════════════════════════════════════════════════════════"
            echo "  VM:        $VM_NAME"
            echo "  VNC:       remote-viewer vnc://127.0.0.1:$VNC_DISPLAY  (display :$VNC_DISPLAY = port $VNC_PORT)"
            echo "  SSH:       ssh -p ''${SSH_PORT} tester@127.0.0.1  (pw: tester)"
            echo "  Console:   tail -f $WORKDIR/console.log"
            echo "  State dir: $VM_DAEDALUS_DIR  (mounted into VM)"
            echo "  Ctrl-C to stop and remove this VM."
            echo "════════════════════════════════════════════════════════"

            wait "$QEMU_PID"
          '';
        };

      # ── Evidence collection ───────────────────────────────────────────────────
      # collect-evidence-<distro>-<cluster> apps run the linux-chromium-sandbox-probe
      # against a *running* test VM and write the output to the evidence directory.
      #
      # Usage (two-terminal workflow):
      #   Terminal 1:  nix run .#test-vm-deb-preprod      # boot VM, leave running
      #   Terminal 2:  nix run .#collect-evidence-deb-preprod   # collect + write files
      #
      # The script waits for SSH, copies the probe into the guest, runs it, and
      # writes the resulting JSON to scripts/linux-chromium-sandbox-probe/evidence/.
      #
      # Check mode (--check flag): fails with a diff if evidence would change; use
      # this locally before committing to verify the files are up to date.
      #
      # The sandbox-evidence CI check validates the committed evidence files
      # statically (no VM needed) — it checks that every index.json references
      # the matrixRevision declared in main.cjs and that all listed files exist.
      mkCollectEvidence = {
        distro,
        cluster,
        sshPort,
        matrixRow,
        matrixRevision,
        sandboxClass,
        # Absolute path to the electron binary inside the guest OS.
        electronPath,
        # Evidence subdirectory name (e.g. "task-112").
        taskDir,
      }: let
        probeScript = ../scripts/linux-chromium-sandbox-probe/main.cjs;
        evidenceDir = ../scripts/linux-chromium-sandbox-probe/evidence;
      in
        pkgs.writeShellApplication {
          name = "collect-evidence-${distro}-${cluster}";
          runtimeInputs = with pkgs; [openssh gnused jq git];
          text = ''
            CHECK_MODE=0
            for arg in "$@"; do [ "$arg" = "--check" ] && CHECK_MODE=1; done

            SSH_PORT="${toString sshPort}"
            SSH_OPTS=(-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR)
            SSH_TARGET="tester@127.0.0.1"
            EVIDENCE_DIR="${toString evidenceDir}/${taskDir}"

            # Wait for SSH to be ready (cloud-init + package install can take ~5 min).
            echo "Waiting for VM SSH on port $SSH_PORT (up to 10 min)..."
            for i in $(seq 1 120); do
              if ssh "''${SSH_OPTS[@]}" -p "$SSH_PORT" -o ConnectTimeout=3 "$SSH_TARGET" true 2>/dev/null; then
                echo "SSH ready."
                break
              fi
              [ "$i" -eq 120 ] && { echo "ERROR: VM did not become reachable after 10 min."; exit 1; }
              sleep 5
            done

            # Wait for the package to be installed (cloud-init runcmd writes a sentinel).
            echo "Waiting for package install to finish..."
            for i in $(seq 1 60); do
              if ssh "''${SSH_OPTS[@]}" -p "$SSH_PORT" "$SSH_TARGET" "[ -x '${electronPath}' ]" 2>/dev/null; then
                echo "Package installed."
                break
              fi
              [ "$i" -eq 60 ] && { echo "ERROR: electron binary not found after 5 min."; exit 1; }
              sleep 5
            done

            # Copy probe script into guest.
            scp "''${SSH_OPTS[@]}" -P "$SSH_PORT" "${toString probeScript}" "$SSH_TARGET:/tmp/daedalus-probe.cjs"

            # Run probe, capturing stdout.
            # GBM_BACKENDS_PATH must point to the bundled libgbm — without it
            # Electron loads the system mesa snap's GBM backends which SIGILL.
            EVIDENCE=$(ssh "''${SSH_OPTS[@]}" -p "$SSH_PORT" "$SSH_TARGET" \
              "DISPLAY=:0.0 \
               XAUTHORITY=/home/tester/.Xauthority \
               LIBGL_ALWAYS_SOFTWARE=1 \
               GBM_BACKENDS_PATH='/opt/daedalus/${cluster}/libexec/bundle-electron/lib/electron/lib' \
               CHROME_DEVEL_SANDBOX='/opt/daedalus/${cluster}/libexec/bundle-electron/lib/electron/chrome-sandbox' \
               LAUNCHER_CONFIG='/opt/daedalus/${cluster}/config/daedalus-config.json' \
               DAEDALUS_PROBE_MATRIX_ROW='${matrixRow}' \
               DAEDALUS_PROBE_MATRIX_REVISION='${matrixRevision}' \
               DAEDALUS_PROBE_SANDBOX_CLASS='${sandboxClass}' \
               DAEDALUS_PROBE_CLUSTER='${cluster}' \
               '${electronPath}' /tmp/daedalus-probe.cjs 2>/dev/null") || true

            RESULT=$(printf '%s' "$EVIDENCE" | jq -r '.result // "unknown"' 2>/dev/null || echo "unknown")
            echo "Probe result: $RESULT"
            if [ "$RESULT" != "pass" ]; then
              echo "ERROR: probe did not pass."
              printf '%s\n' "$EVIDENCE" | jq .
              exit 1
            fi

            OUT_FILE="$EVIDENCE_DIR/${matrixRow}-positive.json"
            mkdir -p "$EVIDENCE_DIR"

            if [ "$CHECK_MODE" = "1" ]; then
              CURRENT=$(cat "$OUT_FILE" 2>/dev/null || echo "")
              NEW=$(printf '%s\n' "$EVIDENCE" | jq --sort-keys .)
              CURRENT_SORTED=$(printf '%s\n' "$CURRENT" | jq --sort-keys . 2>/dev/null || echo "")
              if [ "$CURRENT_SORTED" != "$NEW" ]; then
                echo "ERROR: evidence is stale. Run without --check to update:"
                echo "  nix run .#collect-evidence-${distro}-${cluster}"
                diff <(printf '%s\n' "$CURRENT") <(printf '%s\n' "$EVIDENCE") || true
                exit 1
              fi
              echo "Evidence is up to date."
            else
              printf '%s\n' "$EVIDENCE" | jq . > "$OUT_FILE"
              echo "Written: $OUT_FILE"
              # Regenerate index.json listing all evidence files in the task dir.
              mapfile -t EVIDENCE_FILES < <(find "$EVIDENCE_DIR" -maxdepth 1 -name '*.json' ! -name 'index.json' -printf '%f\n' | sort)
              DATE=$(date +%Y-%m-%d)
              jq -n \
                --arg date "$DATE" \
                --arg matrixRevision "${matrixRevision}" \
                --arg task "${taskDir}" \
                --argjson files "$(printf '%s\n' "''${EVIDENCE_FILES[@]}" | jq -R . | jq -s .)" \
                '{date:$date,matrixRevision:$matrixRevision,task:$task,evidence:$files}' \
                > "$EVIDENCE_DIR/index.json"
              echo "Updated: $EVIDENCE_DIR/index.json"
            fi
          '';
        };

      mkCollectEvidenceForDistro = {
        distro,
        sshBase,
        matrixRows, # list of {cluster, matrixRow, sandboxClass}
        matrixRevision,
        electronPathFn, # cluster -> guest electron path
        taskDir,
      }:
        lib.imap0 (i: entry: {
          name = "collect-evidence-${distro}-${entry.cluster}";
          value = {
            type = "app";
            program = lib.getExe (mkCollectEvidence {
              inherit distro matrixRevision taskDir;
              cluster = entry.cluster;
              sshPort = sshBase + i;
              matrixRow = entry.matrixRow;
              sandboxClass = entry.sandboxClass;
              electronPath = electronPathFn entry.cluster;
            });
          };
        })
        matrixRows;

      # Port assignments (i = cluster index: mainnet=0, preprod=1, preview=2)
      #   VNC:  deb 5930+i, rpm 5940+i, arch 5950+i
      #   SSH:  deb 2230+i, rpm 2240+i, arch 2250+i
      mkVmsForDistro = {
        distro,
        imageUrl,
        imageFormat,
        vncBase,
        sshBase,
        userDataFn,
        packageDistro ? distro,
        useEfi ? false,
      }:
        lib.imap0 (i: cluster: {
          name = "test-vm-${distro}-${cluster}";
          value = {
            type = "app";
            program = lib.getExe (mkTestVm {
              inherit distro cluster imageUrl imageFormat packageDistro useEfi;
              vncPort = vncBase + i;
              sshPort = sshBase + i;
              userDataFile = userDataFn cluster;
            });
          };
        })
        clusters;
      # Evidence task tag for this PR's builds (watchdog-as-entrypoint + linux pkgs).
      # Bump when the process tree or sandbox policy changes; re-collect evidence.
      evidenceTask = "task-112";
      # deb and rpm rows use the upstream MATRIX_REVISION (sandbox policy unchanged).
      debRpmMatrixRevision = "task-108-matrix-2026-08-18";
      archMatrixRevision = "task-111-matrix-2026-09-02";
      nixosMatrixRevision = "task-112-matrix-2026-09-11";

      debElectronPath = cluster: "/opt/daedalus/${cluster}/libexec/bundle-electron/lib/electron/electron";
      rpmElectronPath = cluster: "/opt/daedalus/${cluster}/libexec/bundle-electron/lib/electron/electron";
      archElectronPath = cluster: "/opt/daedalus/${cluster}/libexec/bundle-electron/lib/electron/electron";
    in {
      apps = lib.listToAttrs (
        # ── Test VMs (dev convenience tools — NOT sandbox certification) ──────────
        #
        # These apps exist so a developer can interactively boot a guest OS, poke
        # around, and smoke-test a package build.  They are explicitly NOT part of
        # sandbox certification:
        #
        #  • Image URLs are intentionally mutable (ubuntu .../current/..., arch
        #    .../latest/...).  No SHA-256 is pinned and the guest identity is not
        #    verified.  Rotating images will silently change what is tested, which
        #    is acceptable for ad-hoc dev use but not for a reproducible audit.
        #
        #  • There is no guest pass/fail channel: QEMU exits 0 when the guest
        #    powers off regardless of what happened inside the VM.
        #
        # Sandbox certification uses a separate, evidence-based workflow:
        #   1. Boot a test VM:   nix run .#test-vm-<distro>-<cluster>
        #   2. Collect evidence: nix run .#collect-evidence-<distro>-<cluster>
        #   3. Commit the JSON output under scripts/linux-chromium-sandbox-probe/evidence/
        #   4. CI enforces consistency via the sandbox-evidence check in checks.nix,
        #      which validates every committed file against the matrix revisions
        #      declared in main.cjs without requiring a VM.
        mkVmsForDistro {
          distro = "deb";
          packageDistro = "deb-dev";
          imageUrl = "https://cloud-images.ubuntu.com/noble/current/noble-server-cloudimg-amd64.img";
          imageFormat = "qcow2";
          vncBase = 5930;
          sshBase = 2230;
          userDataFn = debUserData;
          useEfi = true;
        }
        ++ mkVmsForDistro {
          distro = "rpm";
          packageDistro = "rpm-dev";
          imageUrl = "https://dl.fedoraproject.org/pub/fedora/linux/releases/43/Cloud/x86_64/images/Fedora-Cloud-Base-Generic-43-1.6.x86_64.qcow2";
          imageFormat = "qcow2";
          vncBase = 5940;
          sshBase = 2240;
          userDataFn = rpmUserData;
        }
        ++ mkVmsForDistro {
          distro = "arch";
          packageDistro = "arch-dev";
          imageUrl = "https://geo.mirror.pkgbuild.com/images/latest/Arch-Linux-x86_64-cloudimg.qcow2";
          imageFormat = "qcow2";
          vncBase = 5950;
          sshBase = 2250;
          userDataFn = archUserData;
        }
        # ── Evidence collection (run against a live test VM) ─────────────────────
        # Usage: nix run .#collect-evidence-<distro>-<cluster>
        # Collects sandbox canary evidence and writes to
        # scripts/linux-chromium-sandbox-probe/evidence/<task>/.
        # Add --check to fail if evidence would change (verify before committing).
        ++ mkCollectEvidenceForDistro {
          distro = "deb";
          sshBase = 2230;
          matrixRevision = debRpmMatrixRevision;
          taskDir = evidenceTask;
          electronPathFn = debElectronPath;
          matrixRows =
            lib.imap0 (i: cluster: {
              inherit cluster;
              matrixRow = "ubuntu-24.04";
              sandboxClass = "suid-only";
            })
            clusters;
        }
        ++ mkCollectEvidenceForDistro {
          distro = "rpm";
          sshBase = 2240;
          matrixRevision = debRpmMatrixRevision;
          taskDir = evidenceTask;
          electronPathFn = rpmElectronPath;
          matrixRows =
            lib.imap0 (i: cluster: {
              inherit cluster;
              matrixRow = "fedora-43";
              sandboxClass = "suid-only";
            })
            clusters;
        }
        ++ mkCollectEvidenceForDistro {
          distro = "arch";
          sshBase = 2250;
          matrixRevision = archMatrixRevision;
          taskDir = evidenceTask;
          electronPathFn = archElectronPath;
          matrixRows =
            lib.imap0 (i: cluster: {
              inherit cluster;
              matrixRow = "arch-2026.09.01";
              sandboxClass = "userns-only";
            })
            clusters;
        }
      );
    });
}

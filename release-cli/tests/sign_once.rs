use std::os::unix::fs::PermissionsExt;
use std::process::Command;

#[test]
fn sign_invokes_gpg_once_per_installer_including_native_linux_packages() {
    let directory = tempfile::tempdir().unwrap();
    let bin = directory.path().join("bin");
    std::fs::create_dir(&bin).unwrap();
    let gpg = bin.join("gpg2");
    // Fail on a repeated invocation, as GPG would when a signature already
    // exists and a noninteractive caller cannot approve overwriting it.
    let shell = std::env::split_paths(&std::env::var_os("PATH").unwrap())
        .map(|directory| directory.join("sh"))
        .find(|path| path.is_file())
        .unwrap();
    std::fs::write(&gpg, format!("#!{}\nfor file do :; done\ntest ! -e \"$file.asc\" || exit 1\nprintf signature > \"$file.asc\"\nprintf '%s\\n' \"$file\" >> \"$SIGN_LOG\"\n", shell.display())).unwrap();
    std::fs::set_permissions(&gpg, std::fs::Permissions::from_mode(0o755)).unwrap();
    std::fs::write(directory.path().join("version"), "11.6.0").unwrap();
    let names = [
        "fixture.bin",
        "fixture.deb",
        "fixture.rpm",
        "fixture.pkg.tar.zst",
    ];
    for name in names {
        std::fs::write(directory.path().join(name), b"installer").unwrap();
    }
    let log = directory.path().join("gpg.log");
    let path = std::env::join_paths(
        std::iter::once(bin).chain(std::env::split_paths(&std::env::var_os("PATH").unwrap())),
    )
    .unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_drt"))
        .arg("sign")
        .arg(directory.path())
        .arg("--skip-code-sign")
        .env("PATH", path)
        .env("SIGN_LOG", &log)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        std::fs::read_to_string(log).unwrap().lines().count(),
        names.len()
    );
    for name in names {
        assert!(directory.path().join(format!("{name}.asc")).exists());
    }
}

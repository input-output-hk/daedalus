use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use tokio::time::{Duration, timeout};

const DISK_SPACE_PROBE_TIMEOUT: Duration = Duration::from_secs(5);

pub struct ChainValidationResult {
    /// Whether the path is usable as a chain-storage location.
    pub is_valid: bool,
    /// Path to store in settings. `None` means reset to the managed default.
    pub path: Option<PathBuf>,
    pub resolved_path: Option<PathBuf>,
    pub reason: Option<&'static str>,
    pub available_space_bytes: Option<u64>,
    pub required_space_bytes: u64,
}

fn is_loop_error(e: &std::io::Error) -> bool {
    #[cfg(unix)]
    {
        e.raw_os_error() == Some(libc::ELOOP)
    }
    #[cfg(windows)]
    {
        e.raw_os_error() == Some(1772) // ERROR_SYMLINK_LOOP
    }
    #[cfg(not(any(unix, windows)))]
    {
        false
    }
}

fn is_path_not_found_error(e: &std::io::Error) -> bool {
    matches!(e.kind(), ErrorKind::NotFound | ErrorKind::NotADirectory) || is_loop_error(e)
}

/// Returns true when `child` is equal to or nested under `parent`.
///
/// Uses component-wise comparison on POSIX and case-insensitive comparison on
/// Windows, matching the `isSubPath` semantics from the TypeScript chain-storage
/// subsystem this replaces.
pub fn is_sub_path(parent: &Path, child: &Path) -> bool {
    #[cfg(windows)]
    {
        let p = parent.to_string_lossy().to_lowercase();
        let c = child.to_string_lossy().to_lowercase();
        let sep = std::path::MAIN_SEPARATOR;
        let p_with_sep = format!("{}{}", p.trim_end_matches(sep), sep);
        c == p || c.starts_with(&p_with_sep)
    }
    #[cfg(not(windows))]
    {
        child == parent || child.starts_with(parent)
    }
}

/// Returns true when `a` and `b` refer to the same filesystem path.
///
/// Case-insensitive on Windows.
pub fn is_same_path(a: &Path, b: &Path) -> bool {
    #[cfg(windows)]
    {
        a.to_string_lossy().to_lowercase() == b.to_string_lossy().to_lowercase()
    }
    #[cfg(not(windows))]
    {
        a == b
    }
}

#[cfg(unix)]
fn free_space_bytes_sync(path: &Path) -> std::io::Result<u64> {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;

    let c_path = CString::new(path.as_os_str().as_bytes())
        .map_err(|e| std::io::Error::new(ErrorKind::InvalidInput, e))?;
    let mut stat = std::mem::MaybeUninit::<libc::statvfs>::uninit();
    if unsafe { libc::statvfs(c_path.as_ptr(), stat.as_mut_ptr()) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    let stat = unsafe { stat.assume_init() };
    Ok(stat.f_bavail as u64 * stat.f_frsize as u64)
}

#[cfg(windows)]
fn free_space_bytes_sync(path: &Path) -> std::io::Result<u64> {
    use std::os::windows::ffi::OsStrExt;
    use windows_sys::Win32::Storage::FileSystem::GetDiskFreeSpaceExW;

    let mut wide: Vec<u16> = path.as_os_str().encode_wide().collect();
    wide.push(0);
    let mut free: u64 = 0;
    if unsafe {
        GetDiskFreeSpaceExW(
            wide.as_ptr(),
            &mut free,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        )
    } == 0
    {
        return Err(std::io::Error::last_os_error());
    }
    Ok(free)
}

#[cfg(not(any(unix, windows)))]
fn free_space_bytes_sync(_path: &Path) -> std::io::Result<u64> {
    Err(std::io::Error::new(
        ErrorKind::Unsupported,
        "disk space check not supported on this platform",
    ))
}

/// Reads available bytes at `path`. Returns `None` on timeout or any error.
///
/// The probe is bounded so a hung or slow call on Windows (which spawns a
/// subprocess) cannot block validation indefinitely.
async fn read_free_space(path: PathBuf) -> Option<u64> {
    timeout(
        DISK_SPACE_PROBE_TIMEOUT,
        tokio::task::spawn_blocking(move || free_space_bytes_sync(&path)),
    )
    .await
    .ok()
    .and_then(|r| r.ok())
    .and_then(|r| r.ok())
}

/// Validates `target` as a chain-storage directory.
///
/// Checks existence, symlink resolution (including loops and dangling links),
/// state-directory nesting, writability, and available disk space. When the
/// selected path resolves to the managed chain directory the result carries
/// `path: None` to signal a reset to the default location.
pub async fn validate_chain_storage_directory(
    target: &Path,
    state_dir: &Path,
    default_chain_path: &Path,
    required_space: u64,
) -> ChainValidationResult {
    let chain_path = state_dir.join("chain");

    // Selecting the managed chain directory directly → reset to default.
    if is_same_path(target, &chain_path) {
        return ChainValidationResult {
            is_valid: true,
            path: None,
            resolved_path: Some(default_chain_path.to_owned()),
            reason: None,
            available_space_bytes: None,
            required_space_bytes: required_space,
        };
    }

    let reject = |reason: &'static str, resolved: Option<PathBuf>| ChainValidationResult {
        is_valid: false,
        path: Some(target.to_owned()),
        resolved_path: resolved,
        reason: Some(reason),
        available_space_bytes: None,
        required_space_bytes: required_space,
    };

    // Existence check follows symlinks. On POSIX, dangling links → false.
    // ELOOP from try_exists propagates as Err, unwrap_or(false) → path-not-found.
    let exists = tokio::fs::try_exists(target).await.unwrap_or(false);
    if !exists {
        return reject("path-not-found", None);
    }

    // Resolve all symlinks. Catches dangling reparse points on Windows.
    let resolved = match tokio::fs::canonicalize(target).await {
        Ok(p) => p,
        Err(e) if is_path_not_found_error(&e) => return reject("path-not-found", None),
        Err(_) => return reject("unknown", None),
    };

    // Must be a directory.
    match tokio::fs::metadata(&resolved).await {
        Ok(m) if !m.is_dir() => return reject("not-writable", Some(resolved)),
        Err(_) => return reject("unknown", Some(resolved)),
        _ => {}
    }

    // Resolved to the managed chain directory → reset to default.
    let resolved_default = tokio::fs::canonicalize(default_chain_path)
        .await
        .unwrap_or_else(|_| default_chain_path.to_owned());
    if resolved == resolved_default || is_same_path(&resolved, default_chain_path) {
        return ChainValidationResult {
            is_valid: true,
            path: None,
            resolved_path: Some(resolved_default),
            reason: None,
            available_space_bytes: None,
            required_space_bytes: required_space,
        };
    }

    // Nesting check: reject any path whose resolved form is inside the state dir.
    let resolved_state = tokio::fs::canonicalize(state_dir)
        .await
        .unwrap_or_else(|_| state_dir.to_owned());
    if is_sub_path(&resolved_state, &resolved) {
        return reject("inside-state-dir", Some(resolved));
    }

    // Writability: attempt to create and immediately remove a probe file.
    let writable = {
        let probe = resolved.join(".watchdog-write-probe");
        let opened = tokio::task::spawn_blocking({
            let probe = probe.clone();
            move || {
                std::fs::OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(&probe)
                    .map(|_| ())
            }
        })
        .await
        .ok()
        .and_then(|r| r.ok())
        .is_some();
        let _ = tokio::fs::remove_file(&probe).await;
        opened
    };
    if !writable {
        return reject("not-writable", Some(resolved));
    }

    // Free-space probe: best-effort, bounded by DISK_SPACE_PROBE_TIMEOUT.
    // A timed-out or failed probe does not block validation.
    let free = read_free_space(resolved.clone()).await;
    if let Some(free_bytes) = free {
        if free_bytes < required_space {
            return ChainValidationResult {
                is_valid: false,
                path: Some(target.to_owned()),
                resolved_path: Some(resolved),
                reason: Some("insufficient-space"),
                available_space_bytes: Some(free_bytes),
                required_space_bytes: required_space,
            };
        }
    }

    ChainValidationResult {
        is_valid: true,
        path: Some(target.to_owned()),
        resolved_path: Some(resolved),
        reason: None,
        available_space_bytes: free,
        required_space_bytes: required_space,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    struct TempDir(PathBuf);

    impl TempDir {
        fn new(label: &str) -> Self {
            let path = std::env::temp_dir().join(format!(
                "watchdog-chain-val-{}-{}-{}",
                label,
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
            ));
            fs::create_dir_all(&path).unwrap();
            TempDir(path)
        }

        fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    // ── is_sub_path ────────────────────────────────────────────────────────────

    #[test]
    fn sub_path_child_is_nested() {
        assert!(is_sub_path(Path::new("/a/b"), Path::new("/a/b/c")));
    }

    #[test]
    fn sub_path_equal_counts_as_nested() {
        assert!(is_sub_path(Path::new("/a/b"), Path::new("/a/b")));
    }

    #[test]
    fn sub_path_respects_component_boundary() {
        // "/a/bc" is NOT under "/a/b" — must not match on string prefix alone.
        assert!(!is_sub_path(Path::new("/a/b"), Path::new("/a/bc")));
    }

    #[test]
    fn sub_path_sibling_is_not_nested() {
        assert!(!is_sub_path(Path::new("/a/b"), Path::new("/a/c")));
    }

    #[test]
    fn sub_path_parent_is_not_nested() {
        assert!(!is_sub_path(Path::new("/a/b"), Path::new("/a")));
    }

    // ── is_same_path ──────────────────────────────────────────────────────────

    #[test]
    fn same_path_equal_paths() {
        assert!(is_same_path(Path::new("/a/b"), Path::new("/a/b")));
    }

    #[test]
    fn same_path_different_paths() {
        assert!(!is_same_path(Path::new("/a/b"), Path::new("/a/c")));
    }

    // ── validate_chain_storage_directory ──────────────────────────────────────

    #[tokio::test]
    async fn accepts_a_plain_writable_directory() {
        let tmp = TempDir::new("plain");
        let state = tmp.path().join("state");
        let target = tmp.path().join("target");
        fs::create_dir_all(&state).unwrap();
        fs::create_dir_all(&target).unwrap();
        let default = state.join("chain");

        let result =
            validate_chain_storage_directory(&target, &state, &default, 0).await;
        assert!(result.is_valid);
        assert!(result.reason.is_none());
    }

    #[tokio::test]
    async fn rejects_nonexistent_path() {
        let tmp = TempDir::new("nonexist");
        let state = tmp.path().join("state");
        fs::create_dir_all(&state).unwrap();
        let target = tmp.path().join("does-not-exist");
        let default = state.join("chain");

        let result =
            validate_chain_storage_directory(&target, &state, &default, 0).await;
        assert!(!result.is_valid);
        assert_eq!(result.reason, Some("path-not-found"));
    }

    #[tokio::test]
    async fn resolves_a_symlink_to_its_real_target() {
        let tmp = TempDir::new("symlink");
        let state = tmp.path().join("state");
        let target = tmp.path().join("real-target");
        let link = tmp.path().join("link-to-target");
        fs::create_dir_all(&state).unwrap();
        fs::create_dir_all(&target).unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink(&target, &link).unwrap();
        #[cfg(windows)]
        std::os::windows::fs::symlink_dir(&target, &link).unwrap();
        let default = state.join("chain");

        let result =
            validate_chain_storage_directory(&link, &state, &default, 0).await;
        assert!(result.is_valid);
        assert_eq!(
            result.resolved_path.as_deref(),
            Some(fs::canonicalize(&target).unwrap().as_path())
        );
    }

    // Equivalent to: "resolves a chain of symlinks to the directory at the end of it"
    // (chainStorageValidation.realfs.spec.ts, added in PR #3378).
    #[cfg(unix)]
    #[tokio::test]
    async fn resolves_a_chain_of_symlinks_to_the_final_target() {
        let tmp = TempDir::new("symlink-chain");
        let state = tmp.path().join("state");
        let final_target = tmp.path().join("final-target");
        let intermediate = tmp.path().join("intermediate-link");
        let link = tmp.path().join("link-to-link");
        fs::create_dir_all(&state).unwrap();
        fs::create_dir_all(&final_target).unwrap();
        std::os::unix::fs::symlink(&final_target, &intermediate).unwrap();
        std::os::unix::fs::symlink(&intermediate, &link).unwrap();
        let default = state.join("chain");

        let result =
            validate_chain_storage_directory(&link, &state, &default, 0).await;
        assert!(result.is_valid);
        assert_eq!(
            result.resolved_path.as_deref(),
            Some(fs::canonicalize(&final_target).unwrap().as_path())
        );
    }

    // Equivalent to: "report an unresolvable link as not found, not unknown"
    // (chainStorageValidation.realfs.spec.ts, fixed in PR #3379).
    #[cfg(unix)]
    #[tokio::test]
    async fn rejects_a_dangling_symlink_as_path_not_found() {
        let tmp = TempDir::new("dangling");
        let state = tmp.path().join("state");
        let ghost = tmp.path().join("ghost");
        let link = tmp.path().join("dangling-link");
        fs::create_dir_all(&state).unwrap();
        std::os::unix::fs::symlink(&ghost, &link).unwrap();
        let default = state.join("chain");

        let result =
            validate_chain_storage_directory(&link, &state, &default, 0).await;
        assert!(!result.is_valid);
        assert_eq!(result.reason, Some("path-not-found"));
    }

    // Equivalent to: "rejects a directory that is part of a symlink loop"
    // (chainStorageValidation.realfs.spec.ts, added in PR #3378).
    // Precondition assertion confirms the loop is real before relying on the fallback.
    #[cfg(unix)]
    #[tokio::test]
    async fn rejects_a_symlink_loop_as_path_not_found() {
        let tmp = TempDir::new("loop");
        let state = tmp.path().join("state");
        let link = tmp.path().join("loop-entry");
        let partner = tmp.path().join("loop-partner");
        fs::create_dir_all(&state).unwrap();
        std::os::unix::fs::symlink(&partner, &link).unwrap();
        std::os::unix::fs::symlink(&link, &partner).unwrap();

        // Precondition: canonicalize must actually fail on the loop.
        assert!(fs::canonicalize(&link).is_err());

        let default = state.join("chain");
        let result =
            validate_chain_storage_directory(&link, &state, &default, 0).await;
        assert!(!result.is_valid);
        assert_eq!(result.reason, Some("path-not-found"));
    }

    // Equivalent to: "rejects the state directory itself"
    // (chainStorageValidation.realfs.spec.ts, added in PR #3378).
    #[tokio::test]
    async fn rejects_the_state_directory_itself() {
        let tmp = TempDir::new("state-itself");
        let state = tmp.path().join("state");
        fs::create_dir_all(&state).unwrap();
        let default = state.join("chain");

        let result =
            validate_chain_storage_directory(&state, &state, &default, 0).await;
        assert!(!result.is_valid);
        assert_eq!(result.reason, Some("inside-state-dir"));
    }

    // Equivalent to: "rejects a symlink that resolves to a location inside the state directory"
    // (chainStorageValidation.realfs.spec.ts, added in PR #3378).
    // A mock filesystem cannot express this: it decides both where the link sits
    // and where it resolves. Only a real filesystem shows that nesting is checked
    // on the resolved path rather than the literal path.
    #[cfg(unix)]
    #[tokio::test]
    async fn rejects_a_symlink_whose_target_is_inside_the_state_dir() {
        let tmp = TempDir::new("link-into-state");
        let state = tmp.path().join("state");
        let inside = state.join("nested-target");
        let link = tmp.path().join("link-into-state");
        fs::create_dir_all(&inside).unwrap();
        std::os::unix::fs::symlink(&inside, &link).unwrap();
        let default = state.join("chain");

        let result =
            validate_chain_storage_directory(&link, &state, &default, 0).await;
        assert!(!result.is_valid);
        assert_eq!(result.reason, Some("inside-state-dir"));
    }

    // Equivalent to: "treats the managed chain directory as a reset to the default location"
    // (chainStorageValidation.realfs.spec.ts, added in PR #3378).
    #[tokio::test]
    async fn treats_the_managed_chain_dir_as_a_reset_to_default() {
        let tmp = TempDir::new("chain-reset");
        let state = tmp.path().join("state");
        let chain = state.join("chain");
        fs::create_dir_all(&chain).unwrap();
        let default = fs::canonicalize(&chain).unwrap();

        let result =
            validate_chain_storage_directory(&chain, &state, &default, 0).await;
        assert!(result.is_valid);
        assert!(result.path.is_none(), "path should be None for a reset-to-default");
    }

    // Equivalent to: "treats a symlink that resolves to the managed chain directory as a reset"
    // (chainStorageValidation.realfs.spec.ts, added in PR #3378).
    #[cfg(unix)]
    #[tokio::test]
    async fn treats_a_symlink_to_the_chain_dir_as_a_reset() {
        let tmp = TempDir::new("chain-reset-via-symlink");
        let state = tmp.path().join("state");
        let chain = state.join("chain");
        let alias = tmp.path().join("alias-to-chain");
        fs::create_dir_all(&chain).unwrap();
        std::os::unix::fs::symlink(&chain, &alias).unwrap();
        let default = fs::canonicalize(&chain).unwrap();

        let result =
            validate_chain_storage_directory(&alias, &state, &default, 0).await;
        assert!(result.is_valid);
        assert!(result.path.is_none(), "path should be None for a reset-to-default");
    }

    #[tokio::test]
    async fn rejects_when_insufficient_space() {
        let tmp = TempDir::new("no-space");
        let state = tmp.path().join("state");
        let target = tmp.path().join("target");
        fs::create_dir_all(&state).unwrap();
        fs::create_dir_all(&target).unwrap();
        let default = state.join("chain");

        // Require more space than any disk can ever have.
        let result =
            validate_chain_storage_directory(&target, &state, &default, u64::MAX).await;
        // If the probe succeeded, we get insufficient-space; if it timed out we
        // get a valid result (probe timeout is not a rejection). Either is fine
        // here — we just assert we don't get an unexpected reason.
        if !result.is_valid {
            assert_eq!(result.reason, Some("insufficient-space"));
        }
    }

    // ── Windows-specific tests ────────────────────────────────────────────────
    // These gate on cfg(windows) and cover path shapes that have no POSIX
    // equivalent (PR #3381).

    #[cfg(windows)]
    mod windows_paths {
        use super::*;

        // CON, PRN, NUL and AUX are device names to the Win32 API, but fs reaches
        // the filesystem through libuv's extended-length form and can create them.
        // The resolved path must not carry the \\?\ prefix that libuv uses internally.
        #[tokio::test]
        async fn reserved_device_name_is_accepted_and_resolves_without_extended_prefix() {
            for name in ["CON", "PRN", "NUL", "AUX"] {
                let tmp = TempDir::new(&format!("reserved-{name}"));
                let state = tmp.path().join("state");
                let reserved = tmp.path().join(name);
                fs::create_dir_all(&state).unwrap();
                fs::create_dir(&reserved).unwrap();
                let default = state.join("chain");

                let result =
                    validate_chain_storage_directory(&reserved, &state, &default, 0).await;
                assert!(result.is_valid, "reserved name {name} should be accepted");
                if let Some(resolved) = &result.resolved_path {
                    let s = resolved.to_string_lossy();
                    assert!(
                        !s.starts_with(r"\\?\"),
                        "resolved path for {name} must not carry \\\\?\\ prefix: {s}"
                    );
                }
            }
        }

        // The Win32 API strips trailing dots and spaces from names; libuv does not,
        // so the directory on disk keeps the name exactly as given.
        #[tokio::test]
        async fn trailing_dot_and_space_names_are_accepted_without_extended_prefix() {
            for name in ["trailing-dot.", "trailing-space "] {
                let tmp = TempDir::new("trailing");
                let state = tmp.path().join("state");
                let spelled = tmp.path().join(name);
                fs::create_dir_all(&state).unwrap();
                fs::create_dir_all(&spelled).unwrap();
                let default = state.join("chain");

                let result =
                    validate_chain_storage_directory(&spelled, &state, &default, 0).await;
                assert!(result.is_valid, "name '{name}' should be accepted");
                if let Some(resolved) = &result.resolved_path {
                    let s = resolved.to_string_lossy();
                    assert!(
                        !s.starts_with(r"\\?\"),
                        "resolved path for '{name}' must not carry \\\\?\\ prefix: {s}"
                    );
                }
            }
        }

        // Whether a path beyond MAX_PATH can be created depends on the machine's
        // long-path support setting. Both outcomes are valid; what must hold in
        // either case is that validation agrees with the filesystem and never falls
        // through to the generic 'unknown' reason.
        #[tokio::test]
        async fn agrees_with_the_filesystem_for_paths_beyond_max_path() {
            const MAX_PATH: usize = 260;

            let tmp = TempDir::new("max-path");
            let state = tmp.path().join("state");
            fs::create_dir_all(&state).unwrap();

            let segment = "x".repeat(40);
            let segment_count =
                ((MAX_PATH - tmp.path().to_string_lossy().len()) / (segment.len() + 1)) + 2;
            let long_path: PathBuf = std::iter::repeat(segment.as_str())
                .take(segment_count)
                .fold(tmp.path().to_owned(), |acc, s| acc.join(s));
            assert!(long_path.to_string_lossy().len() > MAX_PATH);

            let _ = fs::create_dir_all(&long_path); // may fail if long-path support is off
            let exists = long_path.exists();
            let default = state.join("chain");

            // is_sub_path is pure string work and holds at any length.
            assert!(is_sub_path(tmp.path(), &long_path));

            let result =
                validate_chain_storage_directory(&long_path, &state, &default, 0).await;
            assert_eq!(
                result.is_valid, exists,
                "validation verdict must match filesystem existence"
            );
            assert_ne!(
                result.reason,
                Some("unknown"),
                "reason must not be generic 'unknown'"
            );
        }
    }
}

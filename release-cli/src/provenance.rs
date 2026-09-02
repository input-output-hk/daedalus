//! Provenance checks, run before anything is uploaded.
//!
//! Publishing a release asserts that a version corresponds to a commit. That
//! assertion is worth checking, because nothing else establishes it.
//!
//! Checking at tag time is not sufficient. A tag is reachable from its branch
//! when it is created and can stop being reachable later — a history rewrite
//! orphans one silently, and the artifacts are built afterwards. Comparing the
//! revision recorded in the artifacts against the commit the tag resolves to
//! settles the question at the only moment that matters, is indifferent to
//! when the tag was created, and survives a rewrite.

use crate::installers::InstallerDir;
use anyhow::{Context, Result};
use reqwest::Client;
use std::collections::BTreeMap;

/// Target systems that appear as the trailing component of an installer name.
/// Listed because they contain a `-` themselves, so the name cannot simply be
/// split on that character.
///
/// Kept in step with the systems the installers are built for; the name is
/// assembled in `nix/internal/any-darwin.nix:26`.
const KNOWN_SYSTEMS: [&str; 4] = [
    "x86_64-linux",
    "x86_64-darwin",
    "aarch64-darwin",
    "x86_64-windows",
];

/// Placeholder `buildRevShort` emits when the source tree was not clean
/// (`nix/internal/source-lib.nix:42-45`). Never acceptable in a release.
const DIRTY_REV: &str = "dirty";

/// The git revision recorded in an installer filename.
///
/// Names are built as
/// `daedalus-<version>-<build>-<cluster>-<revShort>-<system>.<ext>`
/// (`nix/internal/any-darwin.nix:26`). Parsed from the right, because the
/// system component contains a `-` and a prerelease version could too.
///
/// The build counter is decimal and therefore also valid hex, so matching on
/// "looks like a hex string" would be ambiguous. Position is not.
pub fn rev_from_filename(filename: &str) -> Option<&str> {
    let stem = filename
        .strip_suffix(".pkg.tar.zst")
        .or_else(|| filename.rsplit_once('.').map(|(stem, _)| stem))
        .unwrap_or(filename);
    let without_system = KNOWN_SYSTEMS.iter().find_map(|system| {
        stem.strip_suffix(system)
            .and_then(|rest| rest.strip_suffix('-'))
    })?;

    let (_, rev) = without_system.rsplit_once('-')?;
    if rev.is_empty() {
        return None;
    }
    Some(rev)
}

/// Shortest abbreviation accepted when comparing two revisions.
///
/// Installer names carry nine characters. Anything shorter is not a revision
/// this tool produced, and treating it as a prefix would make the comparison
/// far weaker than it looks — `same_commit("a", …)` should not be true.
const MIN_REV_LEN: usize = 7;

/// True when two revisions name the same commit.
///
/// The GitHub API and `meta.json` carry the full 40 characters while a
/// filename carries the first nine, so the shorter is compared as a prefix of
/// the longer. Abbreviations below `MIN_REV_LEN` never match.
fn same_commit(left: &str, right: &str) -> bool {
    let (shorter, longer) = if left.len() <= right.len() {
        (left, right)
    } else {
        (right, left)
    };
    shorter.len() >= MIN_REV_LEN && longer.starts_with(shorter)
}

/// Resolve a release tag to the commit it names, via the GitHub REST API.
///
/// `drt` runs where there is no git: the `ops` shell provides the binary and
/// gnupg and nothing else, and the tool otherwise reaches the network only
/// over HTTPS. So the tag is resolved the same way everything else is
/// fetched.
///
/// An annotated tag resolves to a tag object rather than a commit, which then
/// has to be peeled. `GITHUB_TOKEN` is used when present — a public repository
/// needs no credentials, but the unauthenticated limit is 60 requests an hour
/// and shared by IP.
pub async fn resolve_tag_commit(client: &Client, repo: &str, tag: &str) -> Result<String> {
    // Note the ref is given without the `refs/` prefix: the endpoint is
    // `git/ref/tags/<tag>`, and `git/ref/refs/tags/<tag>` returns 404.
    let object = fetch_git_object(
        client,
        &format!("https://api.github.com/repos/{repo}/git/ref/tags/{tag}"),
    )
    .await
    .with_context(|| format!("resolving tag {tag} in {repo}"))?
    .ok_or_else(|| {
        anyhow::anyhow!(
            "tag {tag} does not exist in {repo}. Publishing asserts that this \
             version corresponds to a commit; without the tag there is nothing \
             to check that against. Create the tag, or pass --skip-tag-check if \
             it is deliberately absent."
        )
    })?;

    match object.kind.as_str() {
        // Lightweight tag: already the commit.
        "commit" => Ok(object.sha),
        // Annotated tag: peel it to the commit it points at.
        "tag" => {
            let peeled = fetch_git_object(
                client,
                &format!(
                    "https://api.github.com/repos/{repo}/git/tags/{}",
                    object.sha
                ),
            )
            .await
            .with_context(|| format!("peeling annotated tag {tag} in {repo}"))?
            .ok_or_else(|| anyhow::anyhow!("tag object {} vanished while peeling", object.sha))?;
            Ok(peeled.sha)
        }
        other => anyhow::bail!("tag {tag} in {repo} points at a {other}, not a commit"),
    }
}

/// The `object` field shared by the ref and tag endpoints.
struct GitObject {
    sha: String,
    kind: String,
}

/// `None` when the resource does not exist; `Err` for anything else.
async fn fetch_git_object(client: &Client, url: &str) -> Result<Option<GitObject>> {
    let response = client
        .get(url)
        .header("Accept", "application/vnd.github+json")
        .send()
        .await?;

    if response.status() == reqwest::StatusCode::NOT_FOUND {
        return Ok(None);
    }
    let status = response.status();
    if !status.is_success() {
        let body = response.text().await.unwrap_or_default();
        anyhow::bail!("GET {url} returned {status}: {}", body.trim());
    }

    let body: serde_json::Value = response.json().await?;
    let object = body
        .get("object")
        .ok_or_else(|| anyhow::anyhow!("GET {url} returned no 'object' field"))?;
    Ok(Some(GitObject {
        sha: object
            .get("sha")
            .and_then(|v| v.as_str())
            .ok_or_else(|| anyhow::anyhow!("GET {url} returned no object.sha"))?
            .to_string(),
        kind: object
            .get("type")
            .and_then(|v| v.as_str())
            .ok_or_else(|| anyhow::anyhow!("GET {url} returned no object.type"))?
            .to_string(),
    }))
}

/// Verify that everything about to be uploaded was built from one commit, and
/// that it is the commit `tag_rev` names.
///
/// `tag_rev` is the commit the release tag resolves to, or `None` when the
/// caller could not determine it — in which case only the internal consistency
/// of the directory is checked, and the caller is responsible for deciding
/// whether that is acceptable.
pub fn verify(dir: &InstallerDir, tag_rev: Option<&str>) -> Result<()> {
    if dir.installers.is_empty() {
        anyhow::bail!("no installers found in {}", dir.dir.display());
    }

    // Group by revision so a mismatch can name every file on each side rather
    // than just the first disagreement.
    let mut by_rev: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for installer in &dir.installers {
        let rev = rev_from_filename(&installer.filename).with_context(|| {
            format!(
                "cannot read a git revision from '{}'; expected \
                 daedalus-<version>-<build>-<cluster>-<rev>-<system>.<ext>",
                installer.filename
            )
        })?;
        by_rev.entry(rev).or_default().push(&installer.filename);
    }

    if by_rev.len() > 1 {
        let detail = by_rev
            .iter()
            .map(|(rev, files)| format!("  {rev}: {}", files.join(", ")))
            .collect::<Vec<_>>()
            .join("\n");
        anyhow::bail!(
            "installers were built from more than one commit, so they cannot be \
             released together:\n{detail}"
        );
    }

    let rev = *by_rev.keys().next().expect("checked non-empty above");

    if rev == DIRTY_REV {
        anyhow::bail!(
            "installers were built from a dirty source tree, so the commit they \
             correspond to is unknown and unreproducible"
        );
    }

    if let Some(meta_rev) = dir.meta.gitrev.as_deref() {
        if !same_commit(meta_rev, rev) {
            anyhow::bail!(
                "meta.json records gitrev {meta_rev}, but the installers were \
                 built from {rev}; the directory holds artifacts from a \
                 different build than its metadata describes"
            );
        }
    }

    match tag_rev {
        Some(tag_rev) if !same_commit(tag_rev, rev) => {
            anyhow::bail!(
                "tag {} points at {}, but the installers were built from {}.\n\
                 Publishing would claim a version corresponds to a commit it \
                 was not built from. Either the tag is on the wrong commit, or \
                 these are the wrong artifacts.",
                dir.version,
                tag_rev,
                rev
            );
        }
        Some(tag_rev) => {
            println!(
                "  provenance: {rev} matches tag {} ({tag_rev})",
                dir.version
            );
        }
        None => {
            println!("  provenance: {rev}, consistent across all installers");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rev_from_linux_installer_name() {
        assert_eq!(
            rev_from_filename("daedalus-11.2.0-86185-mainnet-8f9737937-x86_64-linux.bin"),
            Some("8f9737937")
        );
        assert_eq!(
            rev_from_filename("daedalus-11.2.0-86185-mainnet-8f9737937-x86_64-linux.pkg.tar.zst"),
            Some("8f9737937")
        );
    }

    #[test]
    fn rev_from_every_target_system() {
        for (name, expected) in [
            (
                "daedalus-11.2.0-86185-mainnet-8f9737937-x86_64-darwin.pkg",
                "8f9737937",
            ),
            (
                "daedalus-11.2.0-86185-preprod-8f9737937-aarch64-darwin.pkg",
                "8f9737937",
            ),
            (
                "daedalus-11.2.0-86185-mainnet-8f9737937-x86_64-windows.exe",
                "8f9737937",
            ),
        ] {
            assert_eq!(rev_from_filename(name), Some(expected), "for {name}");
        }
    }

    /// The build counter is decimal and therefore also a valid hex string, so a
    /// parser that searched for "something hex-looking" could return it.
    #[test]
    fn build_counter_is_not_mistaken_for_a_revision() {
        assert_eq!(
            rev_from_filename("daedalus-7.3.0-83641-mainnet-4fe3ea852-x86_64-linux.bin"),
            Some("4fe3ea852")
        );
        assert_eq!(
            rev_from_filename("daedalus-11.2.0-86185-mainnet-123456789-x86_64-linux.bin"),
            Some("123456789")
        );
    }

    /// A prerelease version contains a `-`, so the name cannot be parsed by
    /// counting fields from the left.
    #[test]
    fn version_containing_a_dash_is_handled() {
        assert_eq!(
            rev_from_filename("daedalus-11.2.0-rc1-86185-mainnet-8f9737937-x86_64-linux.bin"),
            Some("8f9737937")
        );
    }

    #[test]
    fn dirty_tree_marker_is_read_back() {
        assert_eq!(
            rev_from_filename("daedalus-11.2.0-86185-mainnet-dirty-x86_64-linux.bin"),
            Some(DIRTY_REV)
        );
    }

    #[test]
    fn unrecognised_names_yield_nothing() {
        assert_eq!(rev_from_filename("daedalus.bin"), None);
        assert_eq!(rev_from_filename("daedalus-11.2.0-mainnet.bin"), None);
        // A system outside the supported set.
        assert_eq!(
            rev_from_filename("daedalus-11.2.0-86185-mainnet-8f9737937-riscv64-linux.bin"),
            None
        );
    }

    #[test]
    fn short_and_full_revisions_compare_equal() {
        assert!(same_commit(
            "8f9737937d346e847c9c29c965fba7aa44136612",
            "8f9737937"
        ));
        assert!(same_commit(
            "8f9737937",
            "8f9737937d346e847c9c29c965fba7aa44136612"
        ));
        assert!(!same_commit(
            "fa8258757ac0b88c71fe82fb91bad9597af2920e",
            "8f9737937"
        ));
        assert!(!same_commit("", "8f9737937"));
        // Abbreviations too short to be one of ours never match, however
        // tempting a prefix they make.
        assert!(!same_commit("8", "8f9737937"));
        assert!(!same_commit("8f973", "8f9737937"));
        assert!(same_commit(
            "8f97379",
            "8f9737937d346e847c9c29c965fba7aa44136612"
        ));
    }
}

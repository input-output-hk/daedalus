# Skip unchanged Linux repository uploads

## Specification

Continue the existing publishing feature in this worktree. Before uploading any
APT, YUM or Arch object, compare remote metadata with the exact local bytes.
Skip transfers when size and checksum match. Existence or size alone is not
sufficient. Changed bytes must still be published, including regenerated indexes
and signatures. Preserve bucket prefixes, ACL behavior and requested content-type
and cache-control headers. Errors checking remote state must abort rather than
trigger a blind upload. This is local code work; do not access live deployment
credentials or change the live repository.

## Plan

Add a repository-specific S3 upload method in `release-cli/src/s3.rs` and use it
at all `publish-linux-repos` call sites. HEAD checks use size and SHA-256 stored
as user metadata. For existing uploads lacking SHA-256 metadata, recognize an
exact single-part MD5 ETag matching the local MD5. Opaque/multipart ETags cannot
prove equality and cause one upload that records SHA-256 for subsequent runs.
An existing SHA-256 checksum takes precedence over any ETag.

For equal contents with missing SHA-256 or different publication headers, perform
an ETag-conditional server-side self-copy with replacement metadata, preserving
other user metadata. This upgrades legacy objects and fixes cache headers without
transferring the package body. Uploads store SHA-256. Other release/newsfeed upload
methods retain their current behavior. Log distinct skip, metadata refresh and
upload actions.

Validate using the real AWS SDK against a local in-process mock S3 endpoint.
Cover absent objects, repeated identical uploads, same-size changed content,
legacy ETags, ambiguous ETags, header changes, bucket prefixes and HEAD errors.
Run tests, treefmt and Clippy through the release-cli Nix shell.

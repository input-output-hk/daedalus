#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p python3

"""
Find npm packages required at runtime by distributed node_modules but missing
from runtime-nodejs-deps.json.

Computes the full transitive closure by:
1. Starting from packages in runtime-nodejs-deps.json (and those in DIST_NM)
2. Scanning all JS/CJS/MJS files for require() calls (including nested node_modules)
3. For each newly found package, scanning IT too
4. Repeating until convergence

Only follows runtime require() calls (not package.json deps) to avoid
pulling in build tools and devDependencies.

Usage: ./scripts/find-missing-runtime-deps.py
"""

import json
import os
import re
import sys

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
REPO_ROOT = os.path.dirname(SCRIPT_DIR)

RUNTIME_DEPS_JSON = os.path.join(REPO_ROOT, "nix/internal/runtime-nodejs-deps.json")
# The daedalus-js store path - find it via the result symlink
RESULT = os.path.join(REPO_ROOT, "result")
DIST_NM = os.path.join(RESULT, "libexec/daedalus-js/node_modules")
# Full node_modules with all packages (from kept build dir or node_modules/)
FULL_NM = os.path.join(REPO_ROOT, "node_modules")

# Fall back to the kept build dir if node_modules/ doesn't exist
if not os.path.isdir(FULL_NM):
    kept_builds = "/nix/var/nix/builds"
    candidates = []
    if os.path.isdir(kept_builds):
        for d in os.listdir(kept_builds):
            nm = os.path.join(kept_builds, d, "build/source/node_modules")
            if os.path.isdir(nm):
                candidates.append(nm)
    if candidates:
        FULL_NM = sorted(candidates)[-1]  # most recent
        print(f"Using full node_modules from: {FULL_NM}", file=sys.stderr)
    else:
        print("ERROR: cannot find full node_modules", file=sys.stderr)
        sys.exit(1)

# Regex for require() calls - captures the module name
REQUIRE_RE = re.compile(r"""require\s*\(\s*['"]([^'"./][^'"]*)['"]\s*\)""")

def top_level_pkg(module_ref):
    "'@scope/name/foo' -> '@scope/name', 'name/foo' -> 'name'"
    parts = module_ref.split("/")
    if parts[0].startswith("@") and len(parts) >= 2:
        return "/".join(parts[:2])
    return parts[0]

def is_covered(distributed_set, pkg):
    """Check if pkg is covered by anything in distributed_set."""
    if pkg in distributed_set:
        return True
    # A scoped package like @trezor/connect is covered if '@trezor' is listed
    scope = pkg.split("/")[0]
    if scope.startswith("@") and scope in distributed_set:
        return True
    return False

def scan_requires(path):
    """Scan a file for require() calls, return set of top-level package names."""
    found = set()
    try:
        with open(path, "r", errors="replace") as f:
            content = f.read()
        for m in REQUIRE_RE.finditer(content):
            found.add(top_level_pkg(m.group(1)))
    except Exception:
        pass
    return found

def walk_js_files(directory):
    """Yield all .js, .cjs, .mjs files under directory."""
    for root, dirs, files in os.walk(directory):
        # Skip test dirs
        dirs[:] = [d for d in dirs if d not in ("test", "tests", "__tests__", ".bin")]
        for fname in files:
            if fname.endswith((".js", ".cjs", ".mjs")):
                yield os.path.join(root, fname)

BUILTINS = {
    "assert", "buffer", "child_process", "cluster", "console", "constants",
    "crypto", "dgram", "dns", "domain", "electron", "events", "fs", "http",
    "http2", "https", "module", "net", "os", "path", "perf_hooks",
    "process", "punycode", "querystring", "readline", "repl", "stream",
    "string_decoder", "timers", "tls", "trace_events", "tty", "url",
    "util", "v8", "vm", "wasi", "worker_threads", "zlib",
}

def scan_package(pkg, nm_root):
    """Scan a package's JS files for runtime require() calls."""
    requires = set()
    pkg_path = os.path.join(nm_root, pkg)
    if not os.path.isdir(pkg_path):
        # Try FULL_NM as fallback
        pkg_path = os.path.join(FULL_NM, pkg)
        if not os.path.isdir(pkg_path):
            return requires
    for js_file in walk_js_files(pkg_path):
        requires |= scan_requires(js_file)
    return requires

def compute_closure(seeds, nm_root):
    """
    Compute the transitive closure of runtime dependencies.
    Only follows actual require() calls in JS files - not package.json deps.
    Returns the full closure set (only packages that exist in FULL_NM).
    """
    scanned = set()
    closure = set()
    queue = list(seeds)

    while queue:
        pkg = queue.pop()
        if pkg in scanned:
            continue
        scanned.add(pkg)

        if pkg in BUILTINS:
            continue
        if not os.path.isdir(os.path.join(FULL_NM, pkg)):
            continue

        closure.add(pkg)

        for req in scan_package(pkg, nm_root):
            if req not in BUILTINS and req not in scanned:
                queue.append(req)

    return closure

def main():
    with open(RUNTIME_DEPS_JSON) as f:
        listed = set(json.load(f))

    if not os.path.isdir(DIST_NM):
        print(f"ERROR: distributed node_modules not found at {DIST_NM}", file=sys.stderr)
        print("Make sure 'result' symlink points to a built daedalus package.", file=sys.stderr)
        sys.exit(1)

    # Collect all packages actually present in the distributed node_modules
    dist_pkgs = set()
    for entry in os.listdir(DIST_NM):
        if entry.startswith("@"):
            scope_dir = os.path.join(DIST_NM, entry)
            if os.path.isdir(scope_dir):
                for sub in os.listdir(scope_dir):
                    dist_pkgs.add(f"{entry}/{sub}")
        else:
            dist_pkgs.add(entry)
    print(f"Distributed packages: {len(dist_pkgs)}", file=sys.stderr)

    # Compute full transitive closure starting from distributed packages
    # (which includes everything in listed that was successfully copied)
    print("Computing transitive closure via require() scanning...", file=sys.stderr)
    closure = compute_closure(dist_pkgs, DIST_NM)
    print(f"Full closure: {len(closure)} packages", file=sys.stderr)

    missing = sorted(p for p in closure if not is_covered(listed, p))

    if missing:
        print(f"\nMissing from runtime-nodejs-deps.json ({len(missing)} packages):")
        for pkg in missing:
            print(f"  {pkg}")
    else:
        print("\nNo missing packages found!")

if __name__ == "__main__":
    main()

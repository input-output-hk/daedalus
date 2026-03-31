from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = str(Path(__file__).resolve().parents[2])
PUBLISH_SCRIPT = os.path.join(REPO_ROOT, "scripts", "agentic-kb-publish.sh")
FETCH_SCRIPT = os.path.join(REPO_ROOT, "scripts", "agentic-kb-fetch.sh")


def _setup_temp_repo(tmp):
    """Create a temp directory mimicking the repo structure with scripts."""
    repo_root = os.path.join(tmp, "repo")
    scripts_dir = os.path.join(repo_root, "scripts")
    os.makedirs(scripts_dir)

    with open(PUBLISH_SCRIPT) as f:
        publish_content = f.read()
    with open(FETCH_SCRIPT) as f:
        fetch_content = f.read()

    publish_script = os.path.join(scripts_dir, "agentic-kb-publish.sh")
    fetch_script = os.path.join(scripts_dir, "agentic-kb-fetch.sh")
    with open(publish_script, "w") as f:
        f.write(publish_content)
    with open(fetch_script, "w") as f:
        f.write(fetch_content)
    os.chmod(publish_script, 0o755)
    os.chmod(fetch_script, 0o755)

    return repo_root, publish_script, fetch_script


def _env(shared_dir=None, extra=None):
    env = os.environ.copy()
    env.pop("AGENTIC_KB_SHARED_DIR", None)
    if shared_dir is not None:
        env["AGENTIC_KB_SHARED_DIR"] = shared_dir
    if extra:
        env.update(extra)
    return env


class PublishScriptTests(unittest.TestCase):
    def test_publish_missing_shared_dir_env(self):
        result = subprocess.run(
            ["bash", PUBLISH_SCRIPT],
            capture_output=True,
            text=True,
            env=_env(),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("AGENTIC_KB_SHARED_DIR is required", result.stderr)

    def test_publish_non_directory_shared_dir(self):
        with tempfile.NamedTemporaryFile() as tmp:
            result = subprocess.run(
                ["bash", PUBLISH_SCRIPT],
                capture_output=True,
                text=True,
                env=_env(shared_dir=tmp.name),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("does not exist or is not a directory", result.stderr)

    def test_publish_wrong_basename_shared_dir(self):
        with tempfile.TemporaryDirectory() as tmp:
            result = subprocess.run(
                ["bash", PUBLISH_SCRIPT],
                capture_output=True,
                text=True,
                env=_env(shared_dir=tmp),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("must point to the 'Daedalus_KB' folder", result.stderr)

    def test_publish_non_readable_shared_dir(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            os.chmod(shared_dir, 0o000)
            try:
                result = subprocess.run(
                    ["bash", PUBLISH_SCRIPT],
                    capture_output=True,
                    text=True,
                    env=_env(shared_dir=shared_dir),
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("not readable", result.stderr)
            finally:
                os.chmod(shared_dir, 0o755)

    def test_publish_non_writable_shared_dir(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            os.chmod(shared_dir, 0o555)
            try:
                result = subprocess.run(
                    ["bash", PUBLISH_SCRIPT],
                    capture_output=True,
                    text=True,
                    env=_env(shared_dir=shared_dir),
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("not writable", result.stderr)
            finally:
                os.chmod(shared_dir, 0o755)

    def test_publish_rejects_path_with_slash(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            result = subprocess.run(
                ["bash", PUBLISH_SCRIPT, "some/path.dump"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=shared_dir),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("not a path", result.stderr)

    def test_publish_strips_dump_suffix(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            result = subprocess.run(
                ["bash", PUBLISH_SCRIPT, "agentic-kb-test.dump"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=shared_dir),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertNotIn("not a path", result.stderr)

    def test_publish_strips_manifest_suffix(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            result = subprocess.run(
                ["bash", PUBLISH_SCRIPT, "agentic-kb-test.manifest.json"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=shared_dir),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertNotIn("not a path", result.stderr)

    def test_publish_sync_failure_propagation(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            fake_docker = os.path.join(tmp, "docker")
            with open(fake_docker, "w") as f:
                f.write("#!/bin/bash\nexit 1\n")
            os.chmod(fake_docker, 0o755)

            env = _env(shared_dir=shared_dir)
            env["PATH"] = tmp + ":" + env.get("PATH", "")

            result = subprocess.run(
                ["bash", PUBLISH_SCRIPT],
                capture_output=True,
                text=True,
                env=env,
            )
            self.assertNotEqual(result.returncode, 0)

    def test_publish_sibling_pair_enforcement(self):
        with tempfile.TemporaryDirectory() as tmp:
            repo_root, publish_script, _ = _setup_temp_repo(tmp)
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            snapshots_dir = os.path.join(repo_root, "agentic", "snapshots")
            os.makedirs(snapshots_dir)

            dump_path = os.path.join(snapshots_dir, "agentic-kb-test.dump")
            with open(dump_path, "w") as f:
                f.write("dummy dump")

            fake_docker = os.path.join(tmp, "docker")
            with open(fake_docker, "w") as f:
                f.write("#!/bin/bash\nexit 0\n")
            os.chmod(fake_docker, 0o755)

            env = _env(shared_dir=shared_dir)
            env["PATH"] = tmp + ":" + env.get("PATH", "")

            result = subprocess.run(
                ["bash", publish_script, "agentic-kb-test"],
                capture_output=True,
                text=True,
                env=env,
                cwd=repo_root,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("sibling manifest", result.stderr)

    def test_publish_copies_both_files_to_shared(self):
        with tempfile.TemporaryDirectory() as tmp:
            repo_root, publish_script, _ = _setup_temp_repo(tmp)
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            snapshots_dir = os.path.join(repo_root, "agentic", "snapshots")
            os.makedirs(snapshots_dir)

            dump_path = os.path.join(snapshots_dir, "agentic-kb-test.dump")
            manifest_path = os.path.join(snapshots_dir, "agentic-kb-test.manifest.json")
            with open(dump_path, "w") as f:
                f.write("dummy dump")
            with open(manifest_path, "w") as f:
                f.write("{}")

            fake_docker = os.path.join(tmp, "docker")
            with open(fake_docker, "w") as f:
                f.write("#!/bin/bash\nexit 0\n")
            os.chmod(fake_docker, 0o755)

            env = _env(shared_dir=shared_dir)
            env["PATH"] = tmp + ":" + env.get("PATH", "")

            result = subprocess.run(
                ["bash", publish_script, "agentic-kb-test"],
                capture_output=True,
                text=True,
                env=env,
                cwd=repo_root,
            )
            self.assertEqual(result.returncode, 0)
            self.assertTrue(os.path.exists(os.path.join(shared_dir, "agentic-kb-test.dump")))
            self.assertTrue(os.path.exists(os.path.join(shared_dir, "agentic-kb-test.manifest.json")))


class FetchScriptTests(unittest.TestCase):
    def test_fetch_requires_argument(self):
        result = subprocess.run(
            ["bash", FETCH_SCRIPT],
            capture_output=True,
            text=True,
            env=_env(),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("usage:", result.stderr)

    def test_fetch_missing_shared_dir_env(self):
        result = subprocess.run(
            ["bash", FETCH_SCRIPT, "agentic-kb-test"],
            capture_output=True,
            text=True,
            env=_env(),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("AGENTIC_KB_SHARED_DIR is required", result.stderr)

    def test_fetch_non_directory_shared_dir(self):
        with tempfile.NamedTemporaryFile() as tmp:
            result = subprocess.run(
                ["bash", FETCH_SCRIPT, "agentic-kb-test"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=tmp.name),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("does not exist or is not a directory", result.stderr)

    def test_fetch_wrong_basename_shared_dir(self):
        with tempfile.TemporaryDirectory() as tmp:
            result = subprocess.run(
                ["bash", FETCH_SCRIPT, "agentic-kb-test"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=tmp),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("must point to the 'Daedalus_KB' folder", result.stderr)

    def test_fetch_non_readable_shared_dir(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            os.chmod(shared_dir, 0o000)
            try:
                result = subprocess.run(
                    ["bash", FETCH_SCRIPT, "agentic-kb-test"],
                    capture_output=True,
                    text=True,
                    env=_env(shared_dir=shared_dir),
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("not readable", result.stderr)
            finally:
                os.chmod(shared_dir, 0o755)

    def test_fetch_rejects_path_with_slash(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            result = subprocess.run(
                ["bash", FETCH_SCRIPT, "some/path.dump"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=shared_dir),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("not a path", result.stderr)

    def test_fetch_strips_dump_suffix(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            result = subprocess.run(
                ["bash", FETCH_SCRIPT, "agentic-kb-test.dump"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=shared_dir),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertNotIn("not a path", result.stderr)

    def test_fetch_strips_manifest_suffix(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            result = subprocess.run(
                ["bash", FETCH_SCRIPT, "agentic-kb-test.manifest.json"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=shared_dir),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertNotIn("not a path", result.stderr)

    def test_fetch_sibling_pair_enforcement_missing_dump(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            manifest_path = os.path.join(shared_dir, "agentic-kb-test.manifest.json")
            with open(manifest_path, "w") as f:
                f.write("{}")

            result = subprocess.run(
                ["bash", FETCH_SCRIPT, "agentic-kb-test"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=shared_dir),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("shared snapshot dump does not exist", result.stderr)

    def test_fetch_sibling_pair_enforcement_missing_manifest(self):
        with tempfile.TemporaryDirectory() as tmp:
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            dump_path = os.path.join(shared_dir, "agentic-kb-test.dump")
            with open(dump_path, "w") as f:
                f.write("dummy dump")

            result = subprocess.run(
                ["bash", FETCH_SCRIPT, "agentic-kb-test"],
                capture_output=True,
                text=True,
                env=_env(shared_dir=shared_dir),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("shared snapshot manifest does not exist", result.stderr)

    def test_fetch_creates_local_snapshots_dir(self):
        with tempfile.TemporaryDirectory() as tmp:
            repo_root, _, fetch_script = _setup_temp_repo(tmp)
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            dump_path = os.path.join(shared_dir, "agentic-kb-test.dump")
            manifest_path = os.path.join(shared_dir, "agentic-kb-test.manifest.json")
            with open(dump_path, "w") as f:
                f.write("dummy dump")
            with open(manifest_path, "w") as f:
                f.write("{}")

            env = _env(shared_dir=shared_dir)

            result = subprocess.run(
                ["bash", fetch_script, "agentic-kb-test"],
                capture_output=True,
                text=True,
                env=env,
                cwd=repo_root,
            )
            self.assertEqual(result.returncode, 0)
            local_snapshots = os.path.join(repo_root, "agentic", "snapshots")
            self.assertTrue(os.path.isdir(local_snapshots))
            self.assertTrue(os.path.exists(os.path.join(local_snapshots, "agentic-kb-test.dump")))
            self.assertTrue(os.path.exists(os.path.join(local_snapshots, "agentic-kb-test.manifest.json")))

    def test_fetch_copies_both_files_to_local(self):
        with tempfile.TemporaryDirectory() as tmp:
            repo_root, _, fetch_script = _setup_temp_repo(tmp)
            shared_dir = os.path.join(tmp, "Daedalus_KB")
            os.makedirs(shared_dir)
            dump_path = os.path.join(shared_dir, "agentic-kb-test.dump")
            manifest_path = os.path.join(shared_dir, "agentic-kb-test.manifest.json")
            with open(dump_path, "w") as f:
                f.write("dummy dump")
            with open(manifest_path, "w") as f:
                f.write("{}")

            env = _env(shared_dir=shared_dir)

            result = subprocess.run(
                ["bash", fetch_script, "agentic-kb-test"],
                capture_output=True,
                text=True,
                env=env,
                cwd=repo_root,
            )
            self.assertEqual(result.returncode, 0)
            local_snapshots = os.path.join(repo_root, "agentic", "snapshots")
            self.assertTrue(os.path.exists(os.path.join(local_snapshots, "agentic-kb-test.dump")))
            self.assertTrue(os.path.exists(os.path.join(local_snapshots, "agentic-kb-test.manifest.json")))


if __name__ == "__main__":
    unittest.main()

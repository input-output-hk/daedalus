#!/usr/bin/env node
//
// Asserts that the documentation says things that are still true.
//
// Documentation in this repository has drifted before in ways no check could
// catch: a link to a directory that was renamed, an instruction to run a script
// that was itself renamed, a version number restated in prose that fell 17
// major releases behind the dependency it named. Each of those is mechanically
// decidable, and none of them was decided, because nothing looked.
//
// The four assertions below are the ones that can be made without a judgement
// call. A claim this check cannot express belongs in prose; a claim it can
// belongs here, because prose is not load-bearing and a failing build is.

'use strict';

const fs = require('fs');
const path = require('path');

const ROOT = process.argv[2] || process.cwd();

// Directories that never contain documentation we assert over.
const SKIP_DIRS = new Set([
  '.git',
  'node_modules',
  'dist',
  'release',
  'installers/icons',
]);

// `.agent/plans/` is a record of work as it stood when it was written, and
// `CHANGELOG.md` is a record of releases that have shipped. Both are correct
// as history and wrong as present-tense instruction, by design. Asserting
// present-tense truth over them would either fail permanently or require
// rewriting the record to keep a check green, and rewriting the record is
// worse than the drift.
const EXCLUDED = [
  (f) => f.startsWith('.agent/plans/'),
  (f) => f === 'CHANGELOG.md',
];

// yarn's own subcommands. `yarn link` is not a missing script.
const YARN_BUILTINS = new Set([
  'add',
  'audit',
  'autoclean',
  'bin',
  'cache',
  'check',
  'config',
  'create',
  'dedupe',
  'exec',
  'generate-lock-entry',
  'global',
  'help',
  'import',
  'info',
  'init',
  'install',
  'licenses',
  'link',
  'list',
  'login',
  'logout',
  'node',
  'outdated',
  'owner',
  'pack',
  'policies',
  'publish',
  'remove',
  'run',
  'set',
  'tag',
  'team',
  'unlink',
  'upgrade',
  'upgrade-interactive',
  'version',
  'versions',
  'why',
  'workspace',
  'workspaces',
]);

// Commands documented here that are deliberately not Daedalus scripts. Each
// entry needs a reason; without one it is indistinguishable from drift.
const YARN_EXCEPTIONS = {
  // README's yarn-link instructions run this in the linked library's tree.
  'README.md': new Set(['build:watch']),
};

// References to systems this project no longer uses. They are dead rather than
// merely stale: following one leads to a workspace, tracker or toolchain that
// is not there. A reader cannot tell the difference from the page, which is
// why these are worth naming individually rather than trusting review to catch.
const DENIED = [
  ['input-output-rnd.slack.com', 'a Slack workspace this project cannot reach'],
  ['daedalus-qa', 'a GitHub team that does not exist'],
  ['daedalus-dev', 'a GitHub team that does not exist'],
  ['manage:translations', 'renamed; the script is i18n:manage'],
  ['flow-typed', 'Flow was removed from this codebase'],
  ['DDW-', 'an issue-tracker prefix no longer in use'],
  ['YouTrack', 'not the tracker for this project'],
  ['Jira', 'not the tracker for this project'],
];

// Files exempted from the denylist, with the reason. An exemption is a debt,
// not a resolution.
const DENY_EXCEPTIONS = {
  // Retiring this document is a decision in its own right. Until it is taken,
  // exempt the file rather than half-editing a document that is going away.
  'BESTPRACTICES.md': true,
};

// Versions restated in prose, and the dependency each one claims to describe.
// Longest key first: "React Router" must not be read as "React".
const VERSION_CLAIMS = [
  ['React Router', 'react-router'],
  ['react-polymorph', 'react-polymorph'],
  ['react-intl', 'react-intl'],
  ['Electron', 'electron'],
  ['Webpack', 'webpack'],
  ['MobX', 'mobx'],
  ['React', 'react'],
];

const failures = [];
const fail = (file, line, msg) =>
  failures.push(`${file}${line ? ':' + line : ''}  ${msg}`);

function walk(dir, out) {
  for (const entry of fs.readdirSync(path.join(ROOT, dir || '.'))) {
    const rel = dir ? `${dir}/${entry}` : entry;
    if (SKIP_DIRS.has(rel) || SKIP_DIRS.has(entry)) continue;
    const stat = fs.lstatSync(path.join(ROOT, rel));
    if (stat.isSymbolicLink()) continue;
    if (stat.isDirectory()) walk(rel, out);
    else if (entry.endsWith('.md')) out.push(rel);
  }
  return out;
}

const lineOf = (text, index) => text.slice(0, index).split('\n').length;

// Everything outside fenced blocks and inline code, for link checking. Links
// inside a code sample are illustrative and are not expected to resolve.
const prose = (text) =>
  text
    .replace(/```[\s\S]*?```/g, (m) => m.replace(/[^\n]/g, ' '))
    .replace(/`[^`\n]*`/g, (m) => ' '.repeat(m.length));

// Only code spans and fenced blocks, for command checking. A command named in
// prose without backticks is usually a noun, not an instruction.
function codeRegions(text) {
  const out = [];
  for (const m of text.matchAll(/```[a-zA-Z]*\n([\s\S]*?)```/g))
    out.push([m[1], m.index + m[0].indexOf('\n') + 1]);
  for (const m of text.matchAll(/`([^`\n]+)`/g)) out.push([m[1], m.index + 1]);
  return out;
}

function checkLinks(file, text) {
  const body = prose(text);
  for (const m of body.matchAll(/\[[^\]]*\]\(([^)\s]+)(?:\s+"[^"]*")?\)/g)) {
    const target = m[1];
    if (/^(https?:|mailto:|#)/.test(target)) continue;
    const [p] = target.split('#');
    if (!p) continue;
    let decoded;
    try {
      decoded = decodeURIComponent(p);
    } catch {
      decoded = p;
    }
    const abs = path.resolve(ROOT, path.dirname(file), decoded);
    if (!fs.existsSync(abs))
      fail(file, lineOf(text, m.index), `link does not resolve: ${target}`);
  }
}

function checkYarnScripts(file, text, scripts) {
  const allowed = YARN_EXCEPTIONS[file] || new Set();
  for (const [region, offset] of codeRegions(text)) {
    for (const m of region.matchAll(/\byarn\s+([a-zA-Z][a-zA-Z0-9:_.-]*)/g)) {
      const name = m[1];
      // `yarn nix:*` and `yarn nix:<network>` stand for a set, not a script.
      const next = region[m.index + m[0].length];
      if (name.endsWith(':') || next === '*' || next === '<') continue;
      if (scripts.has(name) || YARN_BUILTINS.has(name) || allowed.has(name))
        continue;
      fail(
        file,
        lineOf(text, offset + m.index),
        `documents 'yarn ${name}', which is not a script in package.json`
      );
    }
  }
}

function checkDenied(file, text) {
  if (DENY_EXCEPTIONS[file]) return;
  for (const [needle, why] of DENIED) {
    let i = text.indexOf(needle);
    while (i !== -1) {
      fail(file, lineOf(text, i), `refers to '${needle}': ${why}`);
      i = text.indexOf(needle, i + needle.length);
    }
  }
}

function checkVersions(file, text, deps) {
  const seen = [];
  for (const [label, pkg] of VERSION_CLAIMS) {
    const actual = deps[pkg];
    if (!actual) continue;
    const re = new RegExp(
      `\\b${label.replace(/ /g, '\\s+')}\\s+v?(\\d+(?:\\.\\d+)*)\\b`,
      'g'
    );
    for (const m of text.matchAll(re)) {
      // "React Router 5.2.0" already consumed; do not read it again as "React".
      if (seen.some(([s, e]) => m.index >= s && m.index < e)) continue;
      seen.push([m.index, m.index + m[0].length]);
      const claimed = m[1];
      const resolved = actual.replace(/^[\^~>=<\s]+/, '');
      if (resolved !== claimed && !resolved.startsWith(claimed + '.'))
        fail(
          file,
          lineOf(text, m.index),
          `states ${label} ${claimed}; package.json resolves ${pkg} to ${resolved}`
        );
    }
  }
}

function main() {
  const pkg = JSON.parse(
    fs.readFileSync(path.join(ROOT, 'package.json'), 'utf8')
  );
  const scripts = new Set(Object.keys(pkg.scripts || {}));
  const deps = { ...pkg.dependencies, ...pkg.devDependencies };

  const files = walk('', []).filter((f) => !EXCLUDED.some((fn) => fn(f)));
  files.sort();

  for (const file of files) {
    const text = fs.readFileSync(path.join(ROOT, file), 'utf8');
    checkLinks(file, text);
    checkYarnScripts(file, text, scripts);
    checkDenied(file, text);
    checkVersions(file, text, deps);
  }

  if (failures.length) {
    console.error(
      `\nDocumentation check failed, ${failures.length} problem(s):\n`
    );
    for (const f of failures) console.error('  ' + f);
    console.error('');
    process.exit(1);
  }
  console.log(`Documentation check passed over ${files.length} files.`);
}

main();

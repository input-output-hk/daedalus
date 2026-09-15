#!/usr/bin/env node
/*
 * Find story render functions that read an argument Storybook does not fill.
 *
 * Storybook calls a story as render(context.args, context). The first argument
 * is the story's args and nothing else: not the toolbar globals, and not an
 * object a decorator passed by calling story({ ... }), which Storybook merges
 * onto the context instead, after stripping title, id, name, kind, story,
 * parameters, initialArgs and argTypes. So a render function that reads its
 * first argument while neither its own story nor its meta declares args is
 * handed {}, and renders a component with nothing in it.
 *
 * Nothing else in this repository sees that. The story compiles, it lints, the
 * workbench builds, the indexer lists it, the panel appears in the sidebar with
 * the right label, and the component inside it is empty. A browser-driven render
 * check would catch it; there is no browser here. This scan needs neither a
 * browser nor a display, and it costs a parse per story file.
 *
 * Usage:
 *   node story-args-audit.js [root ...]
 *
 * Roots default to the two the story globs cover. Exits 1 when it finds
 * anything, so it can be run as a check.
 *
 * A story render is the `render` property of an exported story object, or an
 * exported top-level function assigned to a const, which is CSF's other story
 * form. Helper functions in the same file are not story renders and are not
 * reported.
 *
 * A story is often a binding rather than a literal function:
 * `export const Thing = SomeStory`, where `SomeStory` is declared in this file
 * or imported from a relative path. Both are followed. What is left is a render
 * that comes back from a call, `export const Thing = makeStory({ ... })`, which
 * would need the callee analysed; those are listed as unresolved rather than
 * passed over, because a scan that silently ignores a shape is worse than one
 * that says which shapes it skipped.
 */
const fs = require('fs');
const path = require('path');

// Resolved the way the rest of this plan's tools resolve it: from the
// repository's own dependencies, so the parser matches the compiler.
const ts = require(
  require.resolve('typescript', { paths: [process.cwd()] })
);

const STORY_FILE = /\.(stories|story)\.tsx?$/;
const SKIP_DIRS = new Set(['node_modules', 'dist', '.git']);
const NON_STORY_EXPORTS = new Set(['default', '__namedExportsOrder']);
const IGNORED_PARAM_NAMES = new Set(['_', '_args']);

const DEFAULT_ROOTS = ['storybook/stories', 'source/renderer/app'];

function findStoryFiles(dir, out) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (!SKIP_DIRS.has(entry.name)) findStoryFiles(p, out);
    } else if (STORY_FILE.test(entry.name)) {
      out.push(p);
    }
  }
  return out;
}

const parseCache = new Map();
function parse(file) {
  if (!parseCache.has(file)) {
    parseCache.set(
      file,
      ts.createSourceFile(
        file,
        fs.readFileSync(file, 'utf8'),
        ts.ScriptTarget.Latest,
        true,
        ts.ScriptKind.TSX
      )
    );
  }
  return parseCache.get(file);
}

// Resolve a relative import specifier the way tsc would for this repository's
// extensions. Only relative paths: a story imported from a package is not ours.
function resolveRelative(fromFile, specifier) {
  if (!specifier.startsWith('.')) return null;
  const base = path.resolve(path.dirname(fromFile), specifier);
  for (const candidate of [
    base,
    `${base}.tsx`,
    `${base}.ts`,
    path.join(base, 'index.tsx'),
    path.join(base, 'index.ts'),
  ]) {
    if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) return candidate;
  }
  return null;
}

const isFunction = (n) =>
  n && (ts.isArrowFunction(n) || ts.isFunctionExpression(n));

const declaresArgs = (obj) =>
  obj.properties.some(
    (p) =>
      ts.isPropertyAssignment(p) && ts.isIdentifier(p.name) && p.name.text === 'args'
  );

function readsFirstArgument(fn) {
  const param = fn.parameters[0];
  if (!param) return false;
  if (ts.isObjectBindingPattern(param.name)) return true;
  const name = param.name.getText();
  if (IGNORED_PARAM_NAMES.has(name)) return false;
  return new RegExp(`\\b${name}\\b`).test(fn.body.getText());
}

function auditFile(file) {
  const sf = parse(file);

  let metaDeclaresArgs = false;
  const renders = [];
  const notAnalysed = [];

  // Find the function a name is bound to, following one relative import if the
  // name is not declared here. One hop is enough for this corpus and keeps the
  // scan a scan rather than a type checker.
  const functionNamed = (name, source = sf, followImports = true) => {
    let found = null;
    const walk = (n) => {
      if (found) return;
      if (
        ts.isVariableDeclaration(n) &&
        ts.isIdentifier(n.name) &&
        n.name.text === name &&
        isFunction(n.initializer)
      ) {
        found = n.initializer;
      } else if (
        ts.isFunctionDeclaration(n) &&
        n.name &&
        n.name.text === name &&
        n.body
      ) {
        found = n;
      }
      ts.forEachChild(n, walk);
    };
    walk(source);
    if (found || !followImports) return found;

    for (const st of source.statements) {
      if (
        !ts.isImportDeclaration(st) ||
        !st.importClause ||
        !ts.isStringLiteral(st.moduleSpecifier)
      ) {
        continue;
      }
      const bindings = st.importClause.namedBindings;
      const named =
        bindings && ts.isNamedImports(bindings)
          ? bindings.elements.find((e) => e.name.text === name)
          : null;
      const isDefault = st.importClause.name && st.importClause.name.text === name;
      if (!named && !isDefault) continue;
      const target = resolveRelative(source.fileName, st.moduleSpecifier.text);
      if (!target) return null;
      const exported = named ? (named.propertyName || named.name).text : 'default';
      if (exported === 'default') return null;
      return functionNamed(exported, parse(target), false);
    }
    return null;
  };

  const objectNamed = (name) => {
    let found = null;
    const walk = (n) => {
      if (
        !found &&
        ts.isVariableDeclaration(n) &&
        ts.isIdentifier(n.name) &&
        n.name.text === name &&
        n.initializer &&
        ts.isObjectLiteralExpression(n.initializer)
      ) {
        found = n.initializer;
      }
      ts.forEachChild(n, walk);
    };
    walk(sf);
    return found;
  };

  const visit = (n) => {
    if (ts.isExportAssignment(n) && !n.isExportEquals) {
      const meta = ts.isObjectLiteralExpression(n.expression)
        ? n.expression
        : ts.isIdentifier(n.expression)
        ? objectNamed(n.expression.text)
        : null;
      if (meta) metaDeclaresArgs = declaresArgs(meta);
    }

    if (
      ts.isVariableStatement(n) &&
      n.modifiers &&
      n.modifiers.some((m) => m.kind === ts.SyntaxKind.ExportKeyword)
    ) {
      for (const d of n.declarationList.declarations) {
        if (!ts.isIdentifier(d.name) || NON_STORY_EXPORTS.has(d.name.text)) continue;
        if (!d.initializer) continue;
        if (ts.isObjectLiteralExpression(d.initializer)) {
          const ownArgs = declaresArgs(d.initializer);
          for (const p of d.initializer.properties) {
            if (
              ts.isPropertyAssignment(p) &&
              ts.isIdentifier(p.name) &&
              p.name.text === 'render' &&
              isFunction(p.initializer)
            ) {
              renders.push({ story: d.name.text, fn: p.initializer, ownArgs });
            }
          }
        } else if (isFunction(d.initializer)) {
          renders.push({ story: d.name.text, fn: d.initializer, ownArgs: false });
        } else if (ts.isIdentifier(d.initializer)) {
          const target = functionNamed(d.initializer.text);
          if (target) {
            renders.push({ story: d.name.text, fn: target, ownArgs: false });
          } else {
            notAnalysed.push({
              file,
              story: d.name.text,
              source: `${d.initializer.text}, declared elsewhere`,
              line: sf.getLineAndCharacterOfPosition(d.getStart()).line + 1,
            });
          }
        } else if (ts.isCallExpression(d.initializer)) {
          notAnalysed.push({
            file,
            story: d.name.text,
            source: `${d.initializer.expression.getText()}(...)`,
            line: sf.getLineAndCharacterOfPosition(d.getStart()).line + 1,
          });
        }
      }
    }
    ts.forEachChild(n, visit);
  };
  visit(sf);

  return {
    findings: renders
      .filter((r) => readsFirstArgument(r.fn))
      .map((r) => ({
        file,
        story: r.story,
        params: r.fn.parameters.map((p) => p.name.getText()).join(', '),
        line: sf.getLineAndCharacterOfPosition(r.fn.getStart()).line + 1,
        argsDeclared: metaDeclaresArgs || r.ownArgs,
      })),
    notAnalysed,
  };
}

const roots = process.argv.slice(2).length ? process.argv.slice(2) : DEFAULT_ROOTS;
const files = roots
  .filter((r) => fs.existsSync(r))
  .flatMap((r) => findStoryFiles(r, []))
  .sort();

const results = files.map(auditFile);
const rows = results.flatMap((r) => r.findings);
const skipped = results.flatMap((r) => r.notAnalysed);
const unfilled = rows.filter((r) => !r.argsDeclared);

for (const r of unfilled) {
  console.log(`${r.file}:${r.line}  ${r.story}(${r.params})  reads args, none declared`);
}
if (skipped.length) {
  console.log('');
  console.log('Not analysed, because the render is not declared here:');
  for (const r of skipped) {
    console.log(`  ${r.file}:${r.line}  ${r.story} = ${r.source}`);
  }
}
console.log('');
console.log(`story files scanned:                 ${files.length}`);
console.log(`renders reading the first argument:  ${rows.length}`);
console.log(`  with args declared:                ${rows.length - unfilled.length}`);
console.log(`  with no args declared:             ${unfilled.length}`);
console.log(`renders this scan cannot resolve:    ${skipped.length}`);

process.exit(unfilled.length ? 1 : 0);

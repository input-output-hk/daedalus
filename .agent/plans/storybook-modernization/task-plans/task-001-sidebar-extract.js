/*
 * Storybook sidebar extractor for the storiesOf() corpus.
 *
 * Produces the artifact committed beside this file as task-001-sidebar-baseline.txt.
 * Run it from the repository root, against a tree whose dependencies are installed,
 * because it uses the repository's own TypeScript parser:
 *
 *   node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .
 *
 * Modes: no argument for the full artifact, --tree, --index or --json for one part.
 *
 * It reads storiesOf() and .add(), so it stops working when the corpus becomes CSF.
 * The artifact it produces outlives it; this script does not.
 */
'use strict';

let ts;
try {
  ts = require('typescript');
} catch (e) {
  process.stderr.write(
    'Cannot resolve typescript. Run this from a tree with dependencies installed,\n' +
      'or set NODE_PATH to one that has them.\n'
  );
  process.exit(1);
}

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(process.argv[2] || '.');
const MODE = process.argv[3] || '--all';
const EXTS = ['.ts', '.tsx', '.js', '.jsx'];
const UNIT = String.fromCharCode(1);

/* What the indexer loads. Until task-010 this was one hand-maintained barrel at
 * storybook/stories/index.ts, and a story file left out of it registered nothing
 * while every check stayed green. storybook/main.ts now declares two globs, so
 * the roots are every file matching the story naming convention under the two
 * directories those globs name. */
const STORY_FILE = /\.(stories|story)\.(ts|tsx)$/;
const GLOB_DIRS = ['storybook/stories', 'source/renderer/app'];

function resolveSpec(fromFile, spec) {
  if (!spec.startsWith('.')) return null;
  const base = path.resolve(path.dirname(fromFile), spec);
  for (const e of EXTS) {
    if (fs.existsSync(base + e) && fs.statSync(base + e).isFile()) return base + e;
  }
  if (fs.existsSync(base) && fs.statSync(base).isDirectory()) {
    for (const e of EXTS) {
      const p = path.join(base, 'index' + e);
      if (fs.existsSync(p)) return p;
    }
  }
  if (fs.existsSync(base) && fs.statSync(base).isFile()) return base;
  return null;
}

function parse(file) {
  const kind =
    file.endsWith('.tsx') || file.endsWith('.jsx') ? ts.ScriptKind.TSX : ts.ScriptKind.TS;
  return ts.createSourceFile(
    file,
    fs.readFileSync(file, 'utf8'),
    ts.ScriptTarget.Latest,
    true,
    kind
  );
}

/* Which modules the indexer loads, and what it therefore registers. */
const reachable = new Set();
function walk(file) {
  if (reachable.has(file)) return;
  reachable.add(file);
  const sf = parse(file);
  const specs = [];
  const visit = (n) => {
    if (ts.isImportDeclaration(n) && ts.isStringLiteral(n.moduleSpecifier)) {
      specs.push(n.moduleSpecifier.text);
    } else if (
      ts.isExportDeclaration(n) &&
      n.moduleSpecifier &&
      ts.isStringLiteral(n.moduleSpecifier)
    ) {
      specs.push(n.moduleSpecifier.text);
    }
    ts.forEachChild(n, visit);
  };
  visit(sf);
  for (const s of specs) {
    const r = resolveSpec(file, s);
    if (r && !r.includes('node_modules')) walk(r);
  }
}

function findFiles(dir, out) {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, e.name);
    if (e.isDirectory()) {
      if (e.name === 'node_modules' || e.name === '.git') continue;
      findFiles(p, out);
    } else if (/\.(ts|tsx)$/.test(e.name)) {
      out.push(p);
    }
  }
  return out;
}

for (const dir of GLOB_DIRS) {
  const abs = path.join(ROOT, dir);
  if (!fs.existsSync(abs)) continue;
  for (const f of findFiles(abs, [])) {
    if (STORY_FILE.test(path.basename(f))) walk(f);
  }
}

/* File-local constants a label can be written through. Nine of the labels in the
 * corpus are not string literals. */
function constants(sf) {
  const maps = new Map();
  const scalars = new Map();
  const visit = (n) => {
    if (ts.isVariableDeclaration(n) && ts.isIdentifier(n.name) && n.initializer) {
      if (ts.isObjectLiteralExpression(n.initializer)) {
        const m = {};
        for (const p of n.initializer.properties) {
          if (ts.isPropertyAssignment(p) && ts.isStringLiteral(p.initializer)) {
            const key =
              ts.isIdentifier(p.name) || ts.isStringLiteral(p.name) ? p.name.text : null;
            if (key !== null) m[key] = p.initializer.text;
          }
        }
        maps.set(n.name.text, m);
      } else if (ts.isStringLiteral(n.initializer)) {
        scalars.set(n.name.text, n.initializer.text);
      }
    }
    ts.forEachChild(n, visit);
  };
  visit(sf);
  return { maps, scalars };
}

function labelOf(arg, ctx) {
  if (!arg) return { value: null, kind: 'missing' };
  if (ts.isStringLiteral(arg) || ts.isNoSubstitutionTemplateLiteral(arg)) {
    return { value: arg.text, kind: 'literal' };
  }
  if (ts.isPropertyAccessExpression(arg) && ts.isIdentifier(arg.expression)) {
    const m = ctx.maps.get(arg.expression.text);
    if (m && arg.name.text in m) return { value: m[arg.name.text], kind: 'const' };
  }
  if (
    ts.isElementAccessExpression(arg) &&
    ts.isIdentifier(arg.expression) &&
    arg.argumentExpression &&
    ts.isStringLiteral(arg.argumentExpression)
  ) {
    const m = ctx.maps.get(arg.expression.text);
    if (m && arg.argumentExpression.text in m) {
      return { value: m[arg.argumentExpression.text], kind: 'const' };
    }
  }
  if (ts.isIdentifier(arg) && ctx.scalars.has(arg.text)) {
    return { value: ctx.scalars.get(arg.text), kind: 'const' };
  }
  if (ts.isTemplateExpression(arg)) {
    let out = arg.head.text;
    for (const span of arg.templateSpans) {
      const part = labelOf(span.expression, ctx);
      if (part.kind === 'unresolved' || part.kind === 'missing') {
        return { value: arg.getText(), kind: 'unresolved' };
      }
      out += part.value + span.literal.text;
    }
    return { value: out, kind: 'template' };
  }
  return { value: arg.getText(), kind: 'unresolved' };
}

const rows = [];
const unresolved = [];

const candidates = [
  ...findFiles(path.join(ROOT, 'storybook'), []),
  ...findFiles(path.join(ROOT, 'source'), []),
].sort();

for (const file of candidates) {
  if (!fs.readFileSync(file, 'utf8').includes('storiesOf(')) continue;
  const sf = parse(file);
  const ctx = constants(sf);
  const rel = path.relative(ROOT, file);

  const titleOfNode = new Map();
  const varTitle = new Map();
  const visitStories = (n) => {
    if (
      ts.isCallExpression(n) &&
      ts.isIdentifier(n.expression) &&
      n.expression.text === 'storiesOf'
    ) {
      const t = n.arguments[0];
      const title =
        t && (ts.isStringLiteral(t) || ts.isNoSubstitutionTemplateLiteral(t))
          ? t.text
          : labelOf(t, ctx).value;
      titleOfNode.set(n, title);
      let p = n.parent;
      while (p && (ts.isPropertyAccessExpression(p) || ts.isCallExpression(p))) p = p.parent;
      if (p && ts.isVariableDeclaration(p) && ts.isIdentifier(p.name)) {
        varTitle.set(p.name.text, title);
      }
    }
    ts.forEachChild(n, visitStories);
  };
  visitStories(sf);

  /* Walk a .add() receiver back to the storiesOf() call that owns it, so a .add()
   * on anything else is never counted as a registration. */
  const receiverTitle = (expr) => {
    let e = expr;
    for (;;) {
      if (ts.isCallExpression(e)) {
        if (ts.isIdentifier(e.expression) && e.expression.text === 'storiesOf') {
          return titleOfNode.has(e) ? titleOfNode.get(e) : null;
        }
        e = e.expression;
      } else if (ts.isPropertyAccessExpression(e)) {
        e = e.expression;
      } else if (ts.isParenthesizedExpression(e)) {
        e = e.expression;
      } else if (ts.isIdentifier(e)) {
        return varTitle.has(e.text) ? varTitle.get(e.text) : null;
      } else {
        return null;
      }
    }
  };

  const visitAdds = (n) => {
    if (
      ts.isCallExpression(n) &&
      ts.isPropertyAccessExpression(n.expression) &&
      n.expression.name.text === 'add'
    ) {
      const title = receiverTitle(n.expression.expression);
      if (title !== null && title !== undefined) {
        const l = labelOf(n.arguments[0], ctx);
        const line =
          sf.getLineAndCharacterOfPosition(n.expression.name.getStart()).line + 1;
        if (l.kind === 'unresolved' || l.kind === 'missing') {
          unresolved.push(rel + ':' + line + '  ' + title + '  ' + l.value);
        }
        rows.push({
          file: rel,
          line,
          title,
          label: l.value,
          kind: l.kind,
          reachable: reachable.has(file),
        });
      }
    }
    ts.forEachChild(n, visitAdds);
  };
  visitAdds(sf);
}

const key = (r) => [r.title, r.label, r.file, String(r.line).padStart(6, '0')].join(UNIT);
rows.sort((a, b) => (key(a) < key(b) ? -1 : key(a) > key(b) ? 1 : 0));
unresolved.sort();

const out = [];
const emit = (s) => out.push(s);

const preamble = () => {
  emit('# Storybook sidebar baseline');
  emit('#');
  emit('# Every storiesOf().add() registration in the corpus, with the group and panel');
  emit('# it appears under and the label it appears as. Regenerate with:');
  emit('#');
  emit('#   node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .');
  emit('#');
  emit('# The tree is the diff surface. It carries no path and no line number, so an edit');
  emit('# above a registration cannot perturb it. The index below carries provenance and is');
  emit('# expected to move whenever a file changes.');
  emit('#');
  emit('# storiesOf() titles are full sidebar paths. The tree splits each at its first');
  emit('# separator, so a three-level title prints as a group and a two-level panel.');
  emit('');
};

const tree = () => {
  const groups = new Map();
  for (const r of rows) {
    const i = r.title.indexOf(' / ');
    const group = i === -1 ? r.title.trim() : r.title.slice(0, i).trim();
    const panel = i === -1 ? '(no panel)' : r.title.slice(i + 3).trim();
    if (!groups.has(group)) groups.set(group, new Map());
    const g = groups.get(group);
    if (!g.has(panel)) g.set(panel, []);
    g.get(panel).push(r);
  }
  const names = [...groups.keys()].sort();
  for (const gn of names) {
    emit('GROUP  ' + gn);
    for (const pn of [...groups.get(gn).keys()].sort()) {
      const rs = groups.get(gn).get(pn);
      emit('  PANEL  ' + pn + '  [' + rs.length + ']');
      for (const r of rs) emit('    STORY  ' + r.label + (r.reachable ? '' : '  (UNREACHABLE)'));
    }
  }
  emit('');
  emit('GROUPS ' + names.length);
  emit('PANELS ' + new Set(rows.map((r) => r.title)).size);
  emit('REGISTRATIONS ' + rows.length);
  emit('REACHABLE ' + rows.filter((r) => r.reachable).length);
  emit('UNREACHABLE ' + rows.filter((r) => !r.reachable).length);
  emit('UNRESOLVED LABELS ' + unresolved.length);
  for (const u of unresolved) emit('  ' + u);
};

const index = () => {
  emit('');
  emit('# Index: title | label | path:line | whether the barrel loads the module');
  emit('');
  for (const r of rows) {
    emit(
      [r.title, r.label, r.file + ':' + r.line, r.reachable ? 'loaded' : 'NOT LOADED'].join(
        '  |  '
      )
    );
  }
};

if (MODE === '--json') {
  process.stdout.write(JSON.stringify({ rows, unresolved }, null, 2) + '\n');
} else {
  if (MODE === '--all') preamble();
  if (MODE === '--all' || MODE === '--tree') tree();
  if (MODE === '--all' || MODE === '--index') index();
  process.stdout.write(out.join('\n') + '\n');
}

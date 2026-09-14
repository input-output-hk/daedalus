{
  nodejs,
  runCommand,
  src,
}:
# The whole source tree is the input, not just the markdown. The link assertion
# resolves references to any tracked file (flake.nix, a component, a SKILL.md),
# so a filtered source would report links as broken here that resolve for every
# reader, which is the opposite of what this check is for.
runCommand "daedalus-docs" {nativeBuildInputs = [nodejs];} ''
  node ${src}/tests/docs-check.js ${src}
  touch $out
''

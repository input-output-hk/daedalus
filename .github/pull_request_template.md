<!---
Briefly describe the change and why it is needed.
-->

This PR ...

Closes #

## Screenshots

<!---
For user interface changes, attach screenshots or a short recording of a
default-sized Daedalus window. Include both English and Japanese where the
change affects text.
-->

## Verification

<!---
Name the checks you ran. Every CI check has an exact local equivalent:

  nix build .#checks.x86_64-linux.lint
  nix build .#checks.x86_64-linux.compile
  nix build .#checks.x86_64-linux.stylelint
  nix build .#checks.x86_64-linux.i18n
  nix build .#checks.x86_64-linux.docs
  nix build .#checks.x86_64-linux.jest
  nix build .#checks.x86_64-linux.cucumber-unit
  nix build .#checks.x86_64-linux.storybook

Describe anything verified by hand that no check covers, and how.
-->

- [ ] The checks relevant to this change pass locally
- [ ] Anything verified by hand is described above

## Tests

- [ ] A bug fix includes a test that fails without the fix
- [ ] New behaviour is covered by tests, or the reason it cannot be is stated

## Code quality

- [ ] Code is typed, and introduces no new `@ts-ignore`
- [ ] React components are split up enough to avoid unnecessary re-renders
- [ ] Code that only works in the main process is separated from components
- [ ] `yarn.lock` is updated if dependencies changed

## Review

- [ ] PR is assigned to its author
- [ ] PR carries the labels that apply to it
- [ ] PR is current with the target branch and has no conflicts
- [ ] The description above summarises every change in the PR

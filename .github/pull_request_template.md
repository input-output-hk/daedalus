<!---
Briefly describe the change.
-->

This PR ...

## Todos

<!---
Consider creating a TODO list to help others understand the progress of work in a WIP pull request.
-->

- [ ] TODO

## Screenshots

<!---
Use the GitHub drag&drop feature to upload default-sized Daedalus window screenshots
or animated GIFs of important UI changes in both English and Japanese.
Do not use shadow or any effects. On macOS this can be accomplished the following way:
1. Use the Command+Shift+4 keyboard shortcut.
2. Press the Spacebar.
3. Hold the Option button and click the window you want to capture.
-->

## Testing Checklist

<!---
Open a thread on #daedalus-qa on Slack, mention `@daedalusqa` and `@daedalusteam`, link the thread below
-->

- [Slack QA thread](https://input-output-rnd.slack.com/messages/GGKFXSKC6)
- [ ] Test

---

## Review Checklist

### Basics

- [ ] PR assigned to the PR author(s)
- [ ] `input-output-hk/daedalus-dev` and `input-output-hk/daedalus-qa` assigned as PR reviewers
- [ ] If there are UI changes, Alexander Rukin assigned as an additional reviewer
- [ ] PR has appropriate labels (`release-vNext`, `feature`/`bug`/`chore`, `WIP`)
- [ ] PR link is added to a Jira ticket, ticket moved to In Review
- [ ] PR is updated to the most recent version of the target branch (and there are no conflicts)
- [ ] PR has a good description that summarizes all changes
- [ ] PR contains screenshots (in case of UI changes)
- [ ] CHANGELOG entry has been added to the top of the appropriate section (_Features_, _Fixes_, _Chores_) and is linked to the correct PR on GitHub
- [ ] There are no missing translations (running `yarn manage:translations` produces no changes)
- [ ] Text changes are proofread and approved (Jane Wild / Amy Reeve)
- [ ] Japanese text changes are proofread and approved (Junko Oda)
- [ ] Storybook works and no stories are broken (`yarn storybook`)
- [ ] In case of dependency changes `yarn.lock` file is updated

### Code Quality

- [ ] Important parts of the code are properly commented and documented
- [ ] Code is properly typed with typescript types
- [ ] React components are split-up enough to avoid unnecessary re-renderings
- [ ] Any code that only works in main process is neatly separated from components

### Testing

- [ ] New feature/change is covered by acceptance tests
- [ ] New feature/change is manually tested and approved by QA team
- [ ] All existing acceptance tests are still up-to-date
- [ ] New feature/change is covered by Daedalus Testing scenario
- [ ] All existing Daedalus Testing scenarios are still up-to-date

### After Review

- [ ] Update Slack QA thread by marking it with a green checkmark

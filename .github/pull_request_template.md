This PR CHANGES.

## Todos

- [ ] Todo

## Screenshots

- Screenshot

---

## Testing Checklist

- [Slack QA thread](https://input-output-rnd.slack.com/messages/GGKFXSKC6)
- [ ] Test

---

## Review Checklist

### Basics

- [ ] PR has been assigned and has appropriate labels (`feature`/`bug`/`chore`, `release-x.x.x`)
- [ ] PR is updated to the most recent version of the target branch (and there are no conflicts)
- [ ] PR has a good description that summarizes all changes and shows some screenshots or animated GIFs of important UI changes
- [ ] CHANGELOG entry has been added to the top of the appropriate section (*Features*, *Fixes*, *Chores*) and is linked to the correct PR on GitHub
- [ ] Automated tests: All acceptance  and unit tests are passing (`yarn run test` and `yarn run test:unit`)
- [ ] Manual tests (minimum tests should cover newly added feature/fix): App works correctly in *development* build (`yarn run dev`)
- [ ] Manual tests (minimum tests should cover newly added feature/fix): App works correctly in *production* build (`yarn run package` / CI builds)
- [ ] There are no *flow* errors or warnings (`yarn run flow:test`)
- [ ] There are no *lint* errors or warnings (`yarn run lint`)
- [ ] There are no *prettier* errors or warnings (`yarn run prettier:check`)
- [ ] There are no missing translations (running `yarn run manage:translations` produces no changes)
- [ ] Text changes are proofread and approved (Jane Wild / Amy Reeve)
- [ ] Japanese text changes are proofread and approved (Junko Oda)
- [ ] UI changes look good in all themes (Alexander Rukin)
- [ ] Storybook works and no stories are broken (`yarn run storybook`)
- [ ] In case of dependency changes `yarn.lock` file is updated

### Code Quality
- [ ] Important parts of the code are properly commented and documented
- [ ] Code is properly typed with flow
- [ ] React components are split-up enough to avoid unnecessary re-renderings
- [ ] Any code that only works in main process is neatly separated from components

### Testing
- [ ] New feature/change is covered by acceptance tests
- [ ] New feature/change is manually tested and approved by QA team
- [ ] All existing acceptance tests are still up-to-date
- [ ] New feature/change is covered by Daedalus Testing scenario
- [ ] All existing Daedalus Testing scenarios are still up-to-date

### After Review
- [ ] Merge the PR
- [ ] Delete the source branch
- [ ] Move the ticket to `done` column on the YouTrack board
- [ ] Update Slack QA thread by marking it with a green checkmark

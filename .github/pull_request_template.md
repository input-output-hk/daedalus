This PR CHANGES.

## Todo:

- [ ] Todo
- [ ] Todo
- [ ] Todo

## Screenshots:

- Screenshot

---

## Review Checklist:

### Basics

- [ ] PR is updated to the most recent version of target branch (and there are no conflicts)
- [ ] PR has good description that summarizes all changes and shows some screenshots or animated GIFs of important UI changes
- [ ] CHANGELOG entry has been added and is linked to the correct PR on GitHub
- [ ] Automated tests: All acceptance tests are passing (`yarn run test`)
- [ ] Manual tests (minimum tests should cover newly added feature/fix): App works correctly in *development* build (`yarn run dev`)
- [ ] Manual tests (minimum tests should cover newly added feature/fix): App works correctly in *production* build (`yarn run package` / CI builds)
- [ ] There are no *flow* errors or warnings (`yarn run flow:test`)
- [ ] There are no *lint* errors or warnings (`yarn run lint`)
- [ ] Text changes are proofread and approved (Jane Wild)
- [ ] There are no missing translations (running `yarn run manage:translations` produces no changes)
- [ ] UI changes look good in all themes (Alexander Rukin)
- [ ] Storybook works and no stories are broken (`yarn run storybook`)
- [ ] In case of dependency changes `yarn.lock` file is updated

### Code Quality
- [ ] Important parts of the code are properly documented and commented
- [ ] Code is properly typed with flow
- [ ] React components are split-up enough to avoid unnecessary re-rendering
- [ ] Any code that only works in Electron is neatly separated from components

### Testing
- [ ] New feature / change is covered by acceptance tests
- [ ] All existing acceptance tests are still up-to-date
- [ ] New feature / change is covered by Daedalus Testing scenario
- [ ] All existing Daedalus Testing scenarios are still up-to-date

### After Review:
- [ ] Merge PR
- [ ] Delete source branch
- [ ] Move ticket to `done` on the Youtrack board

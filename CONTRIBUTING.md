# Contributing to Daedalus

Thank you for your interest in contributing to Daedalus.

SE7EN Labs welcomes community contributions including bug reports, code improvements, testing, documentation and other work that improves Daedalus for its users.

## Getting Support

GitHub is primarily used for development and public issue tracking. If you need individual assistance using Daedalus or troubleshooting a wallet problem, please follow the guidance in [SUPPORT.md](https://github.com/se7en-labs-inc/daedalus/blob/master/SUPPORT.md).

Do not publish wallet recovery phrases, seed phrases, private keys, spending passwords or other wallet secrets in GitHub issues, pull requests or discussions.

## Before Opening an Issue

Search the existing GitHub issues before creating a new one. An existing issue may already describe the same problem or proposed improvement.

When reporting a bug, provide enough information for maintainers to understand and reproduce the problem. Relevant information may include the Daedalus version, operating system, expected behavior, actual behavior and clear reproduction steps.

Do not include sensitive personal information or wallet secrets.

Diagnostic files, screenshots and logs should only be shared publicly when you are confident that they contain no sensitive information.

## Issue Triage

New GitHub issues will receive a first maintainer response within 2 business days and will be triaged within 5 business days.

Triage may include classification, labeling, requesting additional information, identifying a duplicate, determining whether an issue belongs upstream or establishing the appropriate next action.

The complete service targets are documented in [Service Level Targets](https://github.com/se7en-labs-inc/daedalus/blob/master/docs/SERVICE_LEVELS.md).

Detailed issue triage, labeling and assignment standards are documented in [Issue Triage](https://github.com/se7en-labs-inc/daedalus/blob/master/docs/ISSUE_TRIAGE.md).

## Working on an Issue

Before beginning substantial work, review the issue and any existing discussion to make sure the proposed change is still appropriate.

For issues that are already assigned, coordinate with the current assignee or a maintainer before beginning overlapping work.

For unassigned issues, contributors are encouraged to express interest in the issue before investing significant development time. A maintainer may then confirm the expected scope and provide any relevant guidance.

Assignment indicates ownership of the next action on an issue. It should not be interpreted as permanent ownership of a particular area of the Daedalus codebase.

## Good First Issues

Issues labeled `good first issue` are intended to provide approachable entry points for contributors who are new to the Daedalus codebase.

A Good First Issue should have a clearly understood problem, a reasonably well defined expected outcome and a scope suitable for a focused pull request. It should not depend on unresolved architectural or product decisions.

Good First Issues should also have enough context for a contributor to identify the relevant area of the project and understand how the change can be validated.

Maintainers may remove the label if investigation shows that an issue is substantially more complex or sensitive than originally expected.

Detailed standards for selecting and maintaining Good First Issues are documented in [Issue Triage](https://github.com/se7en-labs-inc/daedalus/blob/master/docs/ISSUE_TRIAGE.md).

## Development Environment

Development environment setup instructions are maintained in the repository [README](https://github.com/se7en-labs-inc/daedalus/blob/master/README.md).

Contributors should follow the documented setup process for the platform and network environment relevant to their work.

## Making Changes

Keep changes focused on the issue or purpose described by the pull request.

Avoid unrelated refactoring or formatting changes unless they are necessary to complete the work. Smaller focused changes are generally easier to review, test and maintain.

Changes should preserve existing security assumptions and should not weaken protections around wallet secrets, signing, transaction handling or other security sensitive functionality.

## Testing

Contributors should test their changes appropriately before submitting a pull request.

Where practical, bug fixes should include coverage that demonstrates the corrected behavior and new functionality should include appropriate tests.

Existing tests relevant to the affected area should continue to pass.

If a change cannot reasonably be tested through automated coverage, explain how the change was validated in the pull request.

## Pull Requests

Pull requests should clearly explain what is being changed and why.

Reference the related GitHub issue when one exists and describe how the change was tested.

A pull request may require additional changes before it can be merged. Maintainers may request revisions for correctness, security, maintainability, testing, documentation or consistency with the direction of the project.

Submitting a pull request does not guarantee that a proposed change will be accepted.

## Security Sensitive Issues

Do not publicly disclose suspected vulnerabilities, wallet secrets or information that could place users or their assets at risk.

Security sensitive reports should be handled through the repository's designated private security reporting process rather than through a public GitHub issue.

## Community Participation

Constructive participation is encouraged whether you are contributing code, testing a proposed change, improving documentation or helping reproduce an issue.

Clear issue reports, reliable reproduction steps and focused pull requests are valuable contributions to the project even when they do not involve substantial code changes.

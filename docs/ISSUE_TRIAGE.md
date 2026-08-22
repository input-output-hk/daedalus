# GitHub Issue Triage

SE7EN Labs uses a consistent triage process for issues submitted to the public Daedalus GitHub repository.

The purpose of triage is to make sure new issues are reviewed, classified and given a clear next action within a reasonable period of time.

The service targets governing GitHub issue response and triage are documented in [Service Level Targets](https://github.com/se7en-labs-inc/daedalus/blob/master/docs/SERVICE_LEVELS.md).

## Triage Target

New GitHub issues should receive a first maintainer response within 2 business days and should be triaged within 5 business days.

Triage does not require that an issue be resolved within this period.

An issue is considered triaged when a maintainer has reviewed the available information and established an appropriate classification and current disposition.

Depending on the issue, this may include applying labels, requesting additional information, identifying a duplicate, determining that the issue belongs to an upstream project or identifying the appropriate next action.

## Labeling Standards

Labels should communicate the nature of an issue and its current state without requiring maintainers or contributors to infer meaning from an unnecessarily large label set.

### Issue Type

Where applicable, an issue should receive a label identifying its primary type.

| Label           | Purpose                                              |
| --------------- | ---------------------------------------------------- |
| `bug`           | Confirmed or suspected incorrect software behavior   |
| `enhancement`   | Proposed improvement or new functionality            |
| `documentation` | Documentation additions, corrections or improvements |
| `question`      | Technical question requiring maintainer review       |

### Workflow State

Workflow labels describe information or action currently required.

| Label              | Purpose                                                                    |
| ------------------ | -------------------------------------------------------------------------- |
| `needs triage`     | Issue has not yet completed maintainer triage                              |
| `needs info`       | Additional information is required from the reporter                       |
| `blocked`          | Progress depends on another issue, dependency or external condition        |
| `duplicate`        | The issue is already represented by another GitHub issue                   |
| `upstream`         | The issue appears to require action in an upstream dependency or project   |
| `help wanted`      | Maintainers welcome community assistance with the issue                    |
| `good first issue` | Issue meets the project's standards for an approachable first contribution |

Labels should describe the current state of an issue accurately. Maintainers should remove labels that are no longer applicable as the issue changes.

Additional area specific labels may be used where they materially improve routing or discovery. Area labels should identify a meaningful component or functional area rather than creating labels for individual files or narrowly scoped implementation details.

## Issue Disposition

Triage should establish what currently needs to happen next.

A triaged issue may be confirmed for further investigation, awaiting information from the reporter, identified as a duplicate, referred to an upstream project, accepted as an enhancement request, made available for community contribution or closed when no further action is appropriate.

Triage is not a determination that a fix will be implemented and does not establish a resolution deadline.

## Assignment Standards

Assignment indicates ownership of the next meaningful action on an issue.

An issue should be assigned when a maintainer, contributor or other project participant has accepted responsibility for that next action.

Assignment should not be used merely because a person has expertise in the affected area.

Unassigned issues may remain available for maintainer review or community contribution when no individual has accepted ownership.

If an assigned contributor is no longer able to work on an issue, the assignment may be removed so that another contributor can take ownership.

Issues awaiting information from the reporter do not require an engineering assignee unless additional maintainer work is also underway.

For significant confirmed issues requiring active investigation, maintainers should establish a clear owner when practical.

## Community Assignment

Community contributors are encouraged to indicate their interest before beginning substantial work on an unassigned issue.

A maintainer may confirm the expected scope and assign the issue to the contributor.

Assignment does not guarantee acceptance of a future pull request. Contributions remain subject to review, testing and the project's technical and security requirements.

Contributor guidance is available in [CONTRIBUTING.md](https://github.com/se7en-labs-inc/daedalus/blob/master/CONTRIBUTING.md).

## Good First Issue Standard

The `good first issue` label is reserved for work that provides a reasonable entry point for contributors who are unfamiliar with the Daedalus codebase.

An issue should only receive the `good first issue` label when the problem is sufficiently understood, the expected outcome is reasonably clear and the work can be completed as a focused contribution without requiring unresolved architectural or product decisions.

The issue description or maintainer discussion should provide enough context for a new contributor to identify the relevant area of the project and understand how the proposed change can be validated.

Suitable Good First Issues may include documentation improvements, focused user interface corrections, test improvements, accessibility improvements, small diagnostics improvements and well understood isolated defects.

Issues involving substantial architectural changes, unresolved design decisions or security sensitive functionality should generally not be labeled as Good First Issues.

Maintainers may remove the `good first issue` label if further investigation shows that the work is more complex, sensitive or uncertain than originally expected.

## Security Sensitive Issues

Suspected vulnerabilities or issues involving wallet secrets, signing, key handling or other security sensitive behavior should not be managed as ordinary public contribution opportunities when disclosure could place users or assets at risk.

Public reports that appear security sensitive should be redirected to the project's designated private security reporting process as appropriate.

Security sensitive issues should not normally receive the `good first issue` label.

## Review of Triage Standards

SE7EN Labs may update these standards as the Daedalus project, contributor community and support operations evolve.

Changes should preserve the goal of providing clear and consistent expectations for issue reporters, contributors and maintainers.

## Related Documentation

For customer support guidance, see [SUPPORT.md](https://github.com/se7en-labs-inc/daedalus/blob/master/SUPPORT.md).

For contributor guidance, see [CONTRIBUTING.md](https://github.com/se7en-labs-inc/daedalus/blob/master/CONTRIBUTING.md).

For response and triage targets, see [Service Level Targets](https://github.com/se7en-labs-inc/daedalus/blob/master/docs/SERVICE_LEVELS.md).

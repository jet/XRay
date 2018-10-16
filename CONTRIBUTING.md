# Contributing guidelines

### Getting Started

- Install required software
- Clone the repo \
  `git clone https://github.com/jet/XRay.git`
- Read how to build in [DEVGUIDE.md](DEVGUIDE.md)
- Read how to run tests in [TESTGUIDE.md](TESTGUIDE.md)

## Pull Request Checklist

Before sending your pull requests, make sure you followed this list.

- Read [contributing guidelines](cCONTRIBUTING.md#contribution-guidelines-and-standards).
- Read [Code of Conduct](CODE_OF_CONDUCT.md).
- Ensure you have signed the Contributor License Agreement (CLA).
- Check if your changes are consistent with the guidelines.
- Changes are consistent with the [Coding Style](CodingStyle.md).
- Run Unit Tests.
- Search GitHub for an open or closed Pull Request that relates to your submission. You don't want to duplicate effort.
- Before embarking on an extensive feature implementation, consider making a proposal in a GitHub issue.

## How to become a contributor and submit your code

### Contributor License Agreements

Before we can incorporate your contribution we have to jump a couple of legal hurdles.

You will be asked to fill out either the individual or corporate Contributor License Agreement (CLA).

  * If you are an individual writing original source code and you're sure you own the intellectual property, then you'll need to sign an individual CLA.
  * If you work for a company that wants to allow you to contribute your work, then you'll need to sign a corporate CLA.

Follow either of the two links above your PR to access the appropriate CLA and instructions for how to sign and return it. Once we receive it, we'll be able to accept your PRs. This only needs to be done once for each Jet OSS project you contribute to.

***NOTE***: Only original source code from you and other people who have agreed and signed the CLA can be accepted.

### Contributing code

If you have improvements to this project, send us your PRs! For those just getting started, Github has a [howto](https://help.github.com/articles/using-pull-requests/).

Our team members will be assigned to review your PRs. Once the PRs are approved and pass continuous integration checks, we will merge them.
The commits in the PR might be squashed into a single commit with the PR creator as the author.

If you want to contribute but you're not sure where to start, take a look at the issues with the "help wanted" label.

If you decide to start working on an issue, leave a comment so that other people know that you're working on it.

### Contribution guidelines and standards

Make sure your changes are consistent with these guidelines and follow our coding style before sending your PR for review.

#### General guidelines and philosophy for contribution

- Unit tests must be included within a feature or fix PR (unless tests already exist for a failing case), as they help to prove that your code works correctly, and guard against future breaking changes.
- Keep API compatibility in mind when you change existing code, once we reach version 1 we cannot make non-backward-compatible API changes without a major release. Reviewers of your PR are expected to comment on any backward compatibility issue.
- When you contribute to a new feature, the maintenance burden is transferred to our team. This means that the benefit of the contribution must be compared against the cost of maintaining the feature.
- Partial, incomplete, or poorly-tested contributions will not be accepted.
- Contributions may be put on hold according to stability, testing, and design-coherence requirements.
- If your PR is not ready for review, name it prefixing it with [WIP]
- Before submitting it to review make sure to squash it in single commit, if it makes sense to. There are some cases where it would makes sense to squash it in individual commits to make it easier to review and to keep a clean history, in these cases the review will be made commit by commit, instead of file by file.
- Don't commit comment out code, remove it instead.
- Commit / PR messages should be of a form that they answer the question "If I add this commit/PR to the repo, it will <commit message>"
- The code changes must be reasonably minimal and as low-churn, non-intrusive as possible. Unrelated cleanup should be done in separate PRs, and fixes should be as small as possible. Code cleanup that is part of making a clear and accurate fix is acceptable as part of a bug fix, but care should be taken that it doesn't obscure the fix itself. For example, renaming identifiers to be clearer in a way that would have avoided the original bug is acceptable, but care must still be taken that the actual fix is still apparent and reviewable in the overall diff for the fix.

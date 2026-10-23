/**
 * Declares a test whose body asserts behaviour the code does not have yet.
 *
 * The test passes while an assertion in `body` fails, and fails once every
 * assertion holds, so the declaration cannot outlive the defect it names.
 * Anything other than a failed assertion, such as a missing fixture or a
 * TypeError, still fails the test.
 *
 * With DAEDALUS_KNOWN_DEFECTS=assert in the environment, `body` runs as a
 * plain `it` and each defect is reported as a failure. The retired-DRep
 * withdrawal harness (tests/cluster/drep-withdrawal) runs these specs that
 * way.
 */
export const knownDefect = (
  name: string,
  defect: string,
  body: () => unknown | Promise<unknown>
): void => {
  if (process.env.DAEDALUS_KNOWN_DEFECTS === 'assert') {
    it(name, async () => {
      await body();
    });
    return;
  }
  it(`${name} [known defect: ${defect}]`, async () => {
    let failure: unknown = null;
    try {
      await body();
    } catch (error) {
      failure = error;
    }
    if (failure === null) {
      throw new Error(`Every assertion holds; declare it with it(): ${name}`);
    }
    if (
      typeof failure !== 'object' ||
      failure === null ||
      !('matcherResult' in failure)
    ) {
      throw failure;
    }
  });
};

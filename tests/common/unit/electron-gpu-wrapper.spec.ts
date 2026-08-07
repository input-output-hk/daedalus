// TypeScript equivalent of the DAEDALUS_DISABLE_GPU block in
// nix/internal/x86_64-linux.nix. Tested here without spawning child processes
// so it runs in the Nix build sandbox (srcWithoutNix, no execve outside store).
const applyGpuFlag = (
  args: string[],
  env: Record<string, string | undefined>
): string[] => (env.DAEDALUS_DISABLE_GPU ? ['--disable-gpu', ...args] : args);

describe('electron wrapper GPU flag injection', () => {
  it('injects --disable-gpu when DAEDALUS_DISABLE_GPU=1', () => {
    expect(
      applyGpuFlag(['--other-arg'], { DAEDALUS_DISABLE_GPU: '1' })
    ).toContain('--disable-gpu');
  });

  it('does not inject --disable-gpu when DAEDALUS_DISABLE_GPU is unset', () => {
    expect(applyGpuFlag(['--other-arg'], {})).not.toContain('--disable-gpu');
  });

  it('injects --disable-gpu as first arg and preserves others', () => {
    const args = applyGpuFlag(['--other-arg'], { DAEDALUS_DISABLE_GPU: '1' });
    expect(args[0]).toBe('--disable-gpu');
    expect(args).toContain('--other-arg');
  });
});

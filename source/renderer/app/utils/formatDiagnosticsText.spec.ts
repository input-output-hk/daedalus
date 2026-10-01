import { formatDiagnosticsText } from './formatDiagnosticsText';

describe('formatDiagnosticsText', () => {
  it('wraps the output in a fenced text block', () => {
    const text = formatDiagnosticsText([
      { title: 'SYSTEM INFO', rows: [{ label: 'Platform', value: 'linux' }] },
    ]);

    expect(text.startsWith('```text\n')).toBe(true);
    expect(text.endsWith('\n```')).toBe(true);
  });

  it('renders each row as a labelled line under its section title', () => {
    const text = formatDiagnosticsText([
      {
        title: 'SYSTEM INFO',
        rows: [
          { label: 'Platform', value: 'linux' },
          { label: 'RAM', value: '16 GB' },
        ],
      },
    ]);

    expect(text).toBe('```text\nSYSTEM INFO\nPlatform: linux\nRAM: 16 GB\n```');
  });

  it('separates sections with a blank line', () => {
    const text = formatDiagnosticsText([
      { title: 'A', rows: [{ label: 'x', value: 1 }] },
      { title: 'B', rows: [{ label: 'y', value: 2 }] },
    ]);

    expect(text).toBe('```text\nA\nx: 1\n\nB\ny: 2\n```');
  });

  it('renders numeric zero values', () => {
    const text = formatDiagnosticsText([
      { title: 'A', rows: [{ label: 'Restarts', value: 0 }] },
    ]);

    expect(text).toContain('Restarts: 0');
  });

  it('omits sections without rows', () => {
    const text = formatDiagnosticsText([
      { title: 'EMPTY', rows: [] },
      { title: 'A', rows: [{ label: 'x', value: 'y' }] },
    ]);

    expect(text).not.toContain('EMPTY');
  });

  it('returns an empty fenced block when there are no sections', () => {
    expect(formatDiagnosticsText([])).toBe('```text\n\n```');
  });
});

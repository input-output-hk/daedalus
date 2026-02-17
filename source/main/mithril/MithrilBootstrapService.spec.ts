import {
  parseMithrilProgressLine,
  parseMithrilProgressUpdate,
} from './mithrilProgress';

describe('MithrilBootstrapService parsing', () => {
  it('returns null for non-JSON lines', () => {
    expect(parseMithrilProgressLine('Download started')).toBeNull();
    expect(parseMithrilProgressLine('')).toBeNull();
  });

  it('returns number for numeric progress', () => {
    expect(parseMithrilProgressLine('{"progress": 42}')).toBe(42);
  });

  it('returns number for numeric string progress', () => {
    expect(parseMithrilProgressLine('{"progress": "15"}')).toBe(15);
  });

  it('returns number for percent alias', () => {
    expect(parseMithrilProgressLine('{"percent": 65}')).toBe(65);
    expect(parseMithrilProgressLine('{"percentage": 80}')).toBe(80);
  });

  it('returns null when progress is not numeric', () => {
    expect(parseMithrilProgressLine('{"progress": "foo"}')).toBeNull();
  });

  it('parses file progress totals', () => {
    expect(
      parseMithrilProgressLine('{"files_downloaded": 50, "files_total": 200}')
    ).toBe(25);
  });

  it('parses elapsed and remaining times', () => {
    expect(
      parseMithrilProgressUpdate(
        '{"seconds_elapsed": 120.5, "seconds_left": 42}'
      )
    ).toEqual({ elapsedSeconds: 120.5, remainingSeconds: 42 });
  });
});

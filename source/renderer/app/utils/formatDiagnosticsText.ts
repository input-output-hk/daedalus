export type DiagnosticsTextRow = {
  label: string;
  value: string | number;
};

export type DiagnosticsTextSection = {
  title: string;
  rows: Array<DiagnosticsTextRow>;
};

const FENCE = '```';

/**
 * Formats diagnostics sections as `Label: value` lines inside a fenced
 * `text` block, so the result renders as preformatted text when pasted
 * into a GitHub issue. Sections are separated by a blank line; sections
 * without rows are omitted.
 */
export const formatDiagnosticsText = (
  sections: Array<DiagnosticsTextSection>
): string => {
  const body = sections
    .filter(({ rows }) => rows.length > 0)
    .map(({ title, rows }) =>
      [title, ...rows.map(({ label, value }) => `${label}: ${value}`)].join(
        '\n'
      )
    )
    .join('\n\n');

  return `${FENCE}text\n${body}\n${FENCE}`;
};

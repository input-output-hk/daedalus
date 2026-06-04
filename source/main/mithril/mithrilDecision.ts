export class MithrilDecisionCancelledError extends Error {
  constructor() {
    super('Mithril bootstrap decision was cancelled.');
    this.name = 'MithrilDecisionCancelledError';
  }
}

export const isMithrilDecisionCancelledError = (
  error: unknown
): error is MithrilDecisionCancelledError =>
  error instanceof MithrilDecisionCancelledError;

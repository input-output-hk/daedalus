import type {
  MithrilBootstrapError,
  MithrilBootstrapErrorStage,
  MithrilBootstrapStatusUpdate,
} from '../../common/types/mithril-bootstrap.types';

export class MithrilBootstrapStageError extends Error {
  stage: MithrilBootstrapErrorStage;
  code?: string;

  constructor(
    message: string,
    stage: MithrilBootstrapErrorStage,
    code?: string
  ) {
    super(message);
    this.name = 'MithrilBootstrapStageError';
    this.stage = stage;
    this.code = code;
  }
}

export function buildMithrilError(
  error: unknown,
  fallbackStage?: MithrilBootstrapErrorStage
): MithrilBootstrapError {
  if (error instanceof MithrilBootstrapStageError) {
    return {
      message: error.message,
      code: error.code,
      stage: error.stage,
    };
  }

  if (error instanceof Error) {
    return {
      message: error.message,
      stage: fallbackStage,
    };
  }

  return {
    message: 'Mithril bootstrap failed',
    stage: fallbackStage,
  };
}

export function createStageError(
  stage: MithrilBootstrapErrorStage,
  message: string,
  code?: string
): MithrilBootstrapStageError {
  return new MithrilBootstrapStageError(message, stage, code);
}

export function inferErrorStageFromStatus(
  status: MithrilBootstrapStatusUpdate['status']
): MithrilBootstrapErrorStage | undefined {
  switch (status) {
    case 'downloading':
      return 'download';
    case 'verifying':
      return 'verify';
    case 'unpacking':
    case 'converting':
    case 'finalizing':
      return 'convert';
    default:
      return undefined;
  }
}

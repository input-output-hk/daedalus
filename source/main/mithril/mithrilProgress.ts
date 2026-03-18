export type MithrilProgressUpdate = {
  progress?: number;
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  remainingSeconds?: number;
  label?: 'Files' | 'Ancillary';
  stepNum?: number;
  totalSteps?: number;
  message?: string;
  /** Ancillary stream byte counts — mapped to ancillaryBytesDownloaded/Total on MithrilBootstrapStatusUpdate */
  bytesDownloaded?: number;
  bytesTotal?: number;
};

export const parseMithrilProgressUpdate = (
  line: string
): MithrilProgressUpdate | null => {
  const trimmed = line.trim();
  if (!trimmed.startsWith('{')) return null;

  try {
    const parsed = JSON.parse(trimmed);
    const update: MithrilProgressUpdate = {};

    if (typeof parsed.files_downloaded === 'number') {
      update.filesDownloaded = parsed.files_downloaded;
    }
    if (typeof parsed.files_total === 'number') {
      update.filesTotal = parsed.files_total;
    }

    if (
      update.filesDownloaded != null &&
      update.filesTotal != null &&
      update.filesTotal > 0
    ) {
      update.progress = (update.filesDownloaded / update.filesTotal) * 100;
    }

    const progress = parsed.progress ?? parsed.percentage ?? parsed.percent;
    if (typeof progress === 'number') update.progress = progress;
    if (typeof progress === 'string') {
      const numeric = Number(progress);
      if (!Number.isNaN(numeric)) update.progress = numeric;
    }

    if (typeof parsed.seconds_elapsed === 'number') {
      update.elapsedSeconds = parsed.seconds_elapsed;
    }
    if (typeof parsed.seconds_left === 'number') {
      update.remainingSeconds = parsed.seconds_left;
    }

    if (parsed.label === 'Files' || parsed.label === 'Ancillary') {
      update.label = parsed.label;
    }
    if (typeof parsed.step_num === 'number') {
      update.stepNum = parsed.step_num;
    }
    if (typeof parsed.total_steps === 'number') {
      update.totalSteps = parsed.total_steps;
    }
    if (typeof parsed.message === 'string') {
      update.message = parsed.message;
    }
    if (typeof parsed.bytes_downloaded === 'number') {
      update.bytesDownloaded = parsed.bytes_downloaded;
    }
    if (typeof parsed.bytes_total === 'number') {
      update.bytesTotal = parsed.bytes_total;
    }

    // message and totalSteps alone are supplementary; require at least one signal field
    if (
      update.progress == null &&
      update.filesDownloaded == null &&
      update.filesTotal == null &&
      update.elapsedSeconds == null &&
      update.remainingSeconds == null &&
      update.stepNum == null &&
      update.bytesDownloaded == null &&
      update.bytesTotal == null &&
      update.label == null
    ) {
      return null;
    }

    return update;
  } catch (error) {
    return null;
  }
};

export const parseMithrilProgressLine = (line: string): number | null => {
  const update = parseMithrilProgressUpdate(line);
  if (!update || update.progress == null) return null;
  return update.progress;
};

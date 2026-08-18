/**
 *
 * parseDeviceScaleFactor
 * ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
 * Validates the value of `DAEDALUS_DEVICE_SCALE_FACTOR`, which overrides the
 * device scale factor Chromium detects (from `Xft.dpi` on X11, or from the
 * compositor on Wayland). The value is absolute rather than a multiplier, so
 * e.g. `1.5` on a display Chromium reports as `2.0` scales the UI down.
 *
 * Returns `null` for anything Chromium could not use as a scale factor, so
 * that a malformed value leaves the detected one untouched instead of
 * rendering the window at an unusable size.
 *
 */
export const parseDeviceScaleFactor = (raw?: string): number | null => {
  const scaleFactor = Number(raw);

  if (!Number.isFinite(scaleFactor) || scaleFactor <= 0) {
    return null;
  }

  return scaleFactor;
};

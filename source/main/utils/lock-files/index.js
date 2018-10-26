import lockfile from './lockfile';
import { toPromise, toSync, toSyncOptions } from './adapter';

export async function lock(file, options) {
  const release = await toPromise(lockfile.lock)(file, options);

  return toPromise(release);
}

export function lockSync(file, options) {
  const release = toSync(lockfile.lock)(file, toSyncOptions(options));

  return toSync(release);
}

export function unlock(file, options) {
  return toPromise(lockfile.unlock)(file, options);
}

export function unlockSync(file, options) {
  return toSync(lockfile.unlock)(file, toSyncOptions(options));
}

export function check(file, options) {
  return toPromise(lockfile.check)(file, options);
}

export function checkSync(file, options) {
  return toSync(lockfile.check)(file, toSyncOptions(options));
}

import path from 'path';
import fs from 'graceful-fs';
import retry from 'retry';

const locks = {};

function getLockFile(file) {
  return `${file}.lock`;
}

function resolveCanonicalPath(file, options, callback) {
  if (!options.realpath) {
    return callback(null, path.resolve(file));
  }

  // Use realpath to resolve symlinks
  // It also resolves relative paths
  options.fs.realpath(file, callback);
}

function acquireLock(file, options, callback) {
  // Use mkdir to create the lockfile (atomic operation)
  options.fs.mkdir(getLockFile(file), (err) => {
    // If successful, we are done
    if (!err) {
      return callback();
    }

    // If error is not EEXIST then some other error occurred while locking
    if (err.code !== 'EEXIST') {
      return callback(err);
    }

    // Otherwise, check if lock is stale by analyzing the file mtime
    if (options.stale <= 0) {
      return callback(Object.assign(new Error('Lock file is already being hold'), { code: 'ELOCKED', file }));
    }

    options.fs.stat(getLockFile(file), (err, stat) => {
      if (err) {
        // Retry if the lockfile has been removed (meanwhile)
        // Skip stale check to avoid recursiveness
        if (err.code === 'ENOENT') {
          return acquireLock(file, { ...options, stale: 0 }, callback);
        }

        return callback(err);
      }

      if (!isLockStale(stat, options)) {
        return callback(Object.assign(new Error('Lock file is already being hold'), { code: 'ELOCKED', file }));
      }

      // If it's stale, remove it and try again!
      // Skip stale check to avoid recursiveness
      removeLock(file, options, (err) => {
        if (err) {
          return callback(err);
        }

        acquireLock(file, { ...options, stale: 0 }, callback);
      });
    });
  });
}

function isLockStale(stat, options) {
  return stat.mtime.getTime() < Date.now() - options.stale;
}

function removeLock(file, options, callback) {
  // Remove lockfile, ignoring ENOENT errors
  options.fs.rmdir(getLockFile(file), (err) => {
    if (err && err.code !== 'ENOENT') {
      return callback(err);
    }

    callback();
  });
}

function updateLock(file, options) {
  const lock = locks[file];

  // Just for safety, should never happen
  /* istanbul ignore if */
  if (lock.updateTimeout) {
    return;
  }

  lock.updateDelay = lock.updateDelay || options.update;
  lock.updateTimeout = setTimeout(() => {
    const mtime = Date.now() / 1000;

    lock.updateTimeout = null;

    options.fs.utimes(getLockFile(file), mtime, mtime, (err) => {
      // Ignore if the lock was released
      if (lock.released) {
        return;
      }

      // Verify if we are within the stale threshold
      if (lock.lastUpdate <= Date.now() - options.stale && lock.lastUpdate > Date.now() - (options.stale * 2)) {
        const err = Object.assign(
          new Error(lock.updateError || 'Unable to update lock within the stale threshold'),
          { code: 'ECOMPROMISED' }
        );

        return setLockAsCompromised(file, lock, err);
      }

      // If the file is older than (stale * 2), we assume the clock is moved manually,
      // which we consider a valid case

      // If it failed to update the lockfile, keep trying unless
      // the lockfile was deleted!
      if (err) {
        if (err.code === 'ENOENT') {
          return setLockAsCompromised(file, lock, Object.assign(err, { code: 'ECOMPROMISED' }));
        }

        lock.updateError = err;
        lock.updateDelay = 1000;

        return updateLock(file, options);
      }

      // All ok, keep updating..
      lock.lastUpdate = Date.now();
      lock.updateError = null;
      lock.updateDelay = null;
      updateLock(file, options);
    });
  }, lock.updateDelay);

  // Unref the timer so that the nodejs process can exit freely
  // This is safe because all acquired locks will be automatically released
  // on process exit

  // We first check that `lock.updateTimeout.unref` exists because some users
  // may be using this module outside of NodeJS (e.g., in an electron app),
  // and in those cases `setTimeout` return an integer.
  /* istanbul ignore else */
  if (lock.updateTimeout.unref) {
    lock.updateTimeout.unref();
  }
}

function setLockAsCompromised(file, lock, err) {
  // Signal the lock has been released
  lock.released = true;

  // Cancel lock mtime update
  // Just for safety, at this point updateTimeout should be null
  /* istanbul ignore if */
  if (lock.updateTimeout) {
    clearTimeout(lock.updateTimeout);
  }

  if (locks[file] === lock) {
    delete locks[file];
  }

  lock.options.onCompromised(err);
}

// ----------------------------------------------------------

function lock(file, options, callback) {
  /* istanbul ignore next */
  options = {
    stale: 10000,
    update: null,
    realpath: true,
    retries: 0,
    fs,
    onCompromised: (err) => { throw err; },
    ...options,
  };

  options.retries = options.retries || 0;
  options.retries = typeof options.retries === 'number' ? { retries: options.retries } : options.retries;
  options.stale = Math.max(options.stale || 0, 2000);
  options.update = options.update == null ? options.stale / 2 : options.update || 0;
  options.update = Math.max(Math.min(options.update, options.stale / 2), 1000);

  // Resolve to a canonical file path
  resolveCanonicalPath(file, options, (err, file) => {
    if (err) {
      return callback(err);
    }

    // Attempt to acquire the lock
    const operation = retry.operation(options.retries);

    operation.attempt(() => {
      acquireLock(file, options, (err) => {
        if (operation.retry(err)) {
          return;
        }

        if (err) {
          return callback(operation.mainError());
        }

        // We now own the lock
        const lock = locks[file] = {
          options,
          lastUpdate: Date.now(),
        };

        // We must keep the lock fresh to avoid staleness
        updateLock(file, options);

        callback(null, (releasedCallback) => {
          if (lock.released) {
            return releasedCallback &&
              releasedCallback(Object.assign(new Error('Lock is already released'), { code: 'ERELEASED' }));
          }

          // Not necessary to use realpath twice when unlocking
          unlock(file, { ...options, realpath: false }, releasedCallback);
        });
      });
    });
  });
}

function unlock(file, options, callback) {
  options = {
    fs,
    realpath: true,
    ...options,
  };

  // Resolve to a canonical file path
  resolveCanonicalPath(file, options, (err, file) => {
    if (err) {
      return callback(err);
    }

    // Skip if the lock is not acquired
    const lock = locks[file];

    if (!lock) {
      return callback(Object.assign(new Error('Lock is not acquired/owned by you'), { code: 'ENOTACQUIRED' }));
    }

    lock.updateTimeout && clearTimeout(lock.updateTimeout); // Cancel lock mtime update
    lock.released = true; // Signal the lock has been released
    delete locks[file]; // Delete from locks

    removeLock(file, options, callback);
  });
}

function check(file, options, callback) {
  options = {
    stale: 10000,
    realpath: true,
    fs,
    ...options,
  };

  options.stale = Math.max(options.stale || 0, 2000);

  // Resolve to a canonical file path
  resolveCanonicalPath(file, options, (error, resolvedFile) => {
    if (error) {
      return callback(error);
    }

    // Check if lockfile exists
    options.fs.stat(getLockFile(resolvedFile), (err, stat) => {
      if (err) {
        // If does not exist, file is not locked. Otherwise, callback with error
        return err.code === 'ENOENT' ? callback(null, false) : callback(err);
      }

      // Otherwise, check if lock is stale by analyzing the file mtime
      return callback(null, !isLockStale(stat, options));
    });
  });
}

function getLocks() {
  return locks;
}

// Remove acquired locks on exit
/* istanbul ignore next */
process.on('exit', () => {
  for (const file in locks) {
    try { locks[file].options.fs.rmdirSync(getLockFile(file)); } catch (e) { /* Empty */ }
  }
});

module.exports.lock = lock;
module.exports.unlock = unlock;
module.exports.check = check;
module.exports.getLocks = getLocks;

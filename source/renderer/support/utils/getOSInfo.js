// @flow

// gets 00.00 out of 00.00.00
const regEx = /([0-9])?([0-9])?([0-9])?([0-9])(.)([0-9])?([0-9])?([0-9])?([0-9])/;

// TODO: Get other OS names
const getOsName = (os: string) => {
  if (os === 'macOS') return 'MacOS';
};

export default (os: string, release: string) => {
  const [shortRelease] = release.match(regEx) || [];
  const osName = getOsName(os);
  if (shortRelease && osName) {
    return `${osName} ${shortRelease}`;
  }
};

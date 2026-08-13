const registerExternal = jest.fn();
const registerDirectory = jest.fn();

jest.mock('./open-external-url', () => ({
  registerOpenExternalUrlChannel: registerExternal,
}));
jest.mock('./open-local-directory', () => ({
  registerOpenLocalDirectoryChannel: registerDirectory,
}));

describe('shell IPC registration', () => {
  it('registers process listeners only once', () => {
    const { registerShellIpc } = require('./registerShellIpc');
    expect(registerExternal).not.toHaveBeenCalled();
    expect(registerDirectory).not.toHaveBeenCalled();

    registerShellIpc();
    registerShellIpc();

    expect(registerExternal).toHaveBeenCalledTimes(1);
    expect(registerDirectory).toHaveBeenCalledTimes(1);
  });
});

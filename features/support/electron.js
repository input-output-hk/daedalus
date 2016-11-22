import { Application } from 'spectron';
import electronPath from 'electron';

export default function () {
  this.Before({ timeout: 20 * 1000 }, async function() {
    this.app = new Application({
      path: electronPath,
      args: ['./electron/main.testing'],
      env: {
        HOT: 1,
        NODE_ENV: 'test'
      },
      waitTimeout: 10000
    });
    await this.app.start();
    this.client = this.app.client;
    this.browserWindow = this.app.browserWindow;
  });
  this.After(function() {
    return this.app.stop();
  });
}

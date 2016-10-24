import { Application } from 'spectron';
import electronPath from 'electron';

export default function () {
  this.Before(async function() {
    this.app = new Application({
      path: electronPath,
      args: ['./electron/main.testing'],
      env: {
        HOT: 1,
        NODE_ENV: 'test'
      }
    });
    await this.app.start();
    this.client = this.app.client;
    this.browserWindow = this.app.browserWindow;
  });
  this.After(function() {
    return this.app.stop();
  });
}

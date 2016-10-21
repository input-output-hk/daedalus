import { Application } from 'spectron';
import electronPath from 'electron';

export default function () {
  this.Before(async function() {
    this.app = new Application({
      path: electronPath,
      args: ['.'],
    });
    await this.app.start();
    this.client = this.app.client;
  });
  this.After(function() {
    return this.app.stop();
  });
}

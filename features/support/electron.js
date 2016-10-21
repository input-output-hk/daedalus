import { Application } from 'spectron';
import electronPath from 'electron';

export default function () {
  this.Before(function() {
    this.app = new Application({
      path: electronPath,
      args: ['.'],
    });
    return this.app.start();
  });
  this.After(function() {
    return this.app.stop();
  });
}

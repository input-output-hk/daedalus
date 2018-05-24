// @flow
import { isString } from 'lodash';
import { ipcRenderer } from 'electron';

export class IpcChannel<Request, Response> {
  channel: string;
  constructor(channelName: string) {
    if (isString(channelName) && channelName !== '') {
      this.channel = channelName;
    } else {
      throw new Error(`Invalid channel name ${channelName} provided`);
    }
  }
  async send(request: Request): Promise<Response> {
    return new Promise((resolve, reject) => {
      ipcRenderer.send(this.channel, request);
      ipcRenderer.once(this.channel, (event, isOk: boolean, response: Response) => {
        if (isOk) {
          resolve(response);
        } else {
          reject(response);
        }
      });
    });
  }
}

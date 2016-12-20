// @flow
import { action } from 'mobx';
import BaseController from './BaseController';

export default class SettingsController extends BaseController {

  @action showPage(pageUrlId:string) {
    this.appController.navigateTo(`/settings/${pageUrlId}`);
  }

}

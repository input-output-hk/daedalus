import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { navigateTo } from '../helpers/route-helpers';
import {
  waitUntilWaletNamesEqual,
  getNameOfActiveWalletInSidebar,
} from '../helpers/wallets-helpers';

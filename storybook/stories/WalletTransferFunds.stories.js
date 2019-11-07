// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import faker from 'faker';
import { action } from '@storybook/addon-actions';
import { withKnobs, select } from '@storybook/addon-knobs';
import { BigNumber } from 'bignumber.js';
import { set } from 'lodash';
import StoryDecorator from './support/StoryDecorator';
import TransferFundsStep1Dialog from '../../source/renderer/app/components/wallet/transfer-funds/TransferFundsStep1Dialog';
import TransferFundsStep2Dialog from '../../source/renderer/app/components/wallet/transfer-funds/TransferFundsStep2Dialog';

const wallets = [
  {
    id: '1',
    name: 'Wallet 1',
    amount: new BigNumber(faker.finance.amount()),
  },
  {
    id: '2',
    name: 'Wallet 2',
    amount: new BigNumber(faker.finance.amount()),
  },
];

const addresses = [
  [
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
  ],
  [
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
  ],
];

const walletOptions = wallets.reduce(
  (options, wallet) => ({
    ...options,
    ...set({}, wallet.name, wallet),
  }),
  {}
);
const walletIdOptions = wallets.reduce(
  (options, wallet) => ({
    ...options,
    ...set({}, wallet.name, wallet.id),
  }),
  {}
);
storiesOf('WalletTransferFunds', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('TransferFundsStep1Dialog', () => {
    const sourceWalletSelect = select(
      'sourceWallet',
      walletOptions,
      wallets[1]
    );
    const sourceWallet = {
      ...sourceWalletSelect,
      amount: new BigNumber(sourceWalletSelect.amount),
    };
    return (
      <TransferFundsStep1Dialog
        onClose={action('onClose')}
        onContinue={action('onContinue')}
        onSetSourceWallet={action('onSetSourceWallet')}
        sourceWallet={sourceWallet}
        targetWalletId={select(
          'targetWalletId',
          walletIdOptions,
          wallets[0].id
        )}
        wallets={wallets}
      />
    );
  })
  .add('TransferFundsStep2Dialog', () => {
    const sourceWalletSelect = select(
      'sourceWallet',
      walletOptions,
      wallets[1]
    );
    const targetWalletSelect = select(
      'targetWallet',
      walletOptions,
      wallets[0]
    );
    const sourceWallet = {
      ...sourceWalletSelect,
      amount: new BigNumber(sourceWalletSelect.amount),
    };
    const targetWallet = {
      ...targetWalletSelect,
      amount: new BigNumber(targetWalletSelect.amount),
    };
    const step2Addresses = addresses[parseInt(targetWalletSelect.id, 0) - 1];
    return (
      <TransferFundsStep2Dialog
        onContinue={action('onContinue')}
        onClose={action('onClose')}
        onBack={action('onBack')}
        addresses={step2Addresses}
        fees={12.042481}
        sourceWallet={sourceWallet}
        targetWallet={targetWallet}
      />
    );
  });

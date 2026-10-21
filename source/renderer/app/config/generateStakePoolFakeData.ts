/**
 * It generates stake pool dummy json content
 * Command to run: yarn ts-node source/renderer/app/config/generateStakePoolFakeData.ts
 */
import { faker } from '@faker-js/faker';
import fs from 'fs';
import path from 'path';
import BigNumber from 'bignumber.js';

function generateStakePoolsFakeData() {
  const stakePools = [];

  for (let i = 1; i <= 300; i++) {
    const relativeStake = faker.number.int(100);
    const cost = new BigNumber(faker.number.int(100));
    const createdAt = faker.date.recent();
    const description = faker.lorem.words();
    const homepage = faker.internet.url();
    const id = faker.string.alphanumeric(64);
    const isCharity = faker.datatype.boolean();
    const name = faker.person.fullName();
    const performance = faker.number.int(100);
    const pledge = new BigNumber(faker.number.int(100));
    const producedBlocks = faker.number.int(10000000);
    const profitMargin = faker.number.int(100);
    const ranking = i;
    const retiring = null;
    const saturation = faker.number.float({
      min: 0,
      max: 120,
      fractionDigits: 2,
    });
    const ticker = faker.string.alpha({ length: 4, casing: 'upper' });
    stakePools.push({
      relativeStake,
      cost,
      createdAt,
      description,
      homepage,
      id,
      isCharity,
      name,
      performance,
      pledge,
      producedBlocks,
      profitMargin,
      ranking,
      retiring,
      saturation,
      ticker,
    });
  }

  return stakePools;
}

const fakeStakePools = generateStakePoolsFakeData();
// @TODO - remove flow fix and move fs to main process

/* eslint-disable no-undef */
fs.writeFileSync(
  `${path.join(__dirname, '/')}stakingStakePools.dummy.json`,
  JSON.stringify(fakeStakePools, null, '\t')
);

/**
 * It generates stake pool dummy json content
 * Command to run: yarn ts-node source/renderer/app/config/generateStakePoolFakeData.ts
 */
import faker from '@faker-js/faker';
import fs from 'fs';
import path from 'path';
import BigNumber from 'bignumber.js';

function generateStakePoolsFakeData() {
  const stakePools = [];

  for (let i = 1; i <= 300; i++) {
    const relativeStake = faker.datatype.number(100);
    const cost = new BigNumber(faker.datatype.number(100));
    const createdAt = faker.date.recent();
    const description = faker.lorem.words();
    const homepage = faker.internet.url();
    const id = faker.random.alphaNumeric(64);
    const isCharity = faker.datatype.boolean();
    const name = faker.name.findName();
    const performance = faker.datatype.number(100);
    const pledge = new BigNumber(faker.datatype.number(100));
    const producedBlocks = faker.datatype.number(10000000);
    const profitMargin = faker.datatype.number(100);
    const ranking = i;
    const retiring = null;
    const saturation = faker.datatype.number({
      min: 0,
      max: 120,
      precision: 0.01,
    });
    const ticker = faker.helpers.replaceSymbols('????');
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

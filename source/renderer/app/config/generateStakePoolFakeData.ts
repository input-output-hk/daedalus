/**
 * It generates stake pool dummy json content
 * Command to run: node source/renderer/app/config/generateStakePoolFakeData.js
 */
const faker = require('faker');

// const fs = require('fs');
const path = require('path');

const BigNumber = require('bignumber.js');

function generatStakePoolsFakeData() {
  const stakePools = [];

  for (let i = 1; i <= 300; i++) {
    const relativeStake = faker.random.number(100);
    const cost = new BigNumber(faker.random.number(100));
    const createdAt = faker.date.recent();
    const description = faker.lorem.words();
    const homepage = faker.internet.url();
    const id = faker.random.alphaNumeric(64);
    const isCharity = faker.random.boolean();
    const name = faker.name.findName();
    const performance = faker.random.number(100);
    const pledge = new BigNumber(faker.random.number(100));
    const producedBlocks = faker.random.number(10000000);
    const profitMargin = faker.random.number(100);
    const ranking = i;
    const retiring = null;
    const saturation = faker.random.number({
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

const fakeStakePools = generatStakePoolsFakeData();
// @TODO - remove flow fix and move fs to main process

/* eslint-disable no-undef */
// @ts-ignore
fs.writeFileSync(
  `${path.join(__dirname, '/')}stakingStakePools.dummy.json`,
  JSON.stringify(fakeStakePools, null, '\t')
);

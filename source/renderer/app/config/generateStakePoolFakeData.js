'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
/**
 * It generates stake pool dummy json content
 * Command to run: yarn ts-node source/renderer/app/config/generateStakePoolFakeData.ts
 */
const faker_1 = __importDefault(require('@faker-js/faker'));
const fs_1 = __importDefault(require('fs'));
const path_1 = __importDefault(require('path'));
const bignumber_js_1 = __importDefault(require('bignumber.js'));
function generateStakePoolsFakeData() {
  const stakePools = [];
  for (let i = 1; i <= 300; i++) {
    const relativeStake = faker_1.default.datatype.number(100);
    const cost = new bignumber_js_1.default(
      faker_1.default.datatype.number(100)
    );
    const createdAt = faker_1.default.date.recent();
    const description = faker_1.default.lorem.words();
    const homepage = faker_1.default.internet.url();
    const id = faker_1.default.random.alphaNumeric(64);
    const isCharity = faker_1.default.datatype.boolean();
    const name = faker_1.default.name.findName();
    const performance = faker_1.default.datatype.number(100);
    const pledge = new bignumber_js_1.default(
      faker_1.default.datatype.number(100)
    );
    const producedBlocks = faker_1.default.datatype.number(10000000);
    const profitMargin = faker_1.default.datatype.number(100);
    const ranking = i;
    const retiring = null;
    const saturation = faker_1.default.datatype.number({
      min: 0,
      max: 120,
      precision: 0.01,
    });
    const ticker = faker_1.default.helpers.replaceSymbols('????');
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
fs_1.default.writeFileSync(
  `${path_1.default.join(__dirname, '/')}stakingStakePools.dummy.json`,
  JSON.stringify(fakeStakePools, null, '\t')
);
//# sourceMappingURL=generateStakePoolFakeData.js.map

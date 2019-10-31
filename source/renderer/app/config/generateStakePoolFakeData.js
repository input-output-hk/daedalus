const faker = require('faker');
const fs = require('fs');
const path = require('path');

function generatStakePoolsFakeData() {
  const stakePools = [];
  for (let i = 1; i <= 300; i++) {
    const ranking = i;
    const id = faker.random.alphaNumeric(64);
    const slug = faker.helpers.replaceSymbols('????');
    const name = faker.name.findName();
    const description = faker.lorem.words();
    const url = faker.internet.url();
    const controlledStake = faker.random.number(100);
    const profitMargin = faker.random.number(100);
    const performance = faker.random.number(100);
    const createdAt = faker.date.recent();
    const retiring = null;
    const isCharity = faker.random.boolean();
    stakePools.push({
      ranking,
      id,
      slug,
      name,
      description,
      url,
      controlledStake,
      profitMargin,
      performance,
      createdAt,
      retiring,
      isCharity,
    });
  }
  return stakePools;
}

const fakeStakePools = generatStakePoolsFakeData();
fs.writeFileSync(
  `${path.join(__dirname, '/')}stakingStakePools.dummy.json`,
  JSON.stringify(fakeStakePools, null, '\t')
);

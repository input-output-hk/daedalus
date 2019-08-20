const faker = require('faker');
const fs = require('fs');
const path = require('path');

function generatStakePoolsFakeData() {
  const stakePools = [];
  for (let i=1; i <= 300; i++) {
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
      "ranking": ranking,
      "id": id,
      "slug": slug,
      "name": name,
      "description": description,
      "url": url,
      "controlledStake": controlledStake,
      "profitMargin": profitMargin,
      "performance": performance,
      "created_at": createdAt,
      "retiring": retiring,
      "isCharity": isCharity
    });
  }
  return stakePools;
}

const fakeStakePools = generatStakePoolsFakeData();
fs.writeFileSync(`${path.join(__dirname, '/')}stakingStakePools.dummy.json`, JSON.stringify(fakeStakePools, null, '\t'));

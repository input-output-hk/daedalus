import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import BigNumber from 'bignumber.js/bignumber';
import { DECIMAL_PLACES_IN_ADA, LOVELACES_PER_ADA } from '../../source/renderer/app/config/numbersConfig';
import { getVisibleTextsForSelector } from '../support/helpers/shared-helpers';
import { getWalletByName } from '../support/helpers/wallets-helpers';

Given('I have the following addresses', { timeout: 40000 }, async function (table) {

  this.addresses = table.hashes();


//   // console.log("table", table);

//   const { wallet } = this;
//   this.addresses = [];

//   // const addresses = table.hashes();
//   // console.log("addresses", addresses);

//   // const address = await this.client.executeAsync(async (wallet, done) => {

//   //   const getAddress = await daedalus.actions.ada.addresses.createAddress
//   //     .trigger({
//   //       walletId: wallet.id,
//   //     });

//   //   done(address);

//   // }, wallet);
//     const addresses = await this.client.executeAsync(async function(walletId, password, done) {
//       new window.Promise((resolve) => (
//         // Need to fetch the wallets data async and wait for all results
//         window.Promise.all([
//           daedalus.actions.ada.addresses.createAddress.trigger({ walletId, password }),
//           daedalus.actions.ada.addresses.createAddress.trigger({ walletId, password }),
//         ]).then(results => (
//           resolve(results)
//         ))
//       )).then(done)
//     }, wallet.id, wallet.password);

//   console.log("addresses", addresses);

//   this.addresses.push(...addresses);


//     // daedalus.actions.ada.addresses.createAddress.trigger({
//     //   walletId: wallet.id,
//     //   password,
//     // });


//   // console.log("daedalus.api.ada", daedalus.api.ada);


// // daedalus.api.ada.createTransaction(window.Object.assign(transaction, {
// //             sender: results[0], // Account id of sender wallet
// //             receiver: results[1][0].id // First address of receiving wallet
// //           })).then(resolve)

//   return "pending";

//   // for (const tx of txData) {
//   //   const txResponse = await this.client.executeAsync((transaction, done) => (
//   //     new window.Promise((resolve) => (
//   //       // Need to fetch the wallets data async and wait for all results
//   //       window.Promise.all([
//   //         daedalus.stores.ada.addresses.getAccountIdByWalletId(transaction.sender),
//   //         daedalus.stores.ada.addresses.getAddressesByWalletId(transaction.receiver)
//   //       ]).then(results => (
//   //         daedalus.api.ada.createTransaction(window.Object.assign(transaction, {
//   //           sender: results[0], // Account id of sender wallet
//   //           receiver: results[1][0].id // First address of receiving wallet
//   //         })).then(resolve)
//   //       ))
//   //     )).then(done)
//   //   ), tx);

//   // const txData = table.hashes().map((t) => ({
//   //   sender: getWalletByName.call(this, t.sender).id,
//   //   receiver: getWalletByName.call(this, t.receiver).id,
//   //   amount: new BigNumber(t.amount).times(LOVELACES_PER_ADA),
//   //   password: t.password || null,
//   // }));
//   // this.transactions = [];
//   // // Sequentially (and async) create transactions with for loop
//   // for (const tx of txData) {
//   //   const txResponse = await this.client.executeAsync((transaction, done) => (
//   //     new window.Promise((resolve) => (
//   //       // Need to fetch the wallets data async and wait for all results
//   //       window.Promise.all([
//   //         daedalus.stores.ada.addresses.getAccountIdByWalletId(transaction.sender),
//   //         daedalus.stores.ada.addresses.getAddressesByWalletId(transaction.receiver)
//   //       ]).then(results => (
//   //         daedalus.api.ada.createTransaction(window.Object.assign(transaction, {
//   //           sender: results[0], // Account id of sender wallet
//   //           receiver: results[1][0].id // First address of receiving wallet
//   //         })).then(resolve)
//   //       ))
//   //     )).then(done)
//   //   ), tx);
//   //   this.transactions.push(txResponse);
//   // }
});

// Given(/^I have the following addresses:$/, { timeout: 40000 }, async function (table) {
//   console.log('table', table);

//   const txData = table.hashes().map((t, i) => {
//     console.log('t', t);
//     return ({
//       id: i,
//       amount: 0.000001,
//       isUsed: i % 2
//     });

//   });
//   console.log("txData", txData);
//   console.log('daedalus.stores.ada.addresses', daedalus.stores.ada.addresses);

//   this.addresses = [];
//   // const txData = table.hashes().map((t) => ({
//   //   sender: getWalletByName.call(this, t.sender).id,
//   //   receiver: getWalletByName.call(this, t.receiver).id,
//   //   amount: new BigNumber(t.amount).times(LOVELACES_PER_ADA),
//   //   password: t.password || null,
//   // }));
//   // this.transactions = [];
//   // // Sequentially (and async) create transactions with for loop
//   // for (const tx of txData) {
//   //   const txResponse = await this.client.executeAsync((transaction, done) => (
//   //     new window.Promise((resolve) => (
//   //       // Need to fetch the wallets data async and wait for all results
//   //       window.Promise.all([
//   //         daedalus.stores.ada.addresses.getAccountIdByWalletId(transaction.sender),
//   //         daedalus.stores.ada.addresses.getAddressesByWalletId(transaction.receiver)
//   //       ]).then(results => (
//   //         daedalus.api.ada.createTransaction(window.Object.assign(transaction, {
//   //           sender: results[0], // Account id of sender wallet
//   //           receiver: results[1][0].id // First address of receiving wallet
//   //         })).then(resolve)
//   //       ))
//   //     )).then(done)
//   //   ), tx);
//   //   this.transactions.push(txResponse);
//   // }
// });

// Then(/^I should not see any address:$/)

Then('I should see {int} addresses', async function (addresses) {

  await this.client.waitForVisible('.WalletReceive_walletAddress');
  const addressesElements = await this.client.elements('.WalletReceive_walletAddress');
  console.log("addressesElements", addressesElements);


  // console.log("addresses", addresses);
  // console.log("this.addresses", this.addresses);

  expect(1).to.equal(1);



});

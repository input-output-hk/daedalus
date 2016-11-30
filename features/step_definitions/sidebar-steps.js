import { expect } from 'chai';

export default function () {
  this.Given(/^The sidebar shows the (.*) category$/, async function (category) {
    await this.client.execute(function(route) {
      daedalus.state.sidebar.route = `/${route}`;
    }, category);
    await this.client.waitForVisible(`.SidebarCategory_active.${category}`);
  });

  this.When(/^I click on the (.*) category in the sidebar$/, async function (category) {
    await this.client.click(`.SidebarCategory_component.${category}`);
  });

  this.Then(/^The (.*) category should be active$/, async function (category) {
    await this.client.waitForVisible(`.SidebarCategory_active.${category}`);
  });
}

const DATA_LAYER_MIGRATION_COMPONENT = '.DataLayerMigrationForm_component';

const dataLayerMigration = {
  waitForVisible: async (client, { isHidden } = {}) => (
    client.waitForVisible(DATA_LAYER_MIGRATION_COMPONENT, null, isHidden)
  ),
  startMigration: async (client) => {
    await dataLayerMigration.waitForVisible(client);
    await client.execute(() => {
      daedalus.actions.profile.startDataLayerMigration.trigger();
    });
    await dataLayerMigration.waitForVisible(client, { isHidden: true });
  }
};

export default dataLayerMigration;

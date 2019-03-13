const DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT =
  '.DataLayerMigrationForm_component';

const dataLayerMigration = {
  waitForVisible: async (client, { isHidden } = {}) =>
    client.waitForVisible(
      DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT,
      null,
      isHidden
    ),
  acceptMigration: async client => {
    await client.execute(() => {
      daedalus.actions.profile.acceptDataLayerMigration.trigger();
    });
    await dataLayerMigration.waitForVisible(client, { isHidden: true });
  },
};

export default dataLayerMigration;

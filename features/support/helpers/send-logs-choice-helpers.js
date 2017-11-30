const SEND_LOGS_CHOICE_FORM = '.SendLogsChoiceForm_component';

const sendLogsChoice = {
  waitForVisible: async (client, { isHidden } = {}) => (
    client.waitForVisible(SEND_LOGS_CHOICE_FORM, null, isHidden)
  ),
  agree: async (client) => {
    await sendLogsChoice.waitForVisible(client);
    await client.execute(() => {
      daedalus.actions.profile.setSendLogsChoice.trigger({ sendLogs: true });
    });
    await sendLogsChoice.waitForVisible(client, { isHidden: true });
  }
};

export default sendLogsChoice;

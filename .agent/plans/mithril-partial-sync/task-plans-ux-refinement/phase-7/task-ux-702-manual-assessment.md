# task-ux-702 - Manual Assessment

## Manual Captured Issues

### Daedalus Diagnostics - Mithril Partial Sync not available in the Diagnostics Page on first load

- When choosing genesis sync on the first application load the app begins syncing, via the slow Blockchain Cardano Node sync process.
- However, if the user changes their mind and navigates to the Daedalus Diagnostics page, the Mithril Partial Sync option is not visible and available.
- On app reload the option for Mithril Partial Sync is available in the Diagnostics page.
- It should be available on the first load of the application if the user chooses to use Mithril instead of the slow Blockchain Cardano Node sync process.

Side question: 
- Why is "Last Network Block" not listed/available? At what point does it become available? This also affect other dialogues in the Mithril Partial Sync flow like the proactive prompt and confirmation prompt.

### Daedalus Diagnostics - Mithril Partial Sync copy is too much for users on Daedalus Diagnostics page

- The copy for Mithril Partial Sync on the Daedalus Diagnostics page is too long to be visible in the way it is presented currently.
- Put the Mithril Partial Sync copy into a tooltip style like how the Daedalus state directory tooltip is presented. The tooltip should show when the user hovers over the Mithril Sync button.
- "If Cardano node catch-up is taking longer than you want, Mithril Sync can restore verified chain data to help it catch up faster." Update this copy to "If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync."
- Remove the copy "Review what will happen before Daedalus starts Mithril Sync."

### Mithril Partial Sync Dialogues - "Before Mithril Sync Begins" confirmation body is not UI friendly

- The copy is too verbose and not user friendly. It should be simplified to be more concise and clear.
- It should have the first sentence, "Your node is behind", in normal subject text.
- Then followed by "Mithril Sync will restore verified chain data to help your node sync faster." in the same subject text style and in the same paragraph section.
- The copy below it needs to be spaced appropriately and in a subtext style. Use the following copy: "For this process to begin your Cardano node will need to be shutdown. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks."
- Remove all other body copy.
- Also confirm that the text style is consistent with the rest of the Daedalus application, specifically the initial mithril sync dialogues.


### Mithril Partial Sync Dialogues - Theme colors seem off on dialogues compared to other dialogues in Daedalus

 - Need to evaluate the theme colors for the dialogues.
 - Confirm that the global text import and usage is also integrated correctly. 
 - Use the mithril-bootstrap and chain-storage styling as a guide.
 - Need to confirm that the all the theme colors files have been updated to keep consistent with the rest of the Daedalus application.
 - Use grill skill to identify/compare theme colors.

### Mithril Partial Sync Dialogues - Canceled session dialogue needs to be updated

- Verify the theme colors on the canceled session dialogue. It seems to be off compared to other dialogues in Daedalus.
- The subtext should be normal body subject text.
- The button copy should only be "Retry Mithril Sync (fast)" or "Restart Node Sync (slow)"
- There should be no third option on this screen to wipe all chain data.

Side Questions: 
- Also need to verify which side the action button goes on the dialogue. It should be consistent with other dialogues in Daedalus.
- Preferably the action button should be on the right side of the dialogue. This might need to be updated on the original initial Mithril Sync process.

### Mithril Partial Sync Dialogues - Initial proactive prompt Mithril Partial Sync dialogue UX needs to be updated

- the Mithril Partial Sync dialogue should not appear on the Cardano Node Sync page during the checks and verifications of the blockchain. Show only when the verifications are complete, on the summary wallet page load.
- The Mithril Partial Sync dialogue should still remain on the screen until the user clicks on either "Standard Sync (slow)" or "Mithril Sync (fast)".
- The Mithril Sync (fast) button should be the default action button on the dialogue, highlighted like the other primary action buttons in the Daedalus application.
- Mithril sync should be Mithril Sync in the subtitle text.
- The Mithril Sync (fast) button is selected and the confirmation prompt appears. The copy needs to be updated to "For this process to begin your Cardano node will need to be shutdown. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks."

### Mithril Partial Sync Dialogues - Completion dialogue should be consistent with Mithril Bootstrap flow

- The Mithril Partial Sync success, transition to Cardano Node startup and Wallet Summary page transition should follow the same flow as the normal Mithril Bootstrap flow.
- Flow should still be: Mithril Sync Finalizing -> Mithril Sync Completed -> Timeout started for transition with visual loading animation -> Cardano Node startup -> Wallet Summary page transition.
- On Mithril Sync Partial Flow there is a Completed Dialogue with a button that says "Continue to Daedalus". This dialogue should be removed so the transition is only the timeout and Cardano Node to start up before transitioning to the home Wallet Summary page.
- The mithril-sync folder should still be deleted on the final storage transfer and Mithril Sync completion.

### Storybook - Issues with themes and colors

- As with what is seen in the Daedalus Mithril Partial Sync Dialogues issues, the theme colors in Storybook, and the app, for Mithril Partial Sync dialogues need to be have contrast, be user friendly, and consistent with the rest of the application and theme variables.
- Text color and background color need to be evaluated and updated to not blend into each other.
- The Daedalus theme has an established protocol for implementing the theme colors using the theme variables, consistency should be kept across the existing colors.

### Storybook - Only text showing for some of the dialogue views

- Storybook is showing views of only text blocks.
- Views of the specific UI or dialogue should be used instead of blocks of text.

### Storybook - Include a Mithril Partial Sync download progress bar view

- Storybook must have a view of the Mithril Partial Sync download progress bar in a partial progress state.

### Storybook - Update Status section to Diagnostic section

- The Status section in Storybook should be renamed to Diagnostic to be consistent with the Daedalus Diagnostics page.
- Move the Mithril Partial Sync Dialogue views, except for the Partial Sync Confirmation, to their own Mithril Partial Sync Dialogue section under the Mithril overarching section in Storybook.

Side Question:
 - Why are Mithril and Status section icons different from other areas in Storybook?

import { useLayoutEffect } from 'react';
import { observer } from 'mobx-react';
import { boolean } from '@storybook/addon-knobs';
import { useDiscreetModeFeature } from '../../../source/renderer/app/features';

export const DiscreetModeNotificationKnob = observer(() => {
  const feature = useDiscreetModeFeature();
  const tooltipKnob = boolean('Discreet mode tooltip', false, 'Dicreet Mode');
  const notificationKnob = boolean(
    'Discreet mode notification',
    false,
    'Dicreet Mode'
  );
  useLayoutEffect(() => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'boolean' is not assignable to pa... Remove this comment to see the full error message
    const knobBoolValue = JSON.parse(notificationKnob);

    if (knobBoolValue !== feature.isNotificationEnabled) {
      feature.setDiscreetModeNotification(knobBoolValue);
    }
  }, [notificationKnob, feature.isNotificationEnabled]);
  useLayoutEffect(() => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'boolean' is not assignable to pa... Remove this comment to see the full error message
    const knobBoolValue = JSON.parse(tooltipKnob);

    if (knobBoolValue !== feature.isSettingsTooltipEnabled) {
      feature.setDiscreetModeSettingsTooltip(knobBoolValue);
    }
  }, [tooltipKnob, feature.isSettingsTooltipEnabled]);
  return null;
});

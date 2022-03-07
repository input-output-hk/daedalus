import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import {
  withKnobs,
  button,
  boolean,
  text,
  number,
} from '@storybook/addon-knobs';
import { withState } from '@dump247/storybook-state';
import StoryDecorator from '../_support/StoryDecorator';
import Notification from '../../../source/renderer/app/components/notifications/Notification';
import InlineNotification from '../../../source/renderer/app/components/notifications/InlineNotification';
import { NOTIFICATION_DEFAULT_DURATION } from '../../../source/renderer/app/config/timingConfig';

storiesOf('Common|Notifications', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs) // ====== Stories ======
  .add(
    'General',
    withState(
      {
        isVisible: false,
      },
      (store) => {
        const keepVisible = boolean('Keep notification visible', false);
        const { isVisible } = store.state;
        let clickToClose;
        let onClose = action('onClose');

        if (!keepVisible) {
          let timeout;
          const duration = number(
            'Duration (seconds)',
            NOTIFICATION_DEFAULT_DURATION / 1000
          );

          const showNotification = () => {
            clearTimeout(timeout);
            store.set({
              isVisible: true,
            });
            timeout = setTimeout(() => {
              store.set({
                isVisible: false,
              });
            }, duration * 1000);
            return false;
          };

          button('Trigger notification', showNotification);
          clickToClose = boolean('clickToClose', true);

          onClose = () => {
            store.set({
              isVisible: false,
            });
            action('onClose');
          };
        }

        const hasCloseButton = boolean('hasCloseButton', true);
        return (
          <>
            <Notification
              isVisible={isVisible || keepVisible}
              onClose={onClose}
              clickToClose={clickToClose}
              hasCloseButton={hasCloseButton}
            >
              {text('Content', 'Notification content')}
            </Notification>
            <h2
              style={{
                lineHeight: 1.38,
                margin: '20px 0 10px',
                opacity: 0.5,
                paddingLeft: '20px',
                fontFamily:
                  '"NotoSans-Regular, NotoSansCJKjp-Regular", sans-serif',
              }}
            >
              Use the knob button to trigger the notification
            </h2>
          </>
        );
      }
    )
  )
  .add(
    'With actions',
    withState(
      {
        isVisible: false,
      },
      (store) => {
        const keepVisible = boolean('Keep notification visible', true);
        const { isVisible } = store.state;
        let clickToClose;
        let onClose = action('onClose');

        if (!keepVisible) {
          let timeout;
          const duration = number(
            'Duration (seconds)',
            NOTIFICATION_DEFAULT_DURATION / 1000
          );

          const showNotification = () => {
            clearTimeout(timeout);
            store.set({
              isVisible: true,
            });
            timeout = setTimeout(() => {
              store.set({
                isVisible: false,
              });
            }, duration * 1000);
            return false;
          };

          button('Trigger notification', showNotification);
          clickToClose = boolean('clickToClose', true);

          onClose = () => {
            store.set({
              isVisible: false,
            });
            action('onClose');
          };
        }

        const actions = [
          {
            label: text('Secondary label', 'Secondary'),
          },
          {
            label: text('Primary label', 'Primary'),
            primary: true,
          },
        ];
        const hasCloseButton = boolean('hasCloseButton', true);
        return (
          <>
            <Notification
              isVisible={isVisible || keepVisible}
              onClose={onClose}
              clickToClose={clickToClose}
              hasCloseButton={hasCloseButton}
              actions={actions}
            >
              {text('Content', 'Notification content')}
            </Notification>
            <h2
              style={{
                lineHeight: 1.38,
                margin: '20px 0 10px',
                opacity: 0.5,
                paddingLeft: '20px',
                fontFamily:
                  '"NotoSans-Regular, NotoSansCJKjp-Regular", sans-serif',
              }}
            >
              Use the knob button to trigger the notification
            </h2>
          </>
        );
      }
    )
  )
  .add('Inline', () => (
    <div
      style={{
        position: 'relative',
        padding: 40,
      }}
    >
      <InlineNotification show>
        {text('Content', 'Inline notification content')}
      </InlineNotification>
    </div>
  ));

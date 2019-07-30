// @flow
import React, { Component } from 'react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import classnames from 'classnames';
import type { DialogAction } from '../../widgets/Dialog';

type Props = {
  actions: Array<DialogAction>,
  primaryButtonAutoFocus?: boolean,
};

class WalletCreateHeader extends Component<Props> {
  render() {
    const { actions, primaryButtonAutoFocus } = this.props;

    return (
      <div>
        {actions.map((action: DialogAction) => {
          const buttonClasses = classnames([
            action.className ? action.className : null,
            action.primary ? 'primary' : 'flat',
          ]);
          return (
            <Button
              key={action.label}
              className={buttonClasses}
              label={action.label}
              onClick={action.onClick}
              disabled={action.disabled}
              skin={ButtonSkin}
              autoFocus={action.primary ? primaryButtonAutoFocus : false}
            />
          );
        })}
      </div>
    );
  }
}

export default WalletCreateHeader;

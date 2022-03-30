import React from 'react';
import { PopOver, PopOverProps } from 'react-polymorph/lib/components/PopOver';

const topLevelZIndex = 9999;

const TopLevelPopOver = ({ children, ...props }: PopOverProps) => (
    <PopOver
        zIndex={topLevelZIndex}
        {...props}
    >
        {children}
    </PopOver>
)

export default TopLevelPopOver;

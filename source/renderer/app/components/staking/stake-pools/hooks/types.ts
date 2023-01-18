import StakePool from '../../../../domains/StakePool';

export type StakePoolSortableProps = keyof Pick<
  StakePool,
  | 'ranking'
  | 'ticker'
  | 'saturation'
  | 'cost'
  | 'profitMargin'
  | 'producedBlocks'
  | 'nonMyopicMemberRewards'
  | 'pledge'
  | 'retiring'
>;

// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { observable } from 'mobx';
import Staking from '../../components/staking/Staking';
import StakingChart from '../../components/staking/StakingChart';
import Layout from '../Layout';

const generateRandomSlots = (count:number) => {
  const slots = [];
  for (let i = 0; i < count; i += 1) {
    const numberOfTransactions = i < (count / 2) ? Math.floor(Math.random() * 50) : 0;
    slots.push({
      numberOfTransactions,
      slot: slots.length + 1,
      shares: 'CC',
      openings: 'BB',
      commitments: 'AA',
      mpcPhase: 'Shares',
      hash: 'ad9f37d14e189f5d792aaf524a6e0a13cdc5ba13b540f231638444687526231e',
      time: new Date()
    });
  }
  return slots;
};

@observer(['state', 'controller'])
export default class StakingPage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      settings: PropTypes.shape({
        profile: MobxPropTypes.observableObject.isRequired
      }).isRequired,
      login: PropTypes.shape({
        isLoading: PropTypes.bool.isRequired
      }).isRequired
    }).isRequired,
  };

  render() {
    const { isLoading } = this.props.state.login;
    if (isLoading) return <div>Loading</div>;
    const options = observable({
      data: generateRandomSlots(30),
      ticks: [0, 10, 20, 30, 40, 50],
      activeIndex: null
    });
    return (
      <Layout>
        <div style={{ height: '100%' }}>
          <Staking>
            <StakingChart width={500} height={200} options={options} />
          </Staking>
        </div>
      </Layout>
    );
  }

}

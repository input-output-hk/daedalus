import { Before } from 'cucumber';

// Add context object to share data between steps
Before(function() {
  this.context = {};
});

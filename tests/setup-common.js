import { After, Before } from 'cucumber';
import sinon from "sinon";

// Add context object to share data between steps
Before(function() {
  this.context = {};
});

After(function() {
  sinon.restore();
});

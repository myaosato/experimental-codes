'use strict';

const makeOnceOnly = (proc) => {
  let ret;
  return function (...args) {
    ret = proc.call(this, ...args);
    proc = () => ret;
    return ret;
  }
}

const f = () => {console.log('done!');return 42;};
const g = makeOnceOnly(f);
console.log(g());
console.log(g());
console.log(g());
const h = makeOnceOnly(f);
console.log(h());
console.log(h());

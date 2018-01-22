"use strict";

var seedrandom = typeof module !== "undefined" && module.require
      ? module.require('seedrandom')
      : Math.seedrandom
      ;

// https://stackoverflow.com/a/6274381
exports.shuffle=function(seed){
  return function(list) {
    var rng = seedrandom(seed);
    var a = list.slice();
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
      j = Math.floor(rng() * (i + 1));
      x = a[i];
      a[i] = a[j];
      a[j] = x;
    }
    return a;
  };
};

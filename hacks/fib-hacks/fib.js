// -*- mode: js; js-indent-level: 2; -*-

"use strict"

function fib(n) {
  return (n < 2) ? 1 : (fib(n-1) + fib(n-2));
}

// Code requires microtime module; see
//   https://github.com/wadey/node-microtime

var tm = require('microtime');
function timeit(thunk) {
  var start = tm.nowDouble();
  var result = thunk();
  var finis = tm.nowDouble();
  var delta = finis - start;
  return [result, delta]
}

function add_left_padding(x, len, pad) {
  var str = String(x);
  return (str.length >= len) ? str : Array(len-str.length+1).join(pad||' ')+str;
}

function add_right_padding(x, len, pad) {
  var str = String(x);
  return (str.length >= len) ? str : str+Array(len-str.length+1).join(pad||' ');
}

function report(expr, result, delta) {
  var dsecs = Math.floor(delta);
  var dmicro = (delta % 1);
  var dsuffix = add_right_padding(String(dmicro).substring(2).substring(0,2),
                                  2, '0');
  console.log(expr + ": " + add_left_padding(result, 10) +
              " elapsed: " + dsecs + "." + dsuffix);
}

function main() {
  var rd1 = timeit(function () { return fib(10); });
  var rd2 = timeit(function () { return fib(20); });
  var rd3 = timeit(function () { return fib(30); });
  var rd4 = timeit(function () { return fib(40); });
  var rd5 = timeit(function () { return fib(41); });
  var rd6 = timeit(function () { return fib(42); });
  var rd7 = timeit(function () { return fib(43); });
  var rd8 = timeit(function () { return fib(44); });
  report("fib(10)", rd1[0], rd1[1]);
  report("fib(20)", rd2[0], rd2[1]);
  report("fib(30)", rd3[0], rd3[1]);
  report("fib(40)", rd4[0], rd4[1]);
  report("fib(41)", rd5[0], rd5[1]);
  report("fib(42)", rd6[0], rd6[1]);
  report("fib(43)", rd7[0], rd7[1]);
  report("fib(44)", rd8[0], rd8[1]);
}

main()

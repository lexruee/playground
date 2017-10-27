#!/usr/bin/env node

let fib = function(f1, f2) {
    let fn = f1 + f2;
    return {
        'current': fn,
        'next': function() {
            return fib(f2, fn);
        }
    }
};

let take = function(num, cons) {
    let values = [];
    let iter = cons;
    for (let i = 0; i < num; i++) {
        let current = iter.current;
        values.push(current);
        iter = iter.next();
    }
    return values;
};

console.log(take(10, fib(1,1)));

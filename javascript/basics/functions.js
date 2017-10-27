#!/usr/bin/env node

function myadd(a, b) {
    return a + b; 
}

console.log('myadd(1, -1) = ' + myadd(1, -1));

let id = function(x) {
    return function() { return x };
};

let add = function(a, b) {
    return function() { return a() + b() };
};

let mul = function(a, b) {
    return function() { return a() * b() };
};

let sub = function(a, b) {
    return function() { return a() - b() };
};

let calc = sub(mul(add(id(1),id(2)), id(3)), id(1));
console.log('sub(mult(add(id(1),id(2)), id(3)), id(1)) = ' + calc());

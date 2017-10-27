#!/usr/bin/env node

let dict = {1: 'a', 2: 'b'};

console.log('loop1');
for (let key in dict) {
    console.log(key);
    let value = dict[key];
    console.log(value);
}

console.log('loop2');
Object.keys(dict).forEach(function(key) {
    console.log(key);
});

console.log('loop3');
Object.values(dict).forEach(function(value) {
    console.log(value);
});

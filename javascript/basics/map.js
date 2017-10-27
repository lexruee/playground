#!/usr/bin/env node

let map = new Map([
    [1, 'a'],
    [2, 'b']
]);

console.log('loop1');
map.forEach(function(value, key) {
    console.log(key);
    console.log(value);
});

console.log('loop2');
for (let [key, value] of map) {
    console.log(key);
    console.log(value);
}


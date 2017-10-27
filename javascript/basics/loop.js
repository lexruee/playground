#!/usr/bin/env node

let values = ['a', 'b', 'c'];

console.log('loop1');
for (let i = 0; i < values.length; i++) {
    let item = values[i];
    console.log(item);
}

console.log('loop2');
for (let value of values) {
    console.log(value);
}

console.log('loop3');
values.forEach(function (item) {
    console.log(item);
});

let dict = {
    name: 'Alex',
    location: 'Bern'
};

console.log('loop4');
for (let key in dict) {
    console.log(key);
}   

console.log('loop5');
Object.keys(dict).forEach(function(key) {
    console.log(key);
});

console.log('loop6');
Object.values(dict).forEach(function(value) {
    console.log(value);
});

let map = new Map([
    ['name', 'Alex'],
    ['location', 'Bern']
]);

console.log('loop7');
for (let [key, value] of map) {
    console.log(key + ' - ' + value);
}

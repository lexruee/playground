#!/usr/bin/env node

let alex = {
    _name: 'Alex',
    _location: 'Bern',

    getName: function() {
        return this._name;
    },

    setName: function(value) {
        this._name = value;
    },

    getLocation: function() {
        return this._location;
    },

    setLocation: function(value) {
        this._location = value;
    },

    toString: function() {
        return '{ name: ' + this._name + ', location: ' + this._location + ' }';
    }
};

console.log(alex.getName());
console.log(alex.getLocation());
console.log(alex.toString());

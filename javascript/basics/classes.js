#!/usr/bin/env node

function Person(name, location) {
    this._name = name;
    this._location = location;
}
Person.prototype.getName = function() { 
    return this._name;
};
Person.prototype.getLocation = function() {
    return this._location;
};
Person.prototype.toString = function() {
    return `{ name: ${this._name}, location: ${this._location}`;
};

alex = new Person('Alex', 'Bern');
console.log(alex.getName());
console.log(alex.getLocation());
console.log(alex.toString());

function Student(name, location, subject) {
    Person.call(this, name, location);
    this._subject = subject;
}
Student.prototype = Object.create(Person.prototype);
Student.prototype.getSubject = function() {
    return this._subject;
};
Student.prototype.setSubject = function(value) {
    this._subject = value;
};
Student.prototype.toString = function() {
    return `{ name: ${this._name}, location: ${this._location}, subject: ${this._subject} }`;
};

let student = new Student('Peter', 'Zurich', 'CS');
console.log(student.getName());
console.log(student.getLocation());
console.log(student.toString());



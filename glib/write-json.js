#!/usr/bin/env gjs

const GLib = imports.gi.GLib;

let map = {
    temperature: 20.1,
    humidity: 39.1
};
let data = JSON.stringify(map, null, '\t');
GLib.file_set_contents('new-data.json', data);

#!/usr/bin/env gjs

const GLib = imports.gi.GLib;

let [ok, contents] = GLib.file_get_contents('data.json');
if (ok) {
    let map = JSON.parse(contents);
    log(map['temperature']);
    log(map['humidity']);
}



#!/usr/bin/env gjs

const Clutter = imports.gi.Clutter;

Clutter.init(null);

let stage = new Clutter.Stage({
    title: 'Hello World!',
    color: new Clutter.Color({
        red: 0,
        blue: 0,
        green: 0,
        alpha: 255
    }),
    user_resizable: true
});
stage.set_size(800, 600);
stage.connect('destroy', Clutter.main_quit);

let text = new Clutter.Text({ 
    font_name: 'Sans 16', 
    text: 'Hello World!',
    color: new Clutter.Color({
        red: 255,
        blue: 255,
        green: 255,
        alpha: 255
    })
});

text.set_position(100,100);

stage.add_actor(text);
stage.show();
Clutter.main();

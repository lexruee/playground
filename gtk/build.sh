#!/bin/sh
gcc `pkg-config --cflags gtk+-3.0` -o window window.c `pkg-config --libs gtk+-3.0`

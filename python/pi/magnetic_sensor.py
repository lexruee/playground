#encoding: utf-8
"""
@author     Alexander RÃ¼edlinger <a.rueedlimger@gmail.com>
@date       06.03.2015

Simple program to detect transition of a door from closed state to open state 
and vice versa using a magnetic sensor.

Magnetic sensor: http://www.adafruit.com/product/375

"""
import RPi.GPIO as GPIO
import time
GPIO.setmode(GPIO.BOARD)

pin = 11

GPIO.setup(pin, GPIO.IN)

open = False # use as default value false

# if we have a HIGH on pin 11 set open to true
if GPIO.input(pin):
        open = True

while True:
        # on transition from HIGH/LOW to HIGH/HIGH print open
        if GPIO.input(pin) and not open:
                print("open!")
                open = True

        # on transition from LOW/HIGH to LOW/LOW print closed
        if not GPIO.input(pin) and open:
                print("closed")
                open = False

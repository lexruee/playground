import time
import RPi.GPIO as GPIO
GPIO.setmode(GPIO.BOARD)

pin  = 11

GPIO.setup(pin, GPIO.OUT)

time.sleep(1)

GPIO.output(pin, 1)

time.sleep(2)

GPIO.output(pin, 0)

GPIO.cleanup()

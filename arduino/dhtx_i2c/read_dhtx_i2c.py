"""
@author     Alexande Rüedlinger <a.rueedlinger.gmail>
@date       14.03.2015

This program (I2C master) reads temperature and humidity data
from an arduino (I2C slave) which is connected to a DHT22 sensor.
"""

import smbus
import time

def word2data(data):
    msb = data & 0x00FF
    lsb = data & 0xFF00
    lsb = lsb >> 8

    sign = msb & 0x80
    if sign:
        msb -= 256
        lsb *= -1

    value = msb*100 + lsb
    return value/100.0


def main():
    i2c_address = 0x04 # i2c address of the arduino
    bus = smbus.SMBus(1)


    for x in range(0,100):
        try:
            # read temperature value
            temperature_word = bus.read_word_data(i2c_address, 0xB0)
            time.sleep(0.3)

            # read humidity value
            humidity_word = bus.read_word_data(i2c_address, 0xA0)

            temperature = word2data(temperature_word)
            humidity = word2data(humidity_word)
            print "t: %0.2f, h: %0.2f" % (temperature, humidity)
            time.sleep(2)
        except IOError:
            pass



if __name__ == '__main__':
    main()

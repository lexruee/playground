# encoding: utf
"""
@author     Alexande Rüedlinger <a.rueedlinger.gmail>
@date       15.03.2015

This program (I2C master) reads humidity data
from an arduino (I2C slave) which is connected to a soil moisture sensor.
"""

import smbus
import time

def word2data(word):
    """
    Converts a word (2 bytes) to a value with two digits of precision.
    """
    # I2C kernel functions assume little endian
    # but arm uses big endian.
    # Therefore we need to read a stream of bytes in the reverse order.
    msb = word & 0x00FF # extract msb byte
    lsb = word & 0xFF00 # extract lsb byte
    lsb = lsb >> 8      # eliminate lower bits

    sign = msb & 0x80   # extract sign bit
    if sign: # if negative
        msb -= 256 # subtract max uint_t value which is 256
        lsb *= -1  # make the fractial part also negative for latter addition

    value = msb*100 + lsb  # combine integer part and fractial part to together

    # Divide by 100 because we received a float value encoded
    # as a integer using float * 100.
    return value/100.0


def main():
    i2c_address = 0x05 # i2c address of the arduino
    bus = smbus.SMBus(1)


    for x in range(0,10):
       try:
            # read humidity value
            humidity_word = bus.read_word_data(i2c_address, 0xB0)
            humidity = word2data(humidity_word)
            print "h: %0.2f" % (humidity)
            time.sleep(2)
       except IOError:
           print "error"


if __name__ == '__main__':
    main()


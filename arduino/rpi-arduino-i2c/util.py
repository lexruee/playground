#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# xwot-py - Python tools for the extended Web of Things
# Copyright (C) 2016  Alexander Rüedlinger
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>

__author__ = 'Alexander Rüedlinger'

import smbus
import io
import fcntl


class SMBusAdapter(object):

    def __init__(self, addr=0x04, bus=1):
        self._addr = addr
        self.smbus = smbus.SMBus(bus)

    @property
    def bus(self):
        return self.smbus

    @property
    def addr(self):
        return self._addr

    def write_quick(self):
        self.smbus.write_quick(self._addr)

    def write_byte_data(self, reg, val):
        try:
            self.smbus.write_byte_data(self._addr, reg, val)
            return True
        except IOError:
            return False

    def write_byte(self, val):
        try:
            self.smbus.write_byte(self._addr, val)
            return True
        except IOError:
            return False

    def write_word_data(self, reg, val):
        try:
            self.smbus.write_word_data(self._addr, reg, val)
            return True
        except IOError:
            return False

    def read_byte_data(self, reg):
        try:
            return self.smbus.read_byte_data(self._addr, reg)
        except IOError:
            return None

    def read_word_data(self, reg):
        try:
            return self.smbus.read_word_data(self._addr, reg)
        except IOError:
            return None

    def read_i2c_block_data(self, reg, num):
        try:
            data = self.smbus.read_i2c_block_data(self._addr, reg, num)
            return data
        except IOError:
            return None

    def read_block_data(self, reg):
        try:
            data = self.smbus.read_block_data(self._addr, reg)
            return data
        except IOError:
            return None

    def write_i2c_block_data(self, cmd, vals):
        try:
            data = self.smbus.write_i2c_block_data(self._addr, cmd, vals)
            return data
        except IOError:
            return None

    def write_block_data(self, cmd, vals):
        try:
            data = self.smbus.write_block_data(self._addr, cmd, vals)
            return data
        except IOError:
            return None


class MockSMBusAdapter(SMBusAdapter):

    def __init__(self, addr=0x04, bus=1):
        self._addr = addr
        self.smbus = None

    @property
    def bus(self):
        return self.smbus

    @property
    def i2c_addr(self):
        return self._addr

    def write_quick(self):
        return None

    def write_byte_data(self, reg, val):
        return None

    def write_byte(self, val):
        return None

    def write_word_data(self, reg, val):
        return None

    def read_byte_data(self, reg):
        return None

    def read_word_data(self, reg):
        return None

    def read_i2c_block_data(self, reg, num):
        return None

    def read_block_data(self, reg):
        return None

    def write_i2c_block_data(self, cmd, vals):
        return None

    def write_block_data(self, cmd, vals):
        return None


class I2CAdapter(object):

    I2C_SLAVE = 0x0703

    def __init__(self, addr, bus):
        self._addr = addr
        self._bus_n = bus
        self._fr = io.open("/dev/i2c-%s" % str(self._bus_n), "rb", buffering=0)
        self._fw = io.open("/dev/i2c-%s" % str(self._bus_n), "wb", buffering=0)
        fcntl.ioctl(self._fr, self.I2C_SLAVE, self._addr)
        fcntl.ioctl(self._fw, self.I2C_SLAVE, self._addr)

    def _write_bytes(self, data):
        chars = [chr(i) for i in data]
        data_str = ''.join(chars)
        self._fw.write(data_str)

    def write_bytes(self, data):
        self._write_bytes(data)

    def write_byte(self, b):
        self._write_bytes([b])

    def write_word(self, w):
        msb = (w & 0xff00) >> 8
        lsb = w & 0x00ff
        self._write_bytes([msb, lsb])

    def write(self, b):
        self._fw.write(b)

    def _read_bytes(self, num):
        return [ord(b) for b in self._fr.read(num)]

    def read_bytes(self, num):
        return self._read_bytes(num)

    def read_byte(self):
        _bytes = self._read_bytes(1)
        return _bytes[0]

    def read_word(self):
        _bytes = self._read_bytes(2)
        return (_bytes[0] << 8) | _bytes[1]

    def read(self, n):
        return self._fr.read(n)

    def _close(self):
        self._fr.close()
        self._fw.close()

    def close(self):
        self._close()

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._close()

    def __enter__(self):
        pass

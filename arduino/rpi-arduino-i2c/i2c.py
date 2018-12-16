#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# xwot-devices - Models for the smart devices.
# Copyright (C) 2015  Alexander Rüedlinger
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

import struct
import smbus


class SMBusAdapter(object):

    def __init__(self, bus=1, i2c_addr=0x04):
        self._bus = smbus.SMBus(bus)
        self._i2c_addr = i2c_addr

    @property
    def bus(self):
        return self._bus

    @property
    def i2c_addr(self):
        return self._i2c_addr

    def _to_int(self, data):
        """
        Assumes that data is in little endian format
        """
        b = ''.join(chr(i) for i in data)
        r = struct.unpack('<i', b)
        return r[0]

    def _to_uint(self, data):
        """
        Assumes that data is in little endian format
        """
        b = ''.join(chr(i) for i in data)
        r = struct.unpack('<I', b)
        return r[0]

    def _to_short(self, data):
        """
        Assumes that data is in little endian format
        """
        b = ''.join(chr(i) for i in data)
        r = struct.unpack('<h', b)
        return r[0]

    def _to_ushort(self, data):
        """
        Assumes that data is in little endian format
        """
        b = ''.join(chr(i) for i in data)
        r = struct.unpack('<H', b)
        return r[0]

    def _to_float(self, data):
        """
        Assumes that data is in little endian format
        """
        b = ''.join(chr(i) for i in data)
        r = struct.unpack('<f', b)
        return r[0]

    def _to_double(self, data):
        """
        Assumes that data is in little endian format
        """
        b = ''.join(chr(i) for i in data)
        r = struct.unpack('<d', b)
        return r[0]

    def write_byte(self, cmd):
        try:
            self._bus.write_byte(self._i2c_addr, cmd)
            return True
        except IOError:
            return False

    def write_word(self, cmd, val):
        try:
            self._bus.write_word_data(self._i2c_addr, cmd, val)
            return True
        except IOError:
            return False

    def write_i2c_block_data(self, cmd, vals):
        try:
            return self._bus.write_i2c_block_data(self._i2c_addr, cmd, vals)
        except IOError:
            return None

    def read_byte(self, cmd):
        try:
            return self._bus.read_byte_data(self._i2c_addr, cmd)
        except IOError:
            return None

    def read_i2c_block_data(self, cmd, byte_count):
        try:
            return self._bus.read_i2c_block_data(self._i2c_addr, cmd, byte_count)
        except IOError:
            return None

    def read_int32(self, cmd):
        data = self.read_i2c_block_data(cmd, 4)
        if data is not None:
            return self._to_int(data)
        else:
            return None

    def read_uint32(self, cmd):
        data = self.read_i2c_block_data(cmd, 4)
        if data is not None:
            return self._to_uint(data)
        else:
            return None

    def read_int16(self, cmd):
        data = self.read_i2c_block_data(cmd, 2)
        if data is not None:
            return self._to_short(data)
        else:
            return None

    def read_uint16(self, cmd):
        data = self.read_i2c_block_data(cmd, 2)
        if data is not None:
            return self._to_ushort(data)
        else:
            return None

    def read_float(self, cmd):
        data = self.read_i2c_block_data(cmd, 4)
        if data is not None:
            return self._to_float(data)
        else:
            return None

    def read_double(self, cmd):
        data = self.read_i2c_block_data(cmd, 8)
        if data is not None:
            return self._to_double(data)
        else:
            return None


class Driver():

    CMD_RESET = 0x01
    CMD_CLOSE = 0x02
    CMD_OPEN = 0x03
    CMD_UNLOCK = 0x04
    CMD_LOCK = 0x05
    
    CMD_OCUL_STATE = 0x06
    CMD_OC_STATE = 0x07
    CMD_UL_STATE = 0x08
    

    def __init__(self):
        self._bus = SMBusAdapter()

    def reset(self):
        self._bus.write_byte(self.CMD_RESET)

    def open(self):
        self._bus.write_byte(self.CMD_OPEN)

    def close(self):
        self._bus.write_byte(self.CMD_CLOSE)

    def lock(self):
        self._bus.write_byte(self.CMD_LOCK)

    def unlock(self):
        self._bus.write_byte(self.CMD_UNLOCK)

import time

d = smbus.SMBus(1)

while True:
	r = d.read_i2c_block_data(0x04, 0xA1, 4)
	print(r)
	if r:
		b = ''.join(chr(i) for i in r)
		r = struct.unpack('<f', b)
		print(r)
		time.sleep(2)
	else:
		print("error")

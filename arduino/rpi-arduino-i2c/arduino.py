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

#from __future__ import absolute_import

__author__ = 'Alexander Rüedlinger'

__all__ = ['ArduinoDoor', 'ArduinoShutter', 'ArduinoWindow']

import logging
from twisted.internet import reactor, task
from util import SMBusAdapter, MockSMBusAdapter
import struct




def to_bool_or_none(value):
    if value == 1:
        return True
    elif value == 0:
        return False
    return None


def to_byte_string(data):
    return ''.join(chr(i) for i in data)


class ArduinoListener(object):

    def sync(self, data):
        raise NotImplementedError


class I2CArduino(object):

    def __init__(self, addr, bus, mock=False):
        if mock:
            self._smbus_adapter = MockSMBusAdapter(addr, bus)
        else:
            self._smbus_adapter = SMBusAdapter(addr, bus)

    def get_state(self):
        raise {}

    def sync(self):
        pass

    @property
    def state(self):
        return {}

    @property
    def addr(self):
        return self._smbus_adapter.addr

    @property
    def bus(self):
        return self._smbus_adapter.smbus

    @property
    def smbus(self):
        return self._smbus_adapter


class ArduinoDoor(I2CArduino):
    CMD_RESET = 0x01
    CMD_CLOSE = 0x02
    CMD_OPEN = 0x03
    CMD_UNLOCK = 0x04
    CMD_LOCK = 0x05
    CMD_STOP = 0x06
    CMD_OCUL_STATE = 0xA0
    CMD_OC_STATE = 0xA1
    CMD_UL_STATE = 0xA2

    def __init__(self, addr=0x04, bus=1, mock=False):
        super(ArduinoDoor, self).__init__(addr, bus, mock)
        self._keys = ['closed', 'locked']
        self._state = dict(zip(self._keys, [None] * len(self._keys)))

    @property
    def state(self):
        return self._state

    def lock(self):
        self._smbus_adapter.write_byte(self.CMD_LOCK)

    def unlock(self):
        self._smbus_adapter.write_byte(self.CMD_UNLOCK)

    def open(self):
        self._smbus_adapter.write_byte(self.CMD_OPEN)

    def close(self):
        self._smbus_adapter.write_byte(self.CMD_CLOSE)

    def stop(self):
        self._smbus_adapter.write_byte(self.CMD_STOP)

    def reset(self):
        self._smbus_adapter.write_byte(self.CMD_RESET)

    def is_locked(self):
        val = self._smbus_adapter.read_byte_data(self.CMD_UL_STATE)
        val = to_bool_or_none(val)
        self._state['locked'] = val
        return val is True

    def is_unlocked(self):
        val = self.smbus.read_byte_data(self.CMD_UL_STATE)
        val = to_bool_or_none(val)
        self._state['locked'] = val
        return val is False

    def is_open(self):
        val = self.smbus.read_byte_data(self.CMD_OC_STATE)
        val = to_bool_or_none(val)
        self._state['closed'] = val
        return val is True

    def is_closed(self):
        val = self._smbus_adapter.read_byte_data(self.CMD_OC_STATE)
        val = to_bool_or_none(val)
        self._state['closed'] = val
        return val is False

    def get_state(self):
        self.sync()
        return self._state

    def sync(self):
        data = self._smbus_adapter.read_i2c_block_data(self.CMD_OCUL_STATE, 2)
        if data:
            _state = dict(zip(self._keys, map(to_bool_or_none, data)))
            self._state.update(_state)


class ArduinoWindow(I2CArduino):
    CMD_RESET = 0x01
    CMD_CLOSE = 0x02
    CMD_OPEN = 0x03
    CMD_UNLOCK = 0x04
    CMD_LOCK = 0x05
    CMD_OCUL_STATE = 0xA0
    CMD_OC_STATE = 0xA1
    CMD_UL_STATE = 0xA2

    def __init__(self, addr=0x04, bus=1, mock=False):
        super(ArduinoWindow, self).__init__(addr, bus, mock)
        self._keys = ['closed', 'locked']
        self._state = dict(zip(self._keys, [None] * len(self._keys)))

    @property
    def state(self):
        return self._state

    def lock(self):
        self._smbus_adapter.write_byte(self.CMD_LOCK)

    def unlock(self):
        self._smbus_adapter.write_byte(self.CMD_UNLOCK)

    def open(self):
        self.smbus.write_byte(self.CMD_OPEN)

    def close(self):
        self._smbus_adapter.write_byte(self.CMD_CLOSE)

    def reset(self):
        self._smbus_adapter.write_byte(self.CMD_RESET)

    def is_locked(self):
        val = self._smbus_adapter.read_byte_data(self.CMD_UL_STATE)
        val = to_bool_or_none(val)
        self._state['locked'] = val
        return val is True

    def is_unlocked(self):
        val = self._smbus_adapter.read_byte_data(self.CMD_UL_STATE)
        val = to_bool_or_none(val)
        self._state['locked'] = val
        return val is False

    def is_open(self):
        val = self._smbus_adapter.read_byte_data(self.CMD_OC_STATE)
        val = to_bool_or_none(val)
        self._state['closed'] = val
        return val is True

    def is_closed(self):
        val = self._smbus_adapter.read_byte_data(self.CMD_OC_STATE)
        val = to_bool_or_none(val)
        self._state['closed'] = val
        return val is False

    def get_state(self):
        self.sync()
        return self._state

    def sync(self):
        data = self._smbus_adapter.read_i2c_block_data(self.CMD_OCUL_STATE, 2)
        if data:
            _state = dict(zip(self._keys, map(to_bool_or_none, data)))
            self._state.update(_state)


class ArduinoShutter(I2CArduino):
    CMD_RESET = 0x01
    CMD_CLOSE = 0x02
    CMD_OPEN = 0x03
    CMD_STOP = 0x04
    CMD_OC_STATE = 0xA0

    def __init__(self, addr=0x04, bus=1, mock=False):
        super(ArduinoShutter, self).__init__(addr, bus, mock)
        self._keys = ['closed']
        self._state = dict(zip(self._keys, [None] * len(self._keys)))

    @property
    def state(self):
        return self._state

    def open(self):
        self.smbus.write_byte(self.CMD_OPEN)

    def close(self):
        self.smbus.write_byte(self.CMD_CLOSE)

    def reset(self):
        self.smbus.write_byte(self.CMD_RESET)

    def stop(self):
        self.smbus.write_byte(self.CMD_STOP)

    def is_open(self):
        val = self._smbus_adapter.read_byte_data(self.CMD_OC_STATE)
        val = to_bool_or_none(val)
        self._state['closed'] = val
        return val is True

    def is_closed(self):
        val = self._smbus_adapter.read_byte_data(self.CMD_OC_STATE)
        val = to_bool_or_none(val)
        self._state['closed'] = val
        return val is False

    def get_state(self):
        self.sync()
        return self._state

    def sync(self):
        data = self._smbus_adapter.read_i2c_block_data(self.CMD_OC_STATE, 1)
        if data:
            _state = dict(zip(self._keys, map(to_bool_or_none, data)))
            self._state.update(_state)


class ArduinoWeatherStation(I2CArduino):
    CMD_RESET = 0x01
    CMD_SENSOR_DATA = 0xA0
    CMD_TEMPERATURE = 0xA1
    CMD_HUMIDITY = 0xA2
    CMD_PRESSURE = 0xA3
    CMD_LIGHT = 0xA4

    def __init__(self, addr=0x04, bus=1, mock=False):
        super(ArduinoWeatherStation, self).__init__(addr, bus, mock)
        self._keys = ['temperature', 'humidity', 'pressure', 'light']
        self._state = dict(zip(self._keys, [None] * len(self._keys)))

    @property
    def state(self):
        return self._state

    def get_temperature(self):
        data = self._smbus_adapter.read_i2c_block_data(self.CMD_TEMPERATURE, 4)
        if data:
            val = struct.unpack('<f', to_byte_string(data))[0]
            self._state['temperature'] = val
        return self._state['temperature']

    def get_humidity(self):
        data = self._smbus_adapter.read_i2c_block_data(self.CMD_HUMIDITY, 4)
        if data:
            val = struct.unpack('<f', to_byte_string(data))[0]
            self._state['humidity'] = val
        return self._state['humidity']

    def get_pressure(self):
        data = self._smbus_adapter.read_i2c_block_data(self.CMD_PRESSURE, 4)
        if data:
            val = struct.unpack('<f', to_byte_string(data))[0]
            self._state['pressure'] = val
        return self._state['pressure']

    def get_light(self):
        data = self._smbus_adapter.read_i2c_block_data(self.CMD_LIGHT, 4)
        if data:
            val = struct.unpack('<f', to_byte_string(data))[0]
            self._state['light'] = val
        return self._state['light']

    def get_state(self):
        self.sync()
        return self._state

    def sync(self):
        data = self._smbus_adapter.read_i2c_block_data(self.CMD_SENSOR_DATA, 4*4)
        if data:
            r = struct.unpack('<ffff', to_byte_string(data))
            _state = dict(zip(self._keys, r))
            self._state.update(_state)

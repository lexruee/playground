__author__ = 'lexruee'

import serial
from serial.serialutil import SerialException
from twisted.internet import reactor, task
import logging

logger = logging.getLogger("arduino")


import json


class ArduinoListener(object):

    def update(self, arduino):
        pass


class Arduino(object):

    def __init__(self, port='/dev/ttyACM0', baudrate='9600', interval=2, serial=None, debug=False):
        self._port = port
        self._baudrate = baudrate
        self._serial = serial
        self._listeners = []
        self.state = {}
        self._interval = interval

        d = task.LoopingCall(self._loop)
        d.start(self._interval)

        if debug:
            logging.basicConfig(level=logging.DEBUG)

    def _connect(self):
        try:
            self._serial = serial.Serial(port=self._port, baudrate=self._baudrate, timeout=0)
        except SerialException, e:
            logger.debug(e)

    def _loop(self):
        if self._serial:
            try:
                data = self._serial.readline()
                self._handle_data(data)
            except SerialException, e:
                logger.debug(e)
                self._connect()
        else:
            self._connect()

    def _handle_data(self, data):
        if(len(data)):
            print(data)

    def update(self, data):
        if self._serial:
            self._serial.write(data)

    def run(self):
        if not reactor.running:
            reactor.run()

    def _notify_listeners(self):
        for listener in self._listeners:
            listener.update(self)

    def stop(self):
        if reactor.running:
            try:
                reactor.stop()
            except:
                pass

    def attach(self, listener):
        if listener not in self._listeners:
            self._listeners.append(listener)

    def detach(self, listener):
        if listener in self._listeners:
            self._listeners.remove(listener)



arduino = Arduino(debug=True, port='/dev/ttyAMA0')
reactor.callLater(5, arduino.update, chr(0x01))
#reactor.callLater(2, arduino.update, chr(0x05))
#reactor.callLater(2, arduino.update, chr(0x05))
#reactor.callLater(2, arduino.update, chr(0x05))
arduino.run()

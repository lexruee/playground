#!/usr/bin/env python
import socket
import asyncio
import time

class ServerProtocol(asyncio.DatagramProtocol):

    def __init__(self, server):
        super().__init__()
        self._server = server
        self.transport = None

    def connection_made(self, transport):
        self.transport = transport

    def datagram_received(self, data, addr):
        self._server.datagram_received(data, addr)

    def error_received(self, exc):
        pass

    def connection_lost(self, exc):
        pass


class Server(object):

    MDNS_UDP_PORT = 5353
    MDNS_MULTICAST_ADDR_IPv4 = '224.0.0.251'
    MDNS_MULTICAST_ADDR_IPv6 = 'FF02::FB'

    def __init__(self, loop):
        self._loop = loop
        self._hosts = [];

        # create sockets 
        ipv4_socket = self.create_socket_ipv4()
        ipv6_socket = self.create_socket_ipv6()
       
        # create datagram endpoints
        self._listen_ipv4 = loop.create_datagram_endpoint(lambda: ServerProtocol(self), sock=ipv4_socket)
        self._transport_ipv4, self._protocol_ipv4 = loop.run_until_complete(self._listen_ipv4)

        self._listen_ipv6 = loop.create_datagram_endpoint(lambda: ServerProtocol(self), sock=ipv6_socket)
        self._transport_ipv6, self._protocol_ipv6 = loop.run_until_complete(self._listen_ipv6)

        self._loop.call_later(5, self._print_task)

    @classmethod
    def create_socket_ipv4(cls, interface='0.0.0.0'):
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(('', cls.MDNS_UDP_PORT))
        group = socket.inet_aton(cls.MDNS_MULTICAST_ADDR_IPv4)
        mreq = group + socket.inet_aton(interface)
        sock.setsockopt(socket.SOL_IP, socket.IP_ADD_MEMBERSHIP, mreq)
        return sock

    @classmethod
    def create_socket_ipv6(cls, interface='0.0.0.0'):
        sock = socket.socket(socket.AF_INET6, socket.SOCK_DGRAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(('', cls.MDNS_UDP_PORT))
        group = socket.inet_pton(socket.AF_INET6, cls.MDNS_MULTICAST_ADDR_IPv6)
        mreq = group + socket.inet_aton(interface)
        sock.setsockopt(socket.IPPROTO_IPV6, socket.IPV6_JOIN_GROUP, mreq)
        return sock

    def datagram_received(self, data, addr):
        ip, port = addr[0], addr[1]
        print('Datagram received')
        print('ip: {}, port: {}'.format(ip, port))
        print('')
        if ip not in self._hosts:
            self._hosts.append(ip)

    def _print_task(self):
        self._loop.call_later(5, self._print_task)
        print('Time: {}'.format(time.strftime('%Y-%m-%d, %H:%M:%S')))
        print('Hosts: {}'.format(str(self._hosts)))
        print('')


# get an event loop
loop = asyncio.get_event_loop()

# create server
server = Server(loop)

try:
    print('Run event loop')
    loop.run_forever()
except KeyboardInterrupt:
    print('Event loop stopped')

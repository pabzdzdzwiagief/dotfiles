#!/usr/bin/env python3

from atexit import register
from logging import basicConfig, info, INFO
from json import loads, dumps
from os import unlink, killpg, getpid, getpgid, getuid, setpgid
from signal import SIGTERM
from socket import socket, AF_UNIX, SOCK_SEQPACKET, SOL_SOCKET, SO_PEERCRED
from struct import calcsize, unpack
from subprocess import Popen
from sys import argv

MESSAGE_BUFFER_SIZE = 1024*1024

def serve(socket_path):
        setpgid(0, 0)
        register(lambda: cleanup(socket_path))
        with socket(AF_UNIX, SOCK_SEQPACKET) as commands:
                info("Cleaning up old socket if exists")
                cleanup_socket(socket_path)
                info("Opening socket")
                commands.bind(socket_path)
                commands.listen(1)
                info("Awaiting commands")
                read_commands(commands)

def start(socket_path, *command_line):
        send_command(socket_path, {"type": "start", "to_start": command_line})

def stop(socket_path):
        send_command(socket_path, {"type": "stop"})

def send_command(socket_path, command):
        encoded_address = socket_path.encode('utf-8')
        encoded_command = [dumps(command).encode('utf-8')]
        with socket(AF_UNIX, SOCK_SEQPACKET) as commands:
                commands.connect(encoded_address)
                commands.sendmsg(encoded_command)

def read_commands(commands):
        while True:
                client, _ = commands.accept()
                credentials = client.getsockopt(SOL_SOCKET, SO_PEERCRED, calcsize('3i'))
                _, uid, _ = unpack('3i', credentials)
                info("A new command from: {}".format(uid))
                if uid != getuid():
                        info("Rejected as {} is not the current user".format(uid))
                        continue
                command, _, _, _ = client.recvmsg(MESSAGE_BUFFER_SIZE)
                decoded = command.decode('utf-8')
                info("Command: {}".format(decoded))
                from_json = loads(decoded)
                execute_command(from_json)

def execute_command(command):
        command_type = command['type']
        if command_type == 'start':
                Popen(command['to_start'])
        elif command_type == 'stop':
                exit(0)

def cleanup(socket_path):
        info("Closing socket")
        cleanup_socket(socket_path)
        info("Terminating processes")
        killpg(getpgid(getpid()), SIGTERM)

def cleanup_socket(socket_path):
        try:
                unlink(socket_path)
        except IOError:
                pass

if __name__ == '__main__':
        basicConfig(level=INFO)
        _, operation, *args = argv
        operations = {
                'serve': serve,
                'start': start,
                'stop':  stop
        }
        operations[operation](*args)

#! /usr/bin/env python3

import socket
import struct
import sys
import logging

if len(sys.argv) != 2:
	print("Please provide destination IP or hostname as commandline argument")
	sys.exit(2)
addr = sys.argv[1]
ff = "{:+1.4f}"
uff = "{:1.4f}"

logging.basicConfig(filename='remote.log', level=logging.DEBUG)

def parseData(data):
	#d float, h signed int
	packstring = (
		'!'+'d'*6+ #gr and ac
		'h'*3+ #ma
		'd'*6+ #ro and la
		'h'+ #temp
		'd'*21+ #eu height, motors, Y, fps, Int, Pitch 
		'?' #emergency
	)
	unp = struct.unpack(packstring, data)
	ret = dict(zip(
		[
			'grX', 'grY', 'grZ',
			'acX', 'acY', 'acZ',
			'maX', 'maY', 'maZ',
			'roX', 'roY', 'roZ',
			'laX', 'laY', 'laZ',
			'temp',
			'euHeading', 'euRoll', 'euPitch',
			'sonarHeight',
			'motorA', 'motorB', 'motorC', 'motorD', 'motorE', 'motorF',
			'YYaw', 'YRollX', 'YRollY',
			'fps',
			'IntYaw', 'IntRollX', 'IntRollY',
			'Pitch', 'Yaw', 'Rollx', 'Rolly',
			'emergency'
		],
			unp))
	return ret

def packJs(axis):
	#pitch, yaw, rollx, rolly, emergency
	dat = struct.pack("!dddd?", -axis[3], axis[2],
				-axis[1], axis[0], axis[4])
	return dat

class Connector:
	def __init__(self):
		self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	
	def connect(self, port):
		try:
			self.s.connect((addr, port))
			return True
		except(socket.error, socket.timeout):
			return False
	
	def askForData(self):
		self.s.sendall(b'GET')
	
	def pushData(self, data):
		self.s.sendall(b'JOYSTICK'+data)
		
	def getData(self):
		rawdata = self.s.recv(4096)
		return rawdata


class DummyConnector:
	def __init__(self):
		pass
		#self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	
	def connect(self):
		return True
		#self.s.connect((addr, port))
	
	def askForData(self):
		pass
		#self.s.sendall(b'GET')
		
	def getData(self):
		#rawdata = self.s.recv(4096)
		#data = parseData(rawData)
		data = {'grX': 5,
			'grY': 5,
			'grZ': 5,
			'maX': 100,
			'maY': -24,
			'maZ': 300,
			'motorA': 0.5,
			'motorB': 0.55,
			'motorC': 0.4,
			'motorD': 0.5,
			'motorE': 0.7,
			'motorF': 0.3,
			'YPitch': -0.005,
			'YYaw':   0.01,
			'YRollX': -0.03,
			'YRollY': 0.02,
			'fps': 24.3
		}
		return data
	def pushData(self, data):
		pass

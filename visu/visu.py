#! /usr/bin/env python3

import socket
import struct
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from itertools import product, combinations
import matplotlib.animation as anim

from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d import proj3d


addr = "10.155.50.250"
port = 12000

# Visualize lin Acc Direction + Length
# Visualize Rotation, each axis, maybe isometric image with 'arrows' on each axis
# visualize motor duties %float
# visualize Yis %float

def parseData(data):
	# Format: All small endian, 16bit signed int integers
	# see imu.hrl
	
	# 3dim Vectors in this order: gravity, accel, magnet, rotation, lin_accel, euler
	# Int: temp
	# TODO: Log bad format
	packstring = 'h'*19+'f'*10
	print(len(data))
	unp = struct.unpack(packstring, data)
	ret = {'grX': unp[0],
		'grY': unp[1],
		'grZ': unp[2],
		'acX': unp[3],
		'acY': unp[4],
		'acZ': unp[5],
		'maX': unp[6],
		'maY': unp[7],
		'maZ': unp[8],
		'roX': unp[9],
		'roY': unp[10],
		'roZ': unp[11],
		'laX': unp[12],
		'laY': unp[13],
		'laZ': unp[14],
		'temp': unp[15],
		'euHeading': unp[16],
		'euRoll': unp[17],
		'euPitch': unp[18],
		'motorA': unp[19],
		'motorB': unp[20],
		'motorC': unp[21],
		'motorD': unp[22],
		'motorE': unp[23],
		'motorF': unp[24],
		'YPitch': unp[25],
		'YYaw': unp[26],
		'YRollX': unp[27],
		'YRollY': unp[28]
	}
	return ret

# TODO: Timeout, error catching

class Arrow3D(FancyArrowPatch):

    def __init__(self, xs, ys, zs, *args, **kwargs):
        FancyArrowPatch.__init__(self, (0, 0), (0, 0), *args, **kwargs)
        self._verts3d = xs, ys, zs

    def draw(self, renderer):
        xs3d, ys3d, zs3d = self._verts3d
        xs, ys, zs = proj3d.proj_transform(xs3d, ys3d, zs3d, renderer.M)
        self.set_positions((xs[0], ys[0]), (xs[1], ys[1]))
        FancyArrowPatch.draw(self, renderer)


class Connector:
	def __init__(self):
		self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	
	def connect(self):
		self.s.connect((addr, port))
	
	def askForData(self):
		self.s.sendall(b'GET')
		
	def getData(self):
		rawdata = self.s.recv(4096)
		data = parseData(rawdata)
		return data

class DummyConnector:
	def __init__(self):
		pass
		#self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	
	def connect(self):
		pass
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
			'YRollY': 0.02
		}
		return data

mycon = Connector()
mycon.connect()


fig = plt.figure(dpi=100, figsize=(5,10))
#ax = fig.add_subplot(121, projection='3d')
#motorsab = fig.add_subplot(322, title='Motors a b')
#motorscd = fig.add_subplot(323, title='Motors c d')
#motorsef = fig.add_subplot(324, title='Motors e f')

ax = plt.subplot2grid((7,2), (0,0), rowspan=3,
	projection='3d')
motorsab = plt.subplot2grid((7,2), (0,1), title='Motors a b') 
motorscd = plt.subplot2grid((7,2), (1,1), title='Motors c d')
motorsef = plt.subplot2grid((7,2), (2,1), title='Motors e f')
yi = plt.subplot2grid((7,2), (3,0), colspan=2, title="Controller Output")
mag = plt.subplot2grid((7,2), (4,0), rowspan=3, colspan=2, title='Magnet Vector', projection='3d')


plt.ion()
plt.show()

while True:	
	# draw cube
	r = [-1, 1]
	for s, e in combinations(np.array(list(product(r, r, r))), 2):
	    if np.sum(np.abs(s-e)) == r[1]-r[0]:
	        ax.plot3D(*zip(s, e), color="b")
	
	# draw sphere
	u, v = np.mgrid[0:2*np.pi:20j, 0:np.pi:10j]
	x = np.cos(u)*np.sin(v)
	y = np.sin(u)*np.sin(v)
	z = np.cos(v)
	ax.plot_wireframe(x, y, z, color="r")
	
	# draw a point
	ax.scatter([0], [0], [0], color="g", s=100)
	mycon.askForData()
	parsed = mycon.getData()
	grX = parsed['grX']
	grY = parsed['grY']
	grZ = parsed['grZ']
	print({'gravity',  grX, grY, grZ },)
	norm = np.linalg.norm([grX, grY, grZ])
	a = Arrow3D([0, grX/norm], [0, grY/norm], [0, grZ/norm], mutation_scale=20,
	            lw=1, arrowstyle="-|>", color="k")
	ax.add_artist(a)
	
	ax.set_xlim([-1, 1])
	ax.set_ylim([-1, 1])
	ax.set_zlim([-1, 1])
	ax.set_xlabel('x')
	ax.set_ylabel('y')
	ax.set_zlabel('z')
	ax.set_title("Gravity Vector")
	ax.legend()
	
	motorsab.bar([0,1], [parsed['motorA'], parsed['motorF']],
		width=0.3, tick_label=['a', 'f'])
	motorsab.set_ylim([0,1])
	motorsab.set_title('Motors a f')
	
	motorscd.bar([0,1], [parsed['motorB'], parsed['motorE']],
		width=0.3, tick_label=['b', 'e'])
	motorscd.set_ylim([0,1])
	motorscd.set_title('Motors b e')
	
	motorsef.bar([0,1], [parsed['motorC'], parsed['motorD']],
		width=0.3, tick_label=['c', 'd'])
	motorsef.set_ylim([0,1])
	motorsef.set_title('Motors c d')
	
	
	mag.set_title('Magnet Vector')
	# draw cube
	r = [-1, 1]
	for s, e in combinations(np.array(list(product(r, r, r))), 2):
	    if np.sum(np.abs(s-e)) == r[1]-r[0]:
	        mag.plot3D(*zip(s, e), color="b")
	
	
	# draw sphere
	u, v = np.mgrid[0:2*np.pi:20j, 0:np.pi:10j]
	x = np.cos(u)*np.sin(v)
	y = np.sin(u)*np.sin(v)
	z = np.cos(v)
	mag.plot_wireframe(x, y, z, color="r")
	
	# draw a point
	mag.scatter([0], [0], [0], color="g", s=100)
	
	#draw arrow
	gnorm = np.linalg.norm([parsed['maX'], parsed['maY'], parsed['maZ']])
	marrow = Arrow3D([0, parsed['maX']/gnorm], [0, parsed['maY']/gnorm], [0, parsed['maZ']/gnorm], mutation_scale=20,
	            lw=1, arrowstyle="-|>", color="k")
	mag.add_artist(marrow)
	mag.set_xlim([-1,1])
	mag.set_ylim([-1,1])
	mag.set_zlim([-1,1])
	mag.set_xlabel('x')
	mag.set_ylabel('y')
	mag.set_zlabel('z')
	mag.legend()
	
	yi.bar([0,1,2,3], [parsed['YPitch'], parsed['YYaw'],
		parsed['YRollX'], parsed['YRollY']], width=0.3,
		tick_label=['Pitch', 'Yaw', 'Roll X', 'Roll Y'])
	yi.set_ylim([-0.1, 0.1])
	yi.set_title('Controller Output')
	yi.grid(color='b')
	
	plt.tight_layout()
	plt.pause(0.001)
	ax.cla()
	motorsab.cla()
	motorscd.cla()
	motorsef.cla()

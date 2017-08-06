#! /usr/bin/env python3

import curses
import time
import dsucommon
#format strings:
from dsucommon import ff
from dsucommon import uff

def ohandler(window, con):
	while(True):
		time.sleep(0.1)
		if(con is not None):
			i=0
			window.addstr(0,0, "STATUSMONITOR")
			window.addstr(0, 15, "Connected")
			con.askForData()
			rawdata = con.getData()
			parsed = dsucommon.parseData(rawdata)
			window.addstr(i+2, 1, "FPS: " +ff.format(parsed['fps']))
			if parsed['emergency']:
				window.addstr(i+1, 18, "!!!EMERGENCY!!!")
			window.addstr(i+3,1, "Remote: " +
				" Pitch: " + ff.format(parsed['Pitch']) +
				" Yaw: " + ff.format(parsed['Yaw']) +
				" RollX: " + ff.format(parsed['Rollx']) +
				" RollY: " + ff.format(parsed['Rolly'])
				)
			window.addstr(i+4, 1, "Motors: "+
				" a: " + uff.format(parsed['motorA']) +
				" b: " + uff.format(parsed['motorB']) +
				" c: " + uff.format(parsed['motorC']) +
				" d: " + uff.format(parsed['motorD']) +
				" e: " + uff.format(parsed['motorE']) +
				" f: " + uff.format(parsed['motorF'])
				)
			i+=6
			window.addstr(i,0, "### Sensor Values #"+'#'*35)
			window.addstr(i+1, 1, "Temperature [°C]: "+str(parsed['temp']))
			window.addstr(i+2,1, "Gravity: "+
				" X: "+ ff.format(parsed['grX'])+
				" Y: "+ ff.format(parsed['grY'])+
				" Z: "+ ff.format(parsed['grZ'])
				)
			window.addstr(i+3,1, "Rotation: "+
				" X: "+ ff.format(parsed['roX'])+
				" Y: "+ ff.format(parsed['roY'])+
				" Z: "+ ff.format(parsed['roZ'])
				)
			window.addstr(i+4,1, "Euler: "+
				" Heading: " + ff.format(parsed['euHeading'])+
				" Roll: " + ff.format(parsed['euRoll'])+
				" Pitch: " + ff.format(parsed['euPitch'])
				)
			window.addstr(i+5,1, "LinAcc: "+
				" X: " + ff.format(parsed['laX'])+
				" Y: " + ff.format(parsed['laY'])+
				" Z: " + ff.format(parsed['laZ'])
				)
			window.addstr(i+6,1, "Compass "+
				" X: " + str(parsed['maX']) +
				" Y: " + str(parsed['maY']) +
				" Z: " + str(parsed['maZ'])
				)
			window.addstr(i+7, 0, '#'*40)
			i+=9
			window.addstr(i, 1, "Controller: "+
				#" YPitch: "+ ff.format(parsed['YPitch']) +
				" YYaw: "+ ff.format(parsed['YYaw']) +
				" YRollX: "+ ff.format(parsed['YRollX']) +
				" YRollY: "+ ff.format(parsed['YRollY'])
				)
			window.addstr(i+1, 1, "Controller Integral: "+
				#" IntPitch: "+ ff.format(parsed['IntPitch']) +
				" IntYaw: "+ ff.format(parsed['IntYaw']) +
				" IntRollX: "+ ff.format(parsed['IntRollX']) +
				" IntRollY: "+ ff.format(parsed['IntRollY'])
				)
		else:
			window.addstr(0, 13, "Not connected")
		window.refresh()


con = dsucommon.Connector()
if(not con.connect(12000)):
	con = None
curses.wrapper(ohandler, con)


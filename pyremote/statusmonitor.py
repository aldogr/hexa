#! /usr/bin/env python3

import curses
import signal
from sys import exit
from time import sleep
from datetime import datetime
import dsucommon
#format strings:
from dsucommon import ff
from dsucommon import uff

# Capture 'Crtl+C' (SIGINT)
def SIGINT_handler(signal, frame):
    curses.reset_shell_mode()
    curses.endwin()
    exit(0)

signal.signal(signal.SIGINT, SIGINT_handler)

# Get the controler of the socket
con = dsucommon.Connector()
if(not con.connect(12000)):
	con = None

# Initialize curses
window = curses.initscr()

# By default no logging
logging = False

# Time step. By default 0.01 seconds.
step = 0.01

# Start curses application
while(True):
    # Capture key presses
    try:
        c = window.getch()
        if c == ord('q')
            curses.reset_shell_mode()
            curses.endwin()
            exit(0)
        elif c == ord('l'):
            if logging:
                logging = False
                logfile.close()
            else:
                logging = True
                log_time = 0
                dt = datetime.now()
                logfile = open(str(dt.month)+'_'+str(dt.day)+'_'+str(dt.hour)+'_'+str(dt.minute)+'.txt', 'a')
    except(curses.error):
        pass
    # Process each 'step' second
    time.sleep(step)
    # If the socket is opened
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
        i+=5
        # Logging control
        if logging:
            log_time += step
            window.addstr(i, 1, "Press 'l' to stop saving data.")
            window.addstr(i+1, 1, "Logging for " + log_time + " second(s)")
            logfile.write(str(log_time))
            params  = ["roX", "roY", "roZ", "euHeading", "euRoll", "euPitch"]
            for p in params:
                logfile.write("\t" + str(parsed[p]))
            logfile.write("\n")
        else:
            window.addstr(i, 1, "Press 'l' to start saving data.")
            window.addstr(i+1, 1, "Logging deactivated")
        i+=4
        window.addstr(i, 1, "Press 'q' or 'Crtl+C' to exit.")
    # If the socket is closed
    else:
        window.addstr(0, 13, "Not connected")
	window.refresh()

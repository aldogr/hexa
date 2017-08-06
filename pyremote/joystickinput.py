#! /usr/bin/env python3

import curses
import time
import pygame
import dsucommon
from dsucommon import ff


def ohandler(window, js, con):
	emergency = False
	while(True):
		i=4
		pygame.event.pump()
		
		for event in pygame.event.get():
			if event.type == pygame.JOYBUTTONDOWN:
				if js.get_button(0) and not emergency:
					emergency = True
				if js.get_button(0) and emergency:
					emergency = False
		
		time.sleep(0.02)
		window.addstr(0,0, "JOYSTICKINPUT")
		if(js is not None):
			window.addstr(1, 0, "Device: "+js.get_name()+
					" #Axes: "+ str(js.get_numaxes())+
				" #Balls: " + str(js.get_numballs())+
				" #Buttons: " + str(js.get_numbuttons())+
				" #Hats: " + str(js.get_numhats())
			)
			window.addstr(2, 1, "Axes: ")
			j=0
			axis = list()
			while(j<js.get_numaxes()):
				axis.append(js.get_axis(j))
				window.addstr(2, 7+j*13, str(j) + ": " + ff.format(axis[j]))
				j+=1
			axis.append(emergency)
			if emergency:
				window.addstr(i+2,1, "EMERGENCY_STOP")
		else:
			window.addstr(0, 0, "NO JOYSTICK DETECTED")
		window.hline(i+3, 0, '_', 50)

		if(con is not None):
			dat = dsucommon.packJs(axis)
			window.addstr(0, 15, "Streaming data...")
			#print("Pushing "+str(dat))
			con.pushData(dat)
		else:
			window.addstr(0, 15, "Not connected")
		window.refresh()

pygame.display.init()
pygame.joystick.init()
if(pygame.joystick.get_count() < 1):
	js = None
else:
	js = pygame.joystick.Joystick(0)
	js.init()

con = dsucommon.Connector()
if(not con.connect(12000)):
	con = None
curses.wrapper(ohandler, js, con)
pygame.joystick.quit()

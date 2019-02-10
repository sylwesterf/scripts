#!/bin/python
#pip install RPi.GPIO
import RPi.GPIO as GPIO
import time

channel = 21

# setup GPIO
GPIO.setmode(GPIO.BCM)
GPIO.setwarnings(False)
GPIO.setup(channel,GPIO.IN)

# output gas level high/low
while True:
	if(GPIO.input(channel)==0):
		print "low"
	else:
		print "high"
	
	time.sleep(5)

#GPIO.cleanup()
#This will return either 0 / GPIO.LOW / False or 1 / GPIO.HIGH / True

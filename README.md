## Dynamic Software Update on a Multicopter using Erlang

This software consists of several components.

* [An Autopilot written in Erlang for a hexacopter drone intended to run on a BeagleBone Black](multicopter/)
* [A visualization written in Python3](visu/)
* [A remote control written in Python3](pyremote/)
* [Some scripts, mainly to set up PWM on the BeagleBone](scripts/)
* [Some (performance) tests](tests/)
* [Some general documentation, see also EDoc of the multicopter app](doc/)

### Setting up the Network

1. Reset TP-Link Router
1. Connect to it using over WiFi using the password on the back
1. Open up http://192.168.0.1
1. Navigate to "DHCP", delete IP address of "Default Gateway"
1. Go to tab "Working Mode" choose "Access Point". Device will reboot.
1. Connect BBB, go to http://192.168.0.254 (that's the IP of the AP now), go to tab DHCP->Client list, find out IP and MAC address of BBB.
1. Again mage shure that "DHCP"-> "Default Gateway" is blank or 0.0.0.0
1. Add a static DHCP mapping, so it sticks to that IP.
1. Connect to BBB over ssh

### Setting up the Beaglebone

1. Boot up a fresh Debian IoT (headless) for Beaglebone, set up network (as above).
1. Log in via SSH or console using credentials *debian:temppwd*
1. ```mkdir DSU.git```
1. ```cd DSU.git```
1. ```git init --bare```
1. On your development machine issue: ```git remote add beagle debian@[IP/HOSTNAME]:~/DSU.git```
1. ...and git push beagle
1. Back on the beaglebone do: ```cd ~``` and ```git clone ~/DSU.git```
1. Get fresh package list: ```sudo apt-get update``` (requires internet)
1. Install Erlang: ```sudo apt-get install erlang``` 
1. Install rebar3: ```git clone https://github.com/erlang/rebar3.git``` then ```cd rebar3``` and issue ```./bootstrap```
1. Put rebar3 in the ```$PATH```: Add following line to ```~/.bashrc```: ```PATH=$PATH:/home/debian/rebar3```

### Setting up time (needed for TLS-Connections)

* ```sudo tlsdate --skip-verification -p 80 -H rgnx.net```

### Setting up WiFi (not entirely sure if this is reproducible, as I fiddled around quite a bit)

1. Plugin the USB WiFi adapter, maybe use extension cord, as WiFi allegedly get's disturbed by the HDMI signal on the BBB
1. Issue ```iwconfig```, interface wlan0 should appear
1. In /etc/network/interfaces add ```
allow-hotplug wlan0
iface wlan0 inet manual
    wpa-roam /etc/wpa_supplicant/wpa_supplicant.conf
iface default inet dhcp```
1. Create /etc/wpa_supplican/wpa_supplicant.conf and write the following ```
ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=netdev
update_config=1
network={
 ssid="TP-LINK_220E"
 scan_ssid=1
 psk="50759484"
 proto=RSN
 key_mgmt=WPA-PSK
 pairwise=CCMP
 auth_alg=OPEN
}```
1. ```sudo apt-get install firmware-realtek```
1. ```reboot```
1. Add ```rtl8192cu``` to /etc/modules
1. ```sudo ifup wlan0``` if it fails, do
1. ```sudo service start wpa_supplicant```
1. ```sudo service stop wpa_supplicant```
1. ```sudo ifup wlan0```

### Setting up sshuttle for internet access

1. Download sshuttle from [here](https://pypi.python.org/pypi/sshuttle)
1. scp sshuttle
1. ```sudo pip install --no-binary :all: sshuttle-*.tar.gz```
1. Make sure BBB has access to an	 OpenSSH server (e.g. your laptop), which is connected to the internet.
1. ```sudo sshuttle --dns -vr username@192.168.0.??? 0/0 -x 192.168.0.0/24```
1. You should now have internet access on the BBB.

### Set up Pins, Multiplexing and PWM

* Run ```sudo scripts/setupPinMultiplexing.sh```
* Run ```sudo scripts/setupPWM.sh```

## PIN-Configuration

Output		| Pin		| Motor/Signal
-----		|-----		|-----
epwm0A		| P9_22 	| a
epwm0B		| P9_21		| b
epwm1A		| P9_14		| c
epwm1B		| P9_16		| d
epwm2A		| P8_19		| e
epwm2B		| P8_13		| f
GND			| P9_0,1	| Common Ground
5V			| P9_4,5	| Sensor_Vin
I2C2_SDA	| P9_19		| I2C Signal
I2C2_SDC	| P9_20		| I2C Clock

## Axes of the IMU

![IMU Sensor](https://cdn-learn.adafruit.com/assets/assets/000/024/585/medium800/sensors_2472_top_ORIG.jpg?1429638074)

Chip in upright position, writing BNO055 must be readable upright

* positive z-Axis: coming out of the chip
* positive x-Axis: parallel to the long edge, showing upwards
* positive y-Axis: on the short edge

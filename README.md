# OCC-GPU_Thermals

This repository contains scripts similar to those in [OCC-CPU_Thermals][https://github.com/GuestJim/OCC-CPU_Thermals] but for testing GPUs instead of CPUs.
Like the CPU scripts, this will initiate the recording of certain metrics (GPU temperature, clock speed, VRAM clock speed, and power usage), then wait for a Warm-up Period before beginning some load to stress the GPU.
After this load completes, a Cooldown Period of the same length as the test period, one hour by default, during which monitoring continues so we can see how the graphics card cools down.

These scripts are currently only configured to work with 3DMark Professional Edition, and so their usefulness is going to be limited to those with a license.
In theory any GPU load can be used, provided it can be started, stopped, and looped all by command line interface (CLI).

### Software Involved:

- [Python](https://www.python.org/)
- [R](https://www.r-project.org)
- R libraries:
	- [readr](https://readr.tidyverse.org/)
	- [ggplot2](https://ggplot2.tidyverse.org/index.html)
	- [tidyr](https://tidyr.tidyverse.org/)
- 3DMark Professional Edition
- [GPU-z](https://www.techpowerup.com/gpuz/)

### To Use:

Using these scripts I hope is simple.
First you want to place these scripts all in whatever folder you wish.
In the same folder you should place shortcuts to the appropriate EXEs, which in this case would be GPU-z.exe and 3DMarkCmd.exe.
These two shortcuts should be named "GPU-z" and "3DMarkCmd" as the script looks for those names, but it is possible to edit the script for other names.
You also need GPU-z to be configured so it saves a log file (which it will start upon opening) and that it saves the file to the folder with these scripts.
This log file should be named "GPU-Z Sensor Log.txt"

Once that configuration work is ready, run the **GPU Thermal - 3dmdef.py** script.
This script manages the starting and stopping of everything, and can create a custom 3DMark test definition file.
3DMark has two ways to loop its tests.
One method, easily achieved by the CLI, will repeat a test as long as you wish or indefinitely, but will need to reload the test between each run.
The other method will seamlessly loop the test but requires working with a custom test definition file, which has been configured to loop so many times.
This script will create the test definition file with the appropriate number of loops configured and place it in the folder where it will place the recorded data.

The **GPU Thermal - 3dmdef.py** script will ask for the name of the GPU (example, RX Vega 64), the cooler (example, Gigabyte Gaming OC), the Zero RPM threshold, and the power limit for the card.
The last three of these can be left blank if you wish or do not know the values. 
After asking for that information, a list is presented of the different tests that can be run, including versions that do not seamlessly loop and those that do.
The duration of the load period (which will also be the length of the Cooldown Period) and Warm-up period can then be set, with te defaults being one hour (3600 seconds) and five minutes (300 seconds).

Before starting, the script checks if the "GPU-Z Sensor Log.txt" file already exists and asks about deleting it, with that being the default action.
It is necessary the file be freshly created for each run.

The script will then handle the rest, running all of the test, followed by placing the data in the appropriate folder and creating configured versions of the R scripts to work with it.
Running the **@GPU Thermal - Input.r** script should process the data and create graphs, a statistics TXT file, and HTML files containing summary statistics in tables for easy embedding online.

If the data is going to be processed on a different machine than the one the test ran on, it might be necessary to change the **setwd** command in the **@GPU Thermal - Input.r** script.
Executing the script as a script directly should work fine though, even on a different machine.
Changing the working directory is necessary when running the script in the R GUI.
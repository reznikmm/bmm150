# BMM150 LCD demo

This folder contains a demonstration program showcasing the functionality
of a 3D magnetometer sensor using the STM32 F4VE board
and an LCD display included in the kit. The program features a straightforward
graphical user interface (GUI) for configuring sensor parameters.

![Demo screenshot](bmm150_lcd_x2.png)

## Overview

The demonstration program is designed to work with the STM32 F4VE development
board and a compatible LCD display. It provides a GUI interface to configure
sensor parameters such as measurement frequency and Drive.
The display includes buttons for enabling/disabling the display of
measurement (`Fx`, `Fy`, `Fz`). Additionally, there are yellow buttons
(`LP`, `RE`, `EN`, `HA`) for selecting presets (low power, regular,
enhanced, high accuracy). Grey buttons (`02`, `06`, `08`, `10`, `15`,
`20`, `25`, `30`) control the output data rate (from 2 to 30 Hz).

## Requirements

* STM32 F4VE development board
* Any BMM150 module
* Compatible LCD display/touch panel included in the kit
* Development environment compatible with STM32F4 microcontrollers

## Setup

* Attach BMM150 by I2C to PB9 (SDA), PB8 (SCL)
* Attach the LCD display to the designated port on the STM32F4VE board.
* Connect the STM32 F4VE board to your development environment.

## Usage

Compile and upload the program to the STM32 F4VE board. Upon successful upload,
the demonstration program will run, displaying sensor data on the LCD screen.
Activate the buttons on the GUI interface using the touch panel.
Simply touch the corresponding button on the LCD screen to toggle its state.

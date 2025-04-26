# BMM150

[![Build status](https://github.com/reznikmm/bmm150/actions/workflows/alire.yml/badge.svg)](https://github.com/reznikmm/bmm150/actions/workflows/alire.yml)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bmm150.json)](https://alire.ada.dev/crates/bmm150.html)
[![REUSE status](https://api.reuse.software/badge/github.com/reznikmm/bmm150)](https://api.reuse.software/info/github.com/reznikmm/bmm150)

> Driver for BMM150 magnetic sensor.

- [Datasheet](https://www.bosch-sensortec.com/products/motion-sensors/magnetometers/bmm150/)

The sensor is available as a module for DIY projects from various
manufacturers, such as
[SEN0529 by DFRobot](https://wiki.dfrobot.com/SKU_SEN0529_Gravity_BMM150_Triple_Axis_Magnetometer),
[WareShare](https://www.waveshare.com/wiki/BMM150_3-Axis_Magnetometer_Sensor) or
[CJCMU-150](https://www.aliexpress.com/item/1005004432455077.html).
It boasts 0.3μT resolution, low power consumption, a compact size, etc.

The BMM150 driver enables the following functionalities:

- Detect the presence of the sensor.
- Perform soft reset
- Configure the sensor (power mode, output data rate, repetitions per
  measurement)
- Conduct measurements as raw 13-bit (15 for Z) values and scaled values
  with temperature compensation.

## Install

Add `bmm150` as a dependency to your crate with Alire:

    alr with bmm150

## Usage

The driver implements two usage models: the generic package, which is more
convenient when dealing with a single sensor, and the tagged type, which
allows easy creation of objects for any number of sensors and uniform handling.

Generic instantiation looks like this:

```ada
declare
   package BMM150_I2C is new BMM150.Sensor
     (I2C_Port    => STM32.Device.I2C_1'Access,
      I2C_Address => 16#13#);

begin
   --  Power BMM150 on
   BMM150_I2C.Suspend_Off (Ravenscar_Time.Delays, Ok);

   if BMM150_I2C.Check_Chip_Id then
      ...
```

While declaring object of the tagged type looks like this:

```ada
declare
   Sensor : BMM150.Sensors.BMM150_Sensor
     (I2C_Port => STM32.Device.I2C_1'Access,
      I2C_Address => 16#13#);
begin
   --  Power BMM150 on
   Sensor.Suspend_Off (Ravenscar_Time.Delays, Ok);

   if Sensor.Check_Chip_Id then
      ...
```

### Sensor Configuration

To configure the sensor, use the `Set_Power_Mode` and `Set_Repetitions`
procedures.

Procedure `Set_Power_Mode` accepts `Mode` and `ODR` parameters:

- `Mode`: power mode (`Normal`, `Forced`, `Sleep`)
- `ODR`: Output Data Rate for `Normal` mode. Possible values 2, 6, 8,
   10, 15, 20, 25, 30 Hz.

An example:
```ada
Sensor.Set_Power_Mode
  (Mode    => BMM150.Normal,
   ODR     => 20,
   Success => Ok);
```

Procedure `Set_Repetitions` accepts repetition counts for X/Y and Z axis.
For presets are defined in `BMM150` for convenience' sake, but any value
can be used.
- `Low_Power_Preset`
- `Regular_Preset`
- `Enhanced_Regular_Preset`
- `High_Accuracy_Preset`

An example for `Regular_Preset` (X_Y => 9, Z => 15):
```ada
Sensor.Set_Repetitions
  (BMM150.Regular_Preset,
   Success => Ok);
```

### Read Measurement

The best way to determine data readiness is through interrupts using
a separate pin. Otherwise you can ascertain that the data is ready by
waiting while `Measuring` returns `False`.

Read raw data (as provided by the sensor) with the `Read_Raw_Measurement`
procedure.

Calling `Read_Measurement` returns scaled measurements in Gauss based on
the current `Full_Range` setting.

### Low-Level Interface: `BMM150.Raw`

The `BMM150.Raw` package provides a low-level interface for interacting with
the BMM150 sensor. This package is designed to handle encoding and decoding
of sensor register values, while allowing users to implement the actual
read/write operations in a way that suits their hardware setup. The
communication with the sensor is done by reading or writing one or more bytes
to predefined registers. This package does not depend on HAL and can be used
with DMA or any other method of interacting with the sensor.

#### Purpose of BMM150.Raw

The package defines array subtypes where the index represents the register
number, and the value corresponds to the register's data. Functions in this
package help prepare and interpret the register values. For example, functions
prefixed with `Set_` create the values for writing to registers, while those
prefixed with `Get_` decode the values read from registers. Additionally,
functions starting with `Is_` handle boolean logic values, such as checking
if the sensor is measuring or updating.

Users are responsible for implementing the reading and writing of these
register values to the sensor.

#### SPI and I2C Functions

The package also provides helper functions for handling SPI and I2C
communication with the sensor. For write operations, the register
address is sent first, followed by one or more data bytes, as the
sensor allows multi-byte writes. For read operations, the register
address is sent first, and then consecutive data can be read without
needing to specify the address for each subsequent byte.

- Two functions convert register address to byte:

  ```ada
  function SPI_Write (X : Register_Address) return Byte;
  function SPI_Read (X : Register_Address) return Byte;
  ```

- Other functions prefix a byte array with the register address:

  ```ada
    function SPI_Write (X : Byte_Array) return Byte_Array;
    function SPI_Read (X : Byte_Array) return Byte_Array
    function I2C_Write (X : Byte_Array) return Byte_Array;
    function I2C_Read (X : Byte_Array) return Byte_Array;
  ```

These functions help abstract the specifics of SPI and I2C communication,
making it easier to focus on the sensor’s register interactions without
worrying about protocol details. For example, you configure the sensor
with low power preset:

```ada
declare
   Data : Byte_Array := BMM150.Raw.SPI_Write
    (BMM150.Raw.Set_Repetitions
      (Preset => BMM150.Low_Power_Preset));
begin
   --  Now write Data to the sensor by SPI
```

The reading looks like this:

```ada
declare
   Data   : Byte_Array := BMM150.Raw.SPI_Read
    ((BMM150.Raw.Measurement_Data => 0));
   Result : BMM150.Magnetic_Field_Vector;
begin
   --  Start SPI exchange (read/write) then decode Data:
   Result := BMM150.Raw.Get_Measurement (Data, Trim);
```

## Examples

Examples use `Ada_Drivers_Library`. It's installed by Alire (alr >= 2.1.0 required).
Run Alire to build:

    alr -C examples build

### GNAT Studio

Launch GNAT Studio with Alire:

    alr -C examples exec gnatstudio -- -P bmm150_put/bmm150_put.gpr

### VS Code

Make sure `alr` in the `PATH`.
Open the `examples` folder in VS Code. Use pre-configured tasks to build
projects and flash (openocd or st-util). Install Cortex Debug extension
to launch pre-configured debugger targets.

- [Simple example for STM32 F4VE board](examples/bmm150_put) - complete
  example for the generic instantiation.
- [Advanced example for STM32 F4VE board and LCD & touch panel](examples/bmm150_lcd) -
  complete example of the tagged type usage.
